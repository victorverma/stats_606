# Load packages -----------------------------------------------------------

library(numDeriv)
library(sparklyr)
library(tidyverse)

# Define functions --------------------------------------------------------

calc_grad <- function(f) {
  force(f)
  function(x) grad(f, x)
}

make_main_tbl <- function(f_list,
                          grad_list = NULL,
                          init_xs,
                          init_step_size,
                          weight_mat) {
  if (is.null(grad_list)) {
    grad_list <- map(f_list, calc_grad)
  }
  tibble(
    f = f_list,
    grad = grad_list,
    weights = apply(weight_mat, 1, identity, simplify = FALSE),
    curr_x = init_xs,
    curr_step_size = init_step_size
  )
}

calc_next_x <- function(id_tbl, context) {
  for (nm in names(context)) {
    assign(nm, context[[nm]], envir = .GlobalEnv) 
  }
  
  id <- id_tbl$id
  weights <- main_tbl$weights[[id]]
  curr_xs <- main_tbl$curr_x
  curr_x <- curr_xs[[id]]
  curr_step_size <- main_tbl$curr_step_size[id]
  grad_ <- main_tbl$grad[[id]]
  curr_weighted_sum <- Reduce(
    `+`, mapply(`*`, weights, curr_xs, SIMPLIFY = FALSE)
  )
  next_x <- curr_weighted_sum - curr_step_size * grad_(curr_x)
  
  l <- as.list(next_x)
  names(l) <- paste0("x", seq_along(next_x))
  as.data.frame(l)
}

perform_dgd_update <- function(main_tbl) {
  p <- main_tbl %>% pull(curr_x) %>% first() %>% length()
  columns <- c("integer", rep("double", p)) %>%
    set_names(c("id", str_c("x", seq_len(p))))
  context <- list(
    grad = grad,
    grad.default = numDeriv:::grad.default,
    main_tbl = main_tbl
  )
  next_xs <- sdf_len(sc, nrow(main_tbl)) %>%
    spark_apply(
      calc_next_x, columns = columns, group_by = "id", context = context
    ) %>%
    collect() %>%
    select(-id) %>%
    pmap(c) %>%
    map(unname)
  mutate(main_tbl, curr_x = next_xs)
}