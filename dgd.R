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
    weights = apply(weight_mat, 1, identity, simplify = FALSE), # List of rows
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
  curr_weighted_sum <- Reduce( # Sum weighted x_i's
    `+`,
    mapply(`*`, weights, curr_xs, SIMPLIFY = FALSE) # Mult x_i by weight i
  )
  next_x <- curr_weighted_sum - curr_step_size * grad_(curr_x)
  
  l <- as.list(next_x) # l's entries are individual coordinates of next_x
  names(l) <- paste0("x", seq_along(next_x))
  as.data.frame(l) # 1-row data frame, next_x coordinates in different cols
  # Function given to spark_apply must return data frame
}

perform_dgd_update <- function(main_tbl) {
  p <- main_tbl %>% pull(curr_x) %>% first() %>% length() # x lives in R^p
  columns <- c("integer", rep("double", p)) %>%
    set_names(c("id", str_c("x", seq_len(p)))) # Give output col types for speed
  context <- list( # calc_next_x uses these, so must give to cluster
    grad = grad,
    grad.default = numDeriv:::grad.default, # Dispatched by grad
    main_tbl = main_tbl
  )
  next_xs <- sdf_len(sc, nrow(main_tbl)) %>% # Spark data frame with one col, id
    spark_apply(
      calc_next_x, columns = columns, group_by = "id", context = context
    ) %>% # Gives Spark data frame w/ 1 row per func, id col, p coordinate cols
    collect() %>% # Turn Spark data frame into a plain tibble
    select(-id) %>%
    pmap(c) %>% # Transform tibble with coordinate cols into list of vectors
    map(unname)
  mutate(main_tbl, curr_x = next_xs)
}

run_dgd <- function(sc,
                    f_list,
                    grad_list = NULL,
                    init_xs,
                    init_step_size,
                    weight_mat,
                    num_iters,
                    print = FALSE,
                    make_trace = FALSE) {
  main_tbl <- make_main_tbl(
    f_list, grad_list, init_xs, init_step_size, weight_mat
  )
  
  if (print) {
    cat("Iter 0\n")
    main_tbl %>% pull(curr_x) %>% print()      
  }
  if (make_trace) {
    trace <- vector("list", num_iters + 1)
    trace[[1]] <- pull(main_tbl, curr_x)
  }
  for (iter in seq_len(num_iters)) {
    main_tbl <- perform_dgd_update(main_tbl)
    if (print) {
      cat(str_interp("Iter ${iter}\n"))
      main_tbl %>% pull(curr_x) %>% print()      
    }
    if (make_trace) {
      trace[[iter + 1]] <- pull(main_tbl, curr_x)
    }
  }
  
  if (make_trace) {
    trace %>%
      set_names(seq_along(.) - 1) %>%
      map(enframe, name = "f_id", value = "curr_x") %>%
      bind_rows(.id = "iter_num") %>%
      mutate(iter_num = as.integer(iter_num))
  } else {
    main_tbl$curr_x  
  }
}

# Test the code -----------------------------------------------------------

sc <- spark_connect(master = "local")

trace <- run_dgd(
  sc,
  f_list = list(
    function(x) sum(x^2) + 1,
    function(x) sum(x^2) + 2,
    function(x) sum(x^2) + 3
  ),
  grad_list = NULL,
  init_xs = list(c(1, 1), c(1, -1), c(-1, -1)),
  init_step_size = 0.1,
  weight_mat = rbind(rep(1 / 3, 3), c(0.25, 0.5, 0.25), c(0.5, 0.3, 0.2)),
  num_iters = 100,
  print = TRUE,
  make_trace = TRUE
)