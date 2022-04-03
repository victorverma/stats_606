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