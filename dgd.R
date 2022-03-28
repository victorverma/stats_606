library(numDeriv)
library(sparklyr)
library(tidyverse)

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
    curr_x = init_xs,
    curr_step_size = init_step_size,
    weights = apply(weight_mat, 1, identity, simplify = FALSE)
  )
}