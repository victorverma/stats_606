library(numDeriv)
library(sparklyr)
library(tidyverse)

calc_grad <- function(f) {
  force(f)
  function(x) grad(f, x)
}