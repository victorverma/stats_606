#install.packages("devtools")
#devtools::install_github("bosafoagyare/DistGD")
library(ggplot2)
library(devtools)
library(sparklyr)
library(numDeriv)
library(tidyverse)

N <- 100000
X <- rnorm(n = N, 0, 1)
epsilon <- rnorm(n = N, 0, 1)
X <- cbind(rep(1, N),X)

beta <- c(-3,4)

Y = X%*%beta + epsilon

N_1 = round(N / 3)
N_2 = round(N * 2 / 3)

X_1 = X[1:N_1,]
X_2 = X[(N_1 + 1):N_2,]
X_3 = X[(N_2 + 1):N,]

Y_1 = Y[1:N_1]
Y_2 = Y[(N_1 + 1):N_2]
Y_3 = Y[(N_2 + 1):N]

regress_loss <- function(beta_hat){
  return(sum((Y - X%*%beta_hat)^2))
}

regress_loss_1 <- function(beta_hat){
  return(sum((Y_1 - X_1%*%beta_hat)^2))
}

environment(regress_loss_1) <- list2env(x = list(X_1 = X_1, Y_1 = Y_1))

regress_loss_2 <- function(beta_hat){
  return(sum((Y_2 - X_2%*%beta_hat)^2))
}

environment(regress_loss_2) <- list2env(x = list(X_2 = X_2, Y_2 = Y_2))

regress_loss_3 <- function(beta_hat){
  return(sum((Y_3 - X_3%*%beta_hat)^2))
}

environment(regress_loss_3) <- list2env(x = list(X_3 = X_3, Y_3 = Y_3))

sc <- spark_connect(master = "local")

trace <- DistGD::dgd(
  sc,
  f_list = list(regress_loss_1, 
                regress_loss_2,
                regress_loss_3),
  grad_list = NULL,
  init_xs = list(c(-4,0), c(-1,-1), c(0,5)),
  init_step_size = 0.00001,
  weight_mat = rbind(c(1/3, 1/3, 1/3), 
                     c(1/3, 1/3, 1/3), 
                     c(1/3, 1/3, 1/3)),
  num_iters = 10,
  print = TRUE,
  make_trace = TRUE
)

grid <- expand.grid(beta_0 = seq(from = -7, to = 1, length.out = 10),
                    beta_1 = seq(from = -1, to = 7, length.out = 10))
z <- c()
for (i in 1:dim(grid)[1]) {
  z[i] <- regress_loss(c(grid[i,1], grid[i,2]))
}
grid['loss'] <- z

optimal <- lm.fit(X, Y)$coefficients
optimal <- data.frame(beta_0=optimal[1], beta_1=optimal[2])

iterations_1 <- trace %>%
  relocate(f_id) %>%
  arrange(f_id, iter_num) %>%
  mutate(
    f_id = as_factor(f_id),
    curr_x_1 = map_dbl(curr_x, 1),
    curr_x_2 = map_dbl(curr_x, 2)
  ) %>%
  select(!curr_x)

ggplot() +
  geom_point(data=iterations_1, aes(curr_x_1, curr_x_2, color = iter_num)) +
  facet_wrap(vars(f_id)) +
  geom_path(data=iterations_1, aes(curr_x_1, curr_x_2, color = iter_num)) +
  geom_contour(data=grid, aes(beta_0, beta_1, z = loss), size=0.2) + 
  geom_point(data=optimal, aes(beta_0, beta_1)) + 
  theme_bw()
