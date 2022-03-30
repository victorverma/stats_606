library(sparklyr)
library(dplyr)

sc <- spark_connect(master = "local")

g_1 <- function(x){
  return(c(2*x[1], 0))
}

g_2 <- function(x){
  return(c(0, 2*x[2]))
}

g <- function(x){
  func = unique(x$func)
  for (i in 1:2) {
    if (func == as.character(i)) {
      return(get(paste0("g_", i))(x$curr_x))
    }
  }
}

W = matrix(c(0.8,0.2,0.2,0.8), nrow = 2, ncol = 2)
W_1 = matrix(c(0.8, 0, 0, 0.2, 0.2, 0, 0, 0.8), 
             ncol = 4, nrow = 2)

x_0 <- list(c(30,30), c(-30, -20))
k = length(x_0)

data = data.frame(curr_x = x_0[[1]])
data = data  %>% mutate(func = '1')

for (i in 2:k) {
  data_aux = data.frame(curr_x = x_0[[i]])
  data_aux = data_aux  %>% mutate(func = as.character(i))
  data = rbind(data, data_aux)
}

data = data  %>% mutate(f = func)

data_spark <- copy_to(sc, data, overwrite = TRUE)

w_sum = W_1 %*% data$curr_x
eta = 0.1

context_list = list(g_1=g_1, g_2=g_2, g=g, w_sum = w_sum, eta = eta)

update_x <- function(x, context_list){
  
  return(x %>% spark_apply(function(df, context) {
    for (name in names(context)) assign(name, context[[name]], envir = .GlobalEnv)
    w_sum - g(df)
  }, context = context_list, group_by = "f"))
  
}

update_x(data_spark, context_list)
