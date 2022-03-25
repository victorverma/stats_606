library(sparklyr)
library(dplyr)

sc <- spark_connect(master = "local")

x_0 <- list(c(30,30), c(-30, -20))

g_1 <- function(x){
  return(c(2*x[1], 0))
}

g_2 <- function(x){
  return(c(0, 2*x[2]))
}

gradiente_list = list(g_1 = g_1, 
                      g_2 = g_2)

for (name in names(gradiente_list)) assign(name, gradiente_list[[name]], envir = .GlobalEnv)

distr_grad_desc <- function(gradiente_list, x_0){
  
  k = length(gradiente_list)
  data = data.frame(x_0 = x_0[[1]])
  data = data  %>% mutate(func = '1')
  
  for (i in 2:k) {
    data_aux = data.frame(x_0 = x_0[[i]])
    data_aux = data_aux  %>% mutate(func = as.character(i))
    data = rbind(data, data_aux)
  }
  
  data_spark <- copy_to(sc, data, overwrite = TRUE)
  data_spark <- data_spark %>% mutate(f = func)
  
  g <- function(x){
    func = unique(x$func)
    for (i in 1:2) {
      if (func == as.character(i)) {
        return(gradiente_list[[i]](x$x_0))
      }
    }
  }
  
  g_list = append(gradiente_list, list(g = g))

  return(data_spark %>% spark_apply(function(df, context) {
      for (name in names(context)) assign(name, context[[name]], envir = .GlobalEnv)
      g(df)
    }, context = g_list, group_by = "func"))
  
}

distr_grad_desc(gradiente_list,
                x_0)
