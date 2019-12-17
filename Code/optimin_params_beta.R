k <- 1
x <- seq(from = 0, to = 0.0045, by = 0.000001)
emp_cdf <- ecdf(sample[,,k])

func <- function(params){
  alpha <- params[1]
  beta <- params[2]
  
  return( max( abs(emp_cdf(x) - pbeta(x, alpha, beta) ) ) )

}

optim(c(10, 10), func)
