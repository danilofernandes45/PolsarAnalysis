k <- 5
x <- seq(from = 0, to = 1, by = 0.000001)
emp_cdf <- ecdf(sample[,,k])

func <- function(params){
  alpha <- params[1]
  beta <- params[2]
  
  return( max( abs(emp_cdf(x) - pbeta(x, alpha, beta) ) ) )

}

mean <- mean(sample[,,k])
var <- sd(sample[,,k]) ^ 2
alpha <- mean * ( mean * (1 - mean) / var - 1 )
beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)

op <- optim(c(1, 10), func)
alpha <- op$par[1]
beta <- op$par[2]

#=================================================

library("GA")
GA <- ga(type = "real-valued", fitness = function(x) - func(x), lower = c(0.01, 0.01), upper = c(10, 100), popSize = 300, maxiter = 400)

#=================================================

x <- seq(from = 0, to = 1, by = 0.0001)

ggplot() +
  geom_line(aes(x = x, y = pbeta(x, alpha, beta), colour = "red")) + ylab("probability") +
  geom_line(aes(x = x, y = emp_cdf(x) ))
