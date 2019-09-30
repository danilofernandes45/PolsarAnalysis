getDistDihedral <- function(dim){
  
  hhhh <- read_data("HHHH", dim)
  hvhv <- read_data("HVHV", dim)
  vvvv <- read_data("VVVV", dim)
  
  hhhv <- read_complex_data("HHHV", dim)
  hhvv <- read_complex_data("HHVV", dim)
  hvvv <- read_complex_data("HVVV", dim)
  
  prod_kennaugh <- c( hhhh^2 + hvhv^2 + vvvv^2 + 2*Mod(hhhv)^2 + 2*Mod(hhvv)^2 + 2*Mod(hvvv)^2 )
  
  prod_di <- c( 2 * hvhv )
  prod_lh <- c( hvhv + vvvv - 2*Im(hvvv) ) 
  prod_rh <- c( hvhv + vvvv + 2*Im(hvvv) )
  
  numerator <- prod_di - 0.5*( prod_lh + prod_rh )
  denominator <- sqrt( 4*prod_kennaugh - prod_lh^2 - prod_rh^2 )

  return( (1/pi)*acos( numerator / denominator ) )
}

dim1 <- c(90, 65, 5, 30)

dim <- c(90, 15, 10, 15)

setwd(wd[1])
sample1 <- getDistDihedral(dim)
setwd(wd[5])
sample5 <- getDistDihedral(dim)

mean1 <- mean(sample1)
var1 <- sd(sample1) ^ 2
alpha1 <- mean1 * ( mean1 * (1 - mean1) / var1 - 1 )
beta1 <- ( 1 - mean1 ) * ( mean1 * ( 1 - mean1 ) / var1 - 1)

x <- seq( from = 0, to = 1, by = 0.001)
desc1 <- paste("Beta(", round(alpha1, 3), ", ", round(beta1, 3), ")", sep="")

mean5 <- mean(sample5)
var5 <- sd(sample5) ^ 2
alpha5 <- mean5 * ( mean5 * (1 - mean5) / var5 - 1 )
beta5 <- ( 1 - mean5 ) * ( mean5 * ( 1 - mean5 ) / var5 - 1)

desc5 <- paste("Beta(", round(alpha5, 3), ", ", round(beta5, 3), ")", sep="")

alpha1 <- 436
beta1 <- 456

alpha5 <- 70.5
beta5 <- 71.7

ks.test(sample1, "pbeta", shape1 = alpha1, shape2 = beta1) #436, 456
ks.test(sample5, "pbeta", shape1 = alpha5, shape2 = beta5) #70.5, 71.7

ggplot() + 
  geom_histogram(aes(x = c(sample1), y = ..density..), fill = "red", alpha = 0.35, bins = 45) + xlab("x") +
  geom_histogram(aes(x = c(sample5), y = ..density..), fill = "green", alpha = 0.35, bins = 45) +
  geom_line(aes(x = x, y = dbeta(x, alpha5, beta5), colour = "red"), size = 2) +
  geom_line(aes(x = x, y = dbeta(x, alpha1, beta1), colour = "green"), size = 2) + 
  scale_color_discrete(name = "Parameters", labels = c(desc1, desc5))

plot_ly() %>% add_lines(x = x, y = dbeta(x, alpha1, beta1)) %>% add_lines(x = x, y = dbeta(x, alpha5, beta5))

pbeta(0.462, shape1 = alpha1, shape2 = beta1) #0.0545683
1 - pbeta(0.512, shape1 = alpha1, shape2 = beta1) # 0.08276848

pbeta(0.512, shape1 = alpha5, shape2 = beta5) - pbeta(0.462, shape1 = alpha5, shape2 = beta5) #0.4400932
