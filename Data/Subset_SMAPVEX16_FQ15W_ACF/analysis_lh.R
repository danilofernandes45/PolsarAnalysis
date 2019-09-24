scatterer <- "left helix"

setwd(wd[1])
sample1 <- getGeoDist(scatterer, dim1)

setwd(wd[5])
sample2 <- getGeoDist(scatterer, dim1)

# #Estimative of Beta parameters
# mean <- mean(sample)
# var <- sd(sample) ^ 2
# alpha <- mean * ( mean * (1 - mean) / var - 1 )
# beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
# 
# #Plot
# x <- seq( from = range[1], to = range[2], by = 0.001)
# desc <- paste("Beta(", round(alpha, 3), ", ", round(beta, 3), ")", sep="")

ggplot() + 
  geom_histogram(aes(x = c(sample2), y = ..density..), fill = "green", alpha = 0.5, bins = 45) + xlab("x") +
  geom_histogram(aes(x = c(sample1), y = ..density..), fill = "red", alpha = 0.5, bins = 45)

ecdf(sample1)(0.9)
1 - ecdf(sample2)(0.9)

plotQQPlotBeta("left helix", dim1)
