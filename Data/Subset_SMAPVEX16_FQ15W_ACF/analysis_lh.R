#Usar para classificar: dihedral, right helix e left helix

dim1 <- c(90, 65, 5, 30)

#Proposed sample
dim3 <- c(90, 15, 10, 15)

scatterer <- "right helix"

setwd(wd[1])
sample1 <- getGeoDist(scatterer, dim3)

setwd(wd[5])
sample2 <- getGeoDist(scatterer, dim3)

# #Estimative of Beta parameters
# mean <- mean(sample)
# var <- sd(sample) ^ 2
# alpha <- mean * ( mean * (1 - mean) / var - 1 )
# beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
# 
# #Plot
x <- seq( from = 0, to = 1, by = 0.001)
# desc <- paste("Beta(", round(alpha, 3), ", ", round(beta, 3), ")", sep="")

ggplot() + 
  geom_histogram(aes(x = c(sample1), y = ..density..), fill = "red", alpha = 0.35, bins = 45) + xlab("x") +
  geom_histogram(aes(x = c(sample2), y = ..density..), fill = "green", alpha = 0.35, bins = 45) +
  geom_line(aes(x = x, y = dbeta(x, 21, 1.6), colour = "green"), size = 2) +
  geom_line(aes(x = x, y = dbeta(x, 10, 1.85), colour = "red"), size = 2) + xlab("x") +
  scale_color_discrete(name = "Parameters", labels = c("Beta(21, 1.6)", "Beta(10, 1.85)")) +
  theme_few() +
  theme(text = element_text(size=20))

ecdf(sample1)(0.9)
1 - ecdf(sample2)(0.9)

setwd(wd[5])
ksTestBeta("dihedral", dim3) #Beta(12.362, 2.05)
ksTestBeta("left helix", dim3) #Beta(10.369, 1.803)
ksTestBeta("right helix", dim3) #Beta(10.785, 1.931)

sample <- getGeoDist("dihedral", dim3)
ks.test(sample, "pbeta", shape1 = 12, shape2 = 2)

sample <- getGeoDist("left helix", dim3)
ks.test(sample, "pbeta", shape1 = 10, shape2 = 1.85)

sample <- getGeoDist("right helix", dim3)
ks.test(sample, "pbeta", shape1 = 10, shape2 = 1.85)


setwd(wd[1])
ksTestBeta("dihedral", dim3) #Beta(14.874, 1.528)
ksTestBeta("left helix", dim3) #Beta(21.125, 1.595)
ksTestBeta("right helix", dim3) #Beta(21.678, 1.583)

sample <- getGeoDist("dihedral", dim3)
ks.test(sample, "pbeta", shape1 = 15, shape2 = 1.5)

sample <- getGeoDist("left helix", dim3)
ks.test(sample, "pbeta", shape1 = 21.5, shape2 = 1.6)

sample <- getGeoDist("right helix", dim3)
ks.test(sample, "pbeta", shape1 = 21.5, shape2 = 1.6)

ggplot() + 
     geom_line(aes(x = x, y = dbeta(x, 12, 2), colour = "green"), size = 1.3) + xlab("x") +
     geom_line(aes(x = x, y = dbeta(x, 15, 1.5), colour = "red"), size = 1.3) +
     scale_color_discrete(name = "Parameters", labels = c("Beta(12, 2)", "Beta(15, 1.5)"))

ggplot() + 
  geom_line(aes(x = x, y = dbeta(x, 10, 1.85), color = "green"), size = 1.3) + xlab("x") +
  geom_line(aes(x = x, y = dbeta(x, 21, 1.6), color = "red"), size = 1.3) +
  scale_color_discrete(name = "Parameters", labels = c("Beta(10, 1.85)", "Beta(21, 1.6)"))


x <- seq(from = 0, to = 1, by = 0.01)
x[which(dbeta(x, 12, 2) < dbeta(x, 15, 1.5))]
x[which(dbeta(x, 10, 1.85) < dbeta(x, 21, 1.6))]

#Probability error
1 - pbeta(0.9, 12, 2) #0.378655
pbeta(0.9, 15, 1.5) #0.3600345

1 - pbeta(0.912, 10, 1.85) #0.2920298
pbeta(0.912, 21, 1.6) #0.2984941

library("plotly")
x <- seq(from = 0, to = 1, by = 0.01)
surface <- expand.grid(x = x, y = x)
surface$z <- dbeta(surface$x, 10, 1.85)*dbeta(surface$y, 10, 1.85)
dens1 <- array(surface$z, dim = c(length(x), length(x)))

plot_ly(x = x, y = x, z = dens) %>% add_surface()

surface$z <- dbeta(surface$x, 21, 1.6)*dbeta(surface$y, 21, 1.6)
dens2 <- array(surface$z, dim = c(length(x), length(x)))

plot_ly(showscale = FALSE) %>% add_surface(dens1) %>% add_surface(dens2, opacity = 0.7)

#============================================================================================
#Conditional density for Dihedral 
setwd(wd[1])
sample_lh1 <- getGeoDist("left helix", dim3)
sample_rh1 <- getGeoDist("right helix", dim3)
sample_di1 <- getGeoDist("dihedral", dim3)

fsample_di1 <- sample_di1[ which(sample_lh1 < 0.912 & sample_rh1 > 0.912) ]

setwd(wd[5])
sample_lh5 <- getGeoDist("left helix", dim3)
sample_rh5 <- getGeoDist("right helix", dim3)
sample_di5 <- getGeoDist("dihedral", dim3)

fsample_di5 <- sample_di5[ which(sample_lh1 < 0.912 & sample_rh1 > 0.912) ]

ggplot() + 
  geom_histogram(aes(x = c(fsample_di5), y = ..density..), fill = "green", alpha = 0.5, bins = 45) + xlab("x") +
  geom_histogram(aes(x = c(fsample_di1), y = ..density..), fill = "red", alpha = 0.5, bins = 45)
