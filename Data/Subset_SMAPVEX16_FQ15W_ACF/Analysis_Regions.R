plotScattererMap(dim)
plotHistogramBeta("trihedral", dim, filter = TRUE)
data <- getFilteredData("trihedral", dim)
ksTestBeta("trihedral", dim, filter = TRUE)

plotHistogramBeta("random volume", dim, filter = TRUE)
data <- getFilteredData("random volume", dim)
ksTestBeta("random volume", dim, filter = TRUE)

#===========================================================

tri_alpha <- data.frame(
  X = 1:5,
  SB101 = c(2.331, 3.95, 3.81, 4.314, 4.798), #SOYBEANS 101
  SB231 = c(3.557, 4.578, 3.594, 4.291, 4.192), #SOYBEANS 231
  SB232 = c(3.039, 4.752, 4.553, 4.217, 4.02), #SOYBEANS 232
  CN43 = c(2.65, 3.546, 5.529, 5.567, 6.951), #CANOLA 43
  WT255 = c(2.743, 4.385, 5.802, 5.442, 4.813) #WHEAT 255
  
)

tri_beta <- data.frame(
  X = 1:5,
  SB101 = c(10.086, 13.849, 11.209, 12.227, 13.957), #SOYBEANS 101
  SB231 = c(13.51, 15.066, 10.814, 12.99, 11.54), #SOYBEANS 231
  SB232 = c(11.602, 15.506, 13.68, 12.532, 11.28), #SOYBEANS 232
  CN43 = c(10.603, 11.911, 14.862, 14.06, 16.534), #CANOLA 43
  WT255 = c(10.169, 13.309, 17.049, 15.24, 13.007) #WHEAT 255
  
)

tri_mean <- tri_alpha / (tri_alpha + tri_beta)
tri_mean$X <- 1:5

rv_alpha <- data.frame(
  X = 1:5,
  SB101 = c(5.987, 7.662, 5.75, 5.473, 5.191), #SOYBEANS 101
  SB231 = c(9.937, 8.006, 6.062, 5.527, 5.849), #SOYBEANS 231
  SB232 = c(9.334, 8.654, 5.902, 5.891, 6.582), #SOYBEANS 231-232
  CN43 = c(6.954, 5.886, 5.56, 6.682, 6.028), #CANOLA 43
  WT255 = c(5.784, 6.442, 6.552, 6.976, 6.137) #WHEAT 255
  
)


rv_beta <- data.frame(
  X = 1:5,
  SB101 = c(10.993, 15.978, 11.792, 10.794, 9.438), #SOYBEANS 101
  SB231 = c(20.475, 16.477, 11.269, 9.914, 10.203), #SOYBEANS 231
  SB232 = c(18.516, 17.722, 10.969, 10.621, 11.241), #SOYBEANS 231-232
  CN43 = c(12.92, 11.54, 9.648, 10.779, 10.104), #CANOLA 43
  WT255 = c(10.578, 12.226, 12.212, 12.719, 11.37) #WHEAT 255
  
)

rv_mean <- rv_alpha / (rv_alpha + rv_beta)
rv_mean$X <- 1:5


library(ggplot2)
#Analysis per sample
ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(tri_alpha, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()

ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(tri_beta, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()

ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(tri_mean, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()

ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(rv_alpha, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()

ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(rv_beta, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()

ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(rv_mean, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()
