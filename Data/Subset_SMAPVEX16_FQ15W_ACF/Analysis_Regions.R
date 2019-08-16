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
  SB231_232 = c(3.149, 4.32, 4.401, 4.573, 4.493), #SOYBEANS 231-232
  CN43 = c(2.65, 3.546, 5.529, 5.567, 6.951), #CANOLA 43
  WT255 = c(2.743, 4.385, 5.802, 5.442, 4.813) #WHEAT 255
  
)

tri_beta <- data.frame(
  X = 1:5,
  SB231_232 = c(11.789, 14.22, 13.332, 13.698, 12.304), #SOILBEANS 231-232
  CN43 = c(10.603, 11.911, 14.862, 14.06, 16.534), #CANOLA 43
  WT255 = c(10.169, 13.309, 17.049, 15.24, 13.007) #WHEAT 255
  
)

rv_alpha <- data.frame(
  X = 1:5,
  SB231_232 = c(8.343, 7.853, 5.892, 6.19, 5.999), #SOILBEANS 231-232
  CN43 = c(6.954, 5.886, 5.56, 6.682, 6.028), #CANOLA 43
  WT255 = c(5.784, 6.442, 6.552, 6.976, 6.137) #WHEAT 255
  
)


rv_beta <- c(
  X = 1:5,
  SB231_232 = c(17.102, 15.553, 10.829, 11.269, 10.28), #SOILBEANS 231-232
  CN43 = c(12.92, 11.54, 9.648, 10.779, 10.104), #CANOLA 43
  WT255 = c(10.578, 12.226, 12.212, 12.719, 11.37) #WHEAT 255
  
)


library(ggplot2)
#Analysis per sample
ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(tri_alpha, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()

ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(tri_beta, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()
