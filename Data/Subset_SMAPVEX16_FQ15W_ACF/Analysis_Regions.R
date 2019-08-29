plotScattererMap(dim1)
plotHistogramBeta("trihedral", dim1, filter = TRUE)
data <- getFilteredData("trihedral", dim1)
ksTestBeta("trihedral", dim1, filter = TRUE)

plotHistogramBeta("random volume", dim1, filter = TRUE)
data <- getFilteredData("random volume", dim1)
ksTestBeta("random volume", dim1, filter = TRUE)

plotScattererMap(dim2)
plotHistogramBeta("trihedral", dim2, filter = TRUE)
data <- getFilteredData("trihedral", dim2)
ksTestBeta("trihedral", dim2, filter = TRUE)

plotHistogramBeta("random volume", dim2, filter = TRUE)
data <- getFilteredData("random volume", dim2)
ksTestBeta("random volume", dim2, filter = TRUE)

#===========================================================

tri_alpha <- data.frame(
  X = 1:5,
  SB101 = c(2.331, 3.95, 3.81, 4.314, 4.798), #SOYBEANS 101
  SB231 = c(3.557, 4.578, 3.594, 4.291, 4.192), #SOYBEANS 231
  SB232 = c(3.039, 4.752, 4.553, 4.217, 4.02), #SOYBEANS 232
  CN43 = c(2.65, 3.546, 5.529, 5.567, 6.951), #CANOLA 43
  CN224 = c(2.371, 3.041, 6.207, 5.124, 5.708), #CANOLA 224
  WT104 = c(3.085, 3.198, 5.083, 7.068, 6.39), #WHEAT 104
  WT105 = c(3.111, 3.374, 5.17, 6.184, 5.851), #WHEAT 105
  WT255 = c(2.743, 4.385, 5.802, 5.442, 4.813), #WHEAT 255
  OT102 = c(2.395, 3.099, 4.189, 4.563, 4.361), #OATS 102
  OT103 = c(2.637, 3.453, 4.721, 6.045, 4.847) #OATS 103
  
)

sb_tri_alpha <- data.frame(
  X = 1:5,
  SB101 = c(2.331, 3.95, 3.81, 4.314, 4.798), #SOYBEANS 101
  SB231 = c(3.557, 4.578, 3.594, 4.291, 4.192), #SOYBEANS 231
  SB232 = c(3.039, 4.752, 4.553, 4.217, 4.02) #SOYBEANS 232
)

cn_tri_alpha <- data.frame(
  X = 1:5,
  CN43 = c(2.65, 3.546, 5.529, 5.567, 6.951), #CANOLA 43
  CN224 = c(2.371, 3.041, 6.207, 5.124, 5.708) #CANOLA 224
)

tri_beta <- data.frame(
  X = 1:5,
  SB101 = c(10.086, 13.849, 11.209, 12.227, 13.957), #SOYBEANS 101
  SB231 = c(13.51, 15.066, 10.814, 12.99, 11.54), #SOYBEANS 231
  SB232 = c(11.602, 15.506, 13.68, 12.532, 11.28), #SOYBEANS 232
  CN43 = c(10.603, 11.911, 14.862, 14.06, 16.534), #CANOLA 43
  CN224 = c(10.415, 11.818, 15.623, 11.8, 13.366), #CANOLA 224
  WT104 = c(10.954, 10.916, 15.359, 21.859, 15.871), #WHEAT 104
  WT105 = c(11.172, 11.398, 15.745, 17.611, 14.977), #WHEAT 105
  WT255 = c(10.169, 13.309, 17.049, 15.24, 13.007), #WHEAT 255
  OT102 = c(10.252, 10.719, 13.215, 13.168, 11.956), #OATS 102
  OT103 = c(10.188, 11.496, 12.277, 13.924, 12.266) #OATS 103
  
)

sb_tri_beta <- data.frame(
  X = 1:5,
  SB101 = c(10.086, 13.849, 11.209, 12.227, 13.957), #SOYBEANS 101
  SB231 = c(13.51, 15.066, 10.814, 12.99, 11.54), #SOYBEANS 231
  SB232 = c(11.602, 15.506, 13.68, 12.532, 11.28) #SOYBEANS 232
)

cn_tri_beta <- data.frame(
  X = 1:5,
  CN43 = c(10.603, 11.911, 14.862, 14.06, 16.534), #CANOLA 43
  CN224 = c(10.415, 11.818, 15.623, 11.8, 13.366) #CANOLA 224
)

tri_mean <- tri_alpha / (tri_alpha + tri_beta)
tri_mean$X <- 1:5

sb_tri_mean <- sb_tri_alpha / (sb_tri_alpha + sb_tri_beta)
sb_tri_mean$X <- 1:5

cn_tri_mean <- cn_tri_alpha / (cn_tri_alpha + cn_tri_beta)
cn_tri_mean$X <- 1:5

rv_alpha <- data.frame(
  X = 1:5,
  SB101 = c(5.987, 7.662, 5.75, 5.473, 5.191), #SOYBEANS 101
  SB231 = c(9.937, 8.006, 6.062, 5.527, 5.849), #SOYBEANS 231
  SB232 = c(9.334, 8.654, 5.902, 5.891, 6.582), #SOYBEANS 231-232
  CN43 = c(6.954, 5.886, 5.56, 6.682, 6.028), #CANOLA 43
  CN224 = c(7.494, 7.161, 6.556, 5.646, 6.062), #CANOLA 224
  WT104 = c(5.199, 5.586, 6.292, 5.921, 6.292), #WHEAT 104
  WT105 = c(5.132, 5.503, 6.823, 6.051, 6.054), #WHEAT 105
  WT255 = c(5.784, 6.442, 6.552, 6.976, 6.137), #WHEAT 255
  OT102 = c(4.722, 7.040, 6.593, 6.054, 5.813), #OATS 102
  OT103 = c(5.937, 6.742, 6.460, 6.069, 5.956) #OATS 103
  
)

sb_rv_alpha <- data.frame(
  X = 1:5,
  SB101 = c(5.987, 7.662, 5.75, 5.473, 5.191), #SOYBEANS 101
  SB231 = c(9.937, 8.006, 6.062, 5.527, 5.849), #SOYBEANS 231
  SB232 = c(9.334, 8.654, 5.902, 5.891, 6.582) #SOYBEANS 231-232
  
)

cn_rv_alpha <- data.frame(
  X = 1:5,
  CN43 = c(6.954, 5.886, 5.56, 6.682, 6.028), #CANOLA 43
  CN224 = c(7.494, 7.161, 6.556, 5.646, 6.062) #CANOLA 224
)

rv_beta <- data.frame(
  X = 1:5,
  SB101 = c(10.993, 15.978, 11.792, 10.794, 9.438), #SOYBEANS 101
  SB231 = c(20.475, 16.477, 11.269, 9.914, 10.203), #SOYBEANS 231
  SB232 = c(18.516, 17.722, 10.969, 10.621, 11.241), #SOYBEANS 231-232
  CN43 = c(12.92, 11.54, 9.648, 10.779, 10.104), #CANOLA 43
  CN224 = c(12.707, 15.094, 11.48, 8.854, 9.977), #CANOLA 224
  WT104 = c(9.536, 11.037, 11.552, 11.128, 10.301), #WHEAT 104
  WT105 = c(9.49, 10.916, 12.764, 11.23, 9.989), #WHEAT 105
  WT255 = c(10.578, 12.226, 12.212, 12.719, 11.37), #WHEAT 255
  OT102 = c(10.100, 14.182, 13.566, 11.188, 10.290), #OATS 102
  OT103 = c(11.459, 13.172, 10.961, 9.51, 10.951) #OATS 103
)

sb_rv_beta <- data.frame(
  X = 1:5,
  SB101 = c(10.993, 15.978, 11.792, 10.794, 9.438), #SOYBEANS 101
  SB231 = c(20.475, 16.477, 11.269, 9.914, 10.203), #SOYBEANS 231
  SB232 = c(18.516, 17.722, 10.969, 10.621, 11.241) #SOYBEANS 231-232
)

cn_rv_beta <- data.frame(
  X = 1:5,
  CN43 = c(12.92, 11.54, 9.648, 10.779, 10.104), #CANOLA 43
  CN224 = c(12.707, 15.094, 11.48, 8.854, 9.977) #CANOLA 224
)

rv_mean <- rv_alpha / (rv_alpha + rv_beta)
rv_mean$X <- 1:5

sb_rv_mean <- sb_rv_alpha / (sb_rv_alpha + sb_rv_beta)
sb_rv_mean$X <- 1:5

cn_rv_mean <- cn_rv_alpha / (cn_rv_alpha + cn_rv_beta)
cn_rv_mean$X <- 1:5

library(ggplot2)
#Analysis per sample
ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(rv_beta, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()

