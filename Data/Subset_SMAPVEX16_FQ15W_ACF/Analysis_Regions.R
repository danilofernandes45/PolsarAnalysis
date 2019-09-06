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
  WT104 = c(2.824, 4.287, 6.411, 5.295, 5.651), #WHEAT 104
  WT105 = c(2.668, 4.456, 5.733, 3.347, 4.555), #WHEAT 105
  WT255 = c(2.743, 4.385, 5.802, 5.442, 4.813), #WHEAT 255
  CN43 = c(2.65, 3.546, 5.529, 5.567, 6.951), #CANOLA 43
  CN224 = c(2.371, 3.041, 6.207, 5.124, 5.708), #CANOLA 224
  OT102 = c(3.111, 3.374, 5.17, 6.184, 5.851), #OATS 102
  OT103 = c(3.085, 3.198, 5.083, 7.068, 6.39) #OATS 103
  
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

wt_tri_alpha <- data.frame(
  X = 1:5,
  WT104 = c(2.824, 4.287, 6.411, 5.295, 5.651), #WHEAT 104
  WT105 = c(2.668, 4.456, 5.733, 3.347, 4.555), #WHEAT 105
  WT255 = c(2.743, 4.385, 5.802, 5.442, 4.813) #WHEAT 255
)

ot_tri_alpha <- data.frame(
  X = 1:5,
  OT102 = c(3.111, 3.374, 5.17, 6.184, 5.851), #OATS 102
  OT103 = c(3.085, 3.198, 5.083, 7.068, 6.39) #OATS 103
  
)

tri_beta <- data.frame(
  X = 1:5,
  SB101 = c(10.086, 13.849, 11.209, 12.227, 13.957), #SOYBEANS 101
  SB231 = c(13.51, 15.066, 10.814, 12.99, 11.54), #SOYBEANS 231
  SB232 = c(11.602, 15.506, 13.68, 12.532, 11.28), #SOYBEANS 232
  WT104 = c(10.717, 12.054, 16.753, 12.065, 14.442), #WHEAT 104
  WT105 = c(10.152, 13.458, 14.528, 7.189, 11.353), #WHEAT 105
  WT255 = c(10.169, 13.309, 17.049, 15.24, 13.007), #WHEAT 255
  CN43 = c(10.603, 11.911, 14.862, 14.06, 16.534), #CANOLA 43
  CN224 = c(10.415, 11.818, 15.623, 11.8, 13.366), #CANOLA 224
  OT102 = c(11.172, 11.398, 15.745, 17.611, 14.977), #OATS 102
  OT103 = c(10.954, 10.916, 15.359, 21.859, 15.871) #OATS 103
  
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

wt_tri_beta <- data.frame(
  X = 1:5,
  WT104 = c(10.717, 12.054, 16.753, 12.065, 14.442), #WHEAT 104
  WT105 = c(10.152, 13.458, 14.528, 7.189, 11.353), #WHEAT 105
  WT255 = c(10.169, 13.309, 17.049, 15.24, 13.007) #WHEAT 255
)

ot_tri_beta <- data.frame(
  X = 1:5,
  OT103 = c(10.954, 10.916, 15.359, 21.859, 15.871), #OATS 103
  OT102 = c(11.172, 11.398, 15.745, 17.611, 14.977) #OATS 102
)

tri_mean <- tri_alpha / (tri_alpha + tri_beta)
tri_mean$X <- 1:5

sb_tri_mean <- sb_tri_alpha / (sb_tri_alpha + sb_tri_beta)
sb_tri_mean$X <- 1:5

cn_tri_mean <- cn_tri_alpha / (cn_tri_alpha + cn_tri_beta)
cn_tri_mean$X <- 1:5

wt_tri_mean <- wt_tri_alpha / (wt_tri_alpha + wt_tri_beta)
wt_tri_mean$X <- 1:5

ot_tri_mean <- ot_tri_alpha / (ot_tri_alpha + ot_tri_beta)
ot_tri_mean$X <- 1:5

rv_alpha <- data.frame(
  X = 1:5,
  SB101 = c(5.987, 7.662, 5.75, 5.473, 5.191), #SOYBEANS 101
  SB231 = c(9.937, 8.006, 6.062, 5.527, 5.849), #SOYBEANS 231
  SB232 = c(9.334, 8.654, 5.902, 5.891, 6.582), #SOYBEANS 231-232
  WT104 = c(6.145, 6.140, 7.546, 5.147, 6.272), #WHEAT 104
  WT105 = c(4.708, 5.582, 6.789, 4.819, 5.398), #WHEAT 105
  WT255 = c(5.784, 6.442, 6.552, 6.976, 6.137), #WHEAT 255
  CN43 = c(6.954, 5.886, 5.56, 6.682, 6.028), #CANOLA 43
  CN224 = c(7.494, 7.161, 6.556, 5.646, 6.062), #CANOLA 224
  OT102 = c(5.132, 5.503, 6.823, 6.051, 6.054), #OATS 102
  OT103 = c(5.199, 5.586, 6.292, 5.921, 6.292) #OATS 103
  
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

wt_rv_alpha <- data.frame(
  X = 1:5,
  WT104 = c(6.145, 6.140, 7.546, 5.147, 6.272), #WHEAT 104
  WT105 = c(4.708, 5.582, 6.789, 4.819, 5.398), #WHEAT 105
  WT255 = c(5.784, 6.442, 6.552, 6.976, 6.137) #WHEAT 255
)

ot_rv_alpha <- data.frame(
  X = 1:5,
  OT102 = c(5.132, 5.503, 6.823, 6.051, 6.054), #OATS 102
  OT103 = c(5.199, 5.586, 6.292, 5.921, 6.292) #OATS 103
)

rv_beta <- data.frame(
  X = 1:5,
  SB101 = c(10.993, 15.978, 11.792, 10.794, 9.438), #SOYBEANS 101
  SB231 = c(20.475, 16.477, 11.269, 9.914, 10.203), #SOYBEANS 231
  SB232 = c(18.516, 17.722, 10.969, 10.621, 11.241), #SOYBEANS 231-232
  WT104 = c(10.938, 10.973, 13.919, 7.825, 10.019), #WHEAT 104
  WT105 = c(9.005, 9.885, 12.832, 7.538, 8.922), #WHEAT 105
  WT255 = c(10.578, 12.226, 12.212, 12.719, 11.37), #WHEAT 255
  CN43 = c(12.92, 11.54, 9.648, 10.779, 10.104), #CANOLA 43
  CN224 = c(12.707, 15.094, 11.48, 8.854, 9.977), #CANOLA 224
  OT102 = c(9.49, 10.916, 12.764, 11.23, 9.989), #OATS 102
  OT103 = c(9.536, 11.037, 11.552, 11.128, 10.301) #OATS 103
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

wt_rv_beta <- data.frame(
  X = 1:5,
  WT104 = c(10.938, 10.973, 13.919, 7.825, 10.019), #WHEAT 104
  WT105 = c(9.005, 9.885, 12.832, 7.538, 8.922), #WHEAT 105
  WT255 = c(10.578, 12.226, 12.212, 12.719, 11.37) #WHEAT 255
)

ot_rv_beta <- data.frame(
  X = 1:5,
  OT102 = c(9.49, 10.916, 12.764, 11.23, 9.989), #OATS 102
  OT103 = c(9.536, 11.037, 11.552, 11.128, 10.301) #OATS 103
)

rv_mean <- rv_alpha / (rv_alpha + rv_beta)
rv_mean$X <- 1:5

sb_rv_mean <- sb_rv_alpha / (sb_rv_alpha + sb_rv_beta)
sb_rv_mean$X <- 1:5

cn_rv_mean <- cn_rv_alpha / (cn_rv_alpha + cn_rv_beta)
cn_rv_mean$X <- 1:5

wt_rv_mean <- wt_rv_alpha / (wt_rv_alpha + wt_rv_beta)
wt_rv_mean$X <- 1:5

ot_rv_mean <- ot_rv_alpha / (ot_rv_alpha + ot_rv_beta)
ot_rv_mean$X <- 1:5

library(ggplot2)
#Analysis per sample
ggplot(aes(x = X, y = value, group = variable, color = variable), data = melt(ot_rv_beta, id.vars = "X")) +
  xlab("Sample") + ylab("Parameter value") +
  geom_point() + geom_line()

alpha_tri = c()
alpha_rv = c()
beta_tri = c()
beta_rv = c()
mean_tri = c()
mean_rv = c()

for (i in c(2, 5)){
  dist_med <- sum(apply(tri_alpha[, i:(i+2)], 1, function(x) max(x) - min(x))) / 5
  alpha_tri <- c(alpha_tri, dist_med / (max(tri_alpha[, i:(i+2)]) - min(tri_alpha[, i:(i+2)])))
  
  dist_med <- sum(apply(rv_alpha[, i:(i+2)], 1, function(x) max(x) - min(x))) / 5
  alpha_rv <- c(alpha_rv, dist_med / (max(rv_alpha[, i:(i+2)]) - min(rv_alpha[, i:(i+2)])))
  
  dist_med <- sum(apply(tri_beta[, i:(i+2)], 1, function(x) max(x) - min(x))) / 5
  beta_tri <- c(beta_tri, dist_med / (max(tri_beta[, i:(i+2)]) - min(tri_beta[, i:(i+2)])))
  
  dist_med <- sum(apply(rv_beta[, i:(i+2)], 1, function(x) max(x) - min(x))) / 5
  beta_rv <- c(beta_rv, dist_med / (max(rv_beta[, i:(i+2)]) - min(rv_beta[, i:(i+2)])))
  
  dist_med <- sum(apply(tri_mean[, i:(i+2)], 1, function(x) max(x) - min(x))) / 5
  mean_tri <- c(mean_tri, dist_med / (max(tri_mean[, i:(i+2)]) - min(tri_mean[, i:(i+2)])))
  
  dist_med <- sum(apply(rv_mean[, i:(i+2)], 1, function(x) max(x) - min(x))) / 5
  mean_rv <- c(mean_rv, dist_med / (max(rv_mean[, i:(i+2)]) - min(rv_mean[, i:(i+2)])))
}

for (i in c(8, 10)){
  dist_med <- sum(apply(tri_alpha[, i:(i+1)], 1, function(x) max(x) - min(x))) / 5
  alpha_tri <- c(alpha_tri, dist_med / (max(tri_alpha[, i:(i+1)]) - min(tri_alpha[, i:(i+1)])))
  
  dist_med <- sum(apply(rv_alpha[, i:(i+1)], 1, function(x) max(x) - min(x))) / 5
  alpha_rv <- c(alpha_rv, dist_med / (max(rv_alpha[, i:(i+1)]) - min(rv_alpha[, i:(i+1)])))
  
  dist_med <- sum(apply(tri_beta[, i:(i+1)], 1, function(x) max(x) - min(x))) / 5
  beta_tri <- c(beta_tri, dist_med / (max(tri_beta[, i:(i+1)]) - min(tri_beta[, i:(i+1)])))
  
  dist_med <- sum(apply(rv_beta[, i:(i+1)], 1, function(x) max(x) - min(x))) / 5
  beta_rv <- c(beta_rv, dist_med / (max(rv_beta[, i:(i+1)]) - min(rv_beta[, i:(i+1)])))
  
  dist_med <- sum(apply(tri_mean[, i:(i+1)], 1, function(x) max(x) - min(x))) / 5
  mean_tri <- c(mean_tri, dist_med / (max(tri_mean[, i:(i+1)]) - min(tri_mean[, i:(i+1)])))
  
  dist_med <- sum(apply(rv_mean[, i:(i+1)], 1, function(x) max(x) - min(x))) / 5
  mean_rv <- c(mean_rv, dist_med / (max(rv_mean[, i:(i+1)]) - min(rv_mean[, i:(i+1)])))
}

error <- data.frame(
  X = c("SB", "WT", "CN", "OT"),
  alpha_tri = alpha_tri,
  alpha_rv = alpha_rv,
  beta_tri = beta_tri,
  beta_rv = beta_rv,
  mean_tri = mean_tri,
  mean_rv = mean_rv
)

error['Min_Error'] = colnames(error)[apply(error, 1, which.min)]
