plotScattererMap(dim)
plotHistogramBeta("trihedral", dim, filter = TRUE)
data <- getFilteredData("trihedral", dim)
ksTestBeta("trihedral", dim, filter = TRUE)

plotHistogramBeta("random volume", dim, filter = TRUE)
data <- getFilteredData("random volume", dim)
ksTestBeta("random volume", dim, filter = TRUE)

#===========================================================

tri_alpha <- c(
  
  c(3.149, 4.32, 4.401, 4.573, 4.493), #SOILBEANS 231-232
  c(2.65, 3.546, 5.529, 0, 0), #CANOLA 43
  c(2.743, 4.385, 0, 0, 0) #WHEAT 255
  
)

tri_beta <- c(
  
  c(11.789, 14.22, 13.332, 13.698, 12.304), #SOILBEANS 231-232
  c(10.603, 11.911, 14.862, 0, 0), #CANOLA 43
  c(10.169, 13.309, 0, 0, 0) #WHEAT 255
  
)

rv_alpha <- c(
  
  c(0, 0, 5.892, 6.19, 5.999), #SOILBEANS 231-232
  c(0, 0, 5.56, 6.682, 6.028), #CANOLA 43
  c(0, 6.442, 6.552, 6.976, 6.137) #WHEAT 255
  
)

rv_beta <- c(
  
  c(0, 0, 10.829, 11.269, 10.28), #SOILBEANS 231-232
  c(0, 0, 9.648, 10.779, 10.104), #CANOLA 43
  c(0, 12.226, 12.212, 12.719, 11.37) #WHEAT 255
  
)