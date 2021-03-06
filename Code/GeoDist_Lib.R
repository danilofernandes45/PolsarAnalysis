#Functions to analysis

library(reshape2)
library(ggplot2)
library(stats4)
library(raster)
library(logitnorm)
library(dplyr)
library(hrbrthemes)
require(extrafont)

source("pert.R")

read_data <- function(file, dim) {
  
  init_row <- dim[1]
  nrows <- dim[2]
  init_col <- dim[3]
  ncols <- dim[4]
  
  data <- raster(paste(file, ".grd", sep = ""))
  return( getValuesBlock(data, row = init_row, nrows = nrows, col = init_col, ncols = ncols, format = "matrix") )
  
}

read_complex_data <- function(file, dim) {
  
  nrow <- dim[2]
  ncol <- dim[4]
  num_elements <- nrow * ncol
  
  # dim[3] <- 2*(dim[3] - 1) + 1
  # dim[4] <- 2*dim[4]
  # data <- c( t( read_data( file, dim ) ) ) #Generate vector by column
  # bivector <- matrix(data, ncol = 2, nrow = num_elements, byrow = TRUE)
  # return(
  #   matrix(complex(real = bivector[,1], imaginary = bivector[,2]), nrow = nrow, ncol = ncol, byrow = TRUE)
  # )
  
  real_data <- c(read_data( paste(file, "_Re", sep = ""), dim ))
  img_data <- c(read_data( paste(file, "_Im", sep = ""), dim ))
  return(
     matrix(complex(real = real_data, imaginary = img_data), nrow = nrow, ncol = ncol, byrow = TRUE)
  )
}

getSample <- function(dim){
  
  sample <- array(0, dim = c(dim[2], dim[4], 6))
  sample[,,1] <- read_data("HHHH", dim)
  sample[,,2] <- read_data("HVHV", dim)
  sample[,,3] <- read_data("VVVV", dim)
  
  sample[,,4] <- read_complex_data("HHHV", dim)
  sample[,,5] <- read_complex_data("HHVV", dim)
  sample[,,6] <- read_complex_data("HVVV", dim)  
  
  return(sample)
}

getGeoDist <- function(scatterer, dim){
  
  hhhh <- read_data("HHHH", dim)
  hvhv <- read_data("HVHV", dim)
  vvvv <- read_data("VVVV", dim)
  
  hhhv <- read_complex_data("HHHV", dim)
  hhvv <- read_complex_data("HHVV", dim)
  hvvv <- read_complex_data("HVVV", dim)
  
  mod_kennaugh <- c(sqrt( hhhh^2 + hvhv^2 + vvvv^2 + 2*Mod(hhhv)^2 + 2*Mod(hhvv)^2 + 2*Mod(hvvv)^2 ))
  product <- c(0)
  
  if(scatterer == "trihedral"){
    product <- hhhh / mod_kennaugh
  }
  
  else if(scatterer == "dihedral"){
    product <- hvhv / mod_kennaugh
  }
  
  else if(scatterer == "random volume"){
    product <- ( 2*hhhh + hvhv + vvvv ) / ( sqrt(6)*mod_kennaugh )
  }
  
  else if(scatterer == "narrow dihedral"){
    product <- (hhhh + 9*hvhv + 6*Re(hhhv)) / (10*mod_kennaugh)
  }
  
  else if(scatterer == "cylinder"){
    product <- ( 9*hhhh + hvhv + 6*Re(hhhv) ) / (10*mod_kennaugh)
  }
  
  else if(scatterer == "dipole"){
    product <- ( hhhh + hvhv - 2*Re(hhhv) ) / (2*mod_kennaugh)
  }
  
  else if(scatterer == "left helix"){
    product <- ( hvhv + vvvv - 2*Im(hvvv) ) / (2*mod_kennaugh)
  }
  
  else if(scatterer == "right helix"){
    product <- ( hvhv + vvvv + 2*Im(hvvv) ) / (2*mod_kennaugh)
  }
  
  else if(scatterer == "+1/4-wave"){
    product <- ( hhhh + hvhv - 2*Im(hhhv) ) / (2*mod_kennaugh)
  }
  
  else if(scatterer == "-1/4-wave"){
    product <- ( hhhh + hvhv + 2*Im(hhhv) ) / (2*mod_kennaugh)
  }
  
  return( (2/pi)*acos( product ) )
}

getSimilarity <- function(scatterer, dim){
  
  hhhh <- read_data("HHHH", dim)
  hvhv <- read_data("HVHV", dim)
  vvvv <- read_data("VVVV", dim)
  
  hhhv <- read_complex_data("HHHV", dim)
  hhvv <- read_complex_data("HHVV", dim)
  hvvv <- read_complex_data("HVVV", dim)
  
  mod_kennaugh <- c(sqrt( hhhh^2 + hvhv^2 + vvvv^2 + 2*Mod(hhhv)^2 + 2*Mod(hhvv)^2 + 2*Mod(hvvv)^2 ))
  product <- c(0)
  
  if(scatterer == "trihedral"){
    product <- hhhh / mod_kennaugh
  }
  
  else if(scatterer == "dihedral"){
    product <- hvhv / mod_kennaugh
  }
  
  else if(scatterer == "random volume"){
    product <- ( 2*hhhh + hvhv + vvvv ) / ( sqrt(6)*mod_kennaugh )
  }
  
  else if(scatterer == "narrow dihedral"){
    product <- (hhhh + 9*hvhv + 6*Re(hhhv)) / (10*mod_kennaugh)
  }
  
  else if(scatterer == "cylinder"){
    product <- ( 9*hhhh + hvhv + 6*Re(hhhv) ) / (10*mod_kennaugh)
  }
  
  else if(scatterer == "dipole"){
    product <- ( hhhh + hvhv - 2*Re(hhhv) ) / (2*mod_kennaugh)
  }
  
  else if(scatterer == "left helix"){
    product <- ( hvhv + vvvv - 2*Im(hvvv) ) / (2*mod_kennaugh)
  }
  
  else if(scatterer == "right helix"){
    product <- ( hvhv + vvvv + 2*Im(hvvv) ) / (2*mod_kennaugh)
  }
  
  else if(scatterer == "+1/4-wave"){
    product <- ( hhhh + hvhv - 2*Im(hhhv) ) / (2*mod_kennaugh)
  }
  
  else if(scatterer == "-1/4-wave"){
    product <- ( hhhh + hvhv + 2*Im(hhhv) ) / (2*mod_kennaugh)
  }
  
  return( 1 - (2/pi)*acos( product ) )
}

getMaxSimilarities <- function(dim){
  
  hhhh <- read_data("HHHH", dim)
  hvhv <- read_data("HVHV", dim)
  vvvv <- read_data("VVVV", dim)
  
  hhhv <- read_complex_data("HHHV", dim)
  hhvv <- read_complex_data("HHVV", dim)
  hvvv <- read_complex_data("HVVV", dim)
  
  mod_kennaugh <- c(sqrt( hhhh^2 + hvhv^2 + vvvv^2 + 2*Mod(hhhv)^2 + 2*Mod(hhvv)^2 + 2*Mod(hvvv)^2 ))
  
  similarity_trihedral <- 1 - (2/pi)*acos( hhhh / mod_kennaugh )
  similarity_dihedral  <- 1 - (2/pi)*acos( hvhv / mod_kennaugh )
  similarity_rand_vol  <- 1 - (2/pi)*acos( ( 2*hhhh + hvhv + vvvv ) / ( sqrt(6)*mod_kennaugh ) ) 
  
  similarity_narrow_dihedral <- 1 - (2/pi)*acos((hhhh + 9*hvhv + 6*Re(hhhv))/
                                                  (10*mod_kennaugh) )
  
  similarity_cylinder <- 1 - (2/pi)*acos( ( 9*hhhh + hvhv + 6*Re(hhhv) ) /
                                            (10*mod_kennaugh) )
  
  similarity_dipole <- 1 - (2/pi)*acos( ( hhhh + hvhv - 2*Re(hhhv) ) /
                                          (2*mod_kennaugh) )
  
  similarity_left_helix <- 1 - (2/pi)*acos( ( hvhv + vvvv - 2*Im(hvvv) ) /
                                              (2*mod_kennaugh) )
  
  similarity_right_helix <- 1 - (2/pi)*acos( ( hvhv + vvvv + 2*Im(hvvv) ) /
                                               (2*mod_kennaugh) )
  
  similarity_pos_wave <- 1 - (2/pi)*acos( ( hhhh + hvhv - 2*Im(hhhv) ) /
                                            (2*mod_kennaugh) )
  
  similarity_neg_wave <- 1 - (2/pi)*acos( ( hhhh + hvhv + 2*Im(hhhv) ) /
                                            (2*mod_kennaugh) )
  
  max <- pmax(similarity_trihedral, similarity_dihedral, similarity_rand_vol, similarity_narrow_dihedral,
              similarity_cylinder, similarity_dipole, similarity_left_helix, similarity_right_helix,
              similarity_pos_wave, similarity_neg_wave, na.rm = TRUE)
  
  return(max)
  
}

getMapMatrix <- function(dim){
  
  nrow <- dim[2]
  ncol <- dim[4]
  
  hhhh <- c( read_data("HHHH", dim) ) #Get by column
  hvhv <- c( read_data("HVHV", dim) )
  vvvv <- c( read_data("VVVV", dim) )
  
  hhhv <- c( read_complex_data("HHHV", dim) )
  hhvv <- c( read_complex_data("HHVV", dim) )
  hvvv <- c( read_complex_data("HVVV", dim) )
  
  mod_kennaugh <- sqrt( hhhh^2 + hvhv^2 + vvvv^2 + 2*Mod(hhhv)^2 + 2*Mod(hhvv)^2 + 2*Mod(hvvv)^2 )
  
  similarity_trihedral <- 1 - (2/pi)*acos( hhhh / mod_kennaugh )
  similarity_dihedral  <- 1 - (2/pi)*acos( hvhv / mod_kennaugh )
  similarity_rand_vol  <- 1 - (2/pi)*acos( ( 2*hhhh + hvhv + vvvv ) / ( sqrt(6)*mod_kennaugh ) ) 
  
  similarity_narrow_dihedral <- 1 - (2/pi)*acos((hhhh + 9*hvhv + 6*Re(hhhv))/
                                                  (10*mod_kennaugh) )
  
  similarity_cylinder <- 1 - (2/pi)*acos( ( 9*hhhh + hvhv + 6*Re(hhhv) ) /
                                            (10*mod_kennaugh) )
  
  similarity_dipole <- 1 - (2/pi)*acos( ( hhhh + hvhv - 2*Re(hhhv) ) /
                                          (2*mod_kennaugh) )
  
  similarity_left_helix <- 1 - (2/pi)*acos( ( hvhv + vvvv - 2*Im(hvvv) ) /
                                              (2*mod_kennaugh) )
  
  similarity_right_helix <- 1 - (2/pi)*acos( ( hvhv + vvvv + 2*Im(hvvv) ) /
                                               (2*mod_kennaugh) )
  
  similarity_pos_wave <- 1 - (2/pi)*acos( ( hhhh + hvhv - 2*Im(hhhv) ) /
                                            (2*mod_kennaugh) )
  
  similarity_neg_wave <- 1 - (2/pi)*acos( ( hhhh + hvhv + 2*Im(hhhv) ) /
                                            (2*mod_kennaugh) )
  
  max <- pmax(similarity_trihedral, similarity_dihedral, similarity_rand_vol, similarity_narrow_dihedral,
              similarity_cylinder, similarity_dipole, similarity_left_helix, similarity_right_helix,
              similarity_pos_wave, similarity_neg_wave, na.rm = TRUE)
  
  
  map <- array("NA", dim = c(nrow * ncol))
  map[ which(similarity_neg_wave == max) ] <- "-1/4-wave" #yellow
  map[ which(similarity_pos_wave == max) ] <- "+1/4-wave" #orange
  map[ which(similarity_cylinder == max) ] <- "Cylinder" #red
  map[ which(similarity_dihedral == max) ] <- "Dihedral" #blue
  map[ which(similarity_dipole == max) ] <- "Dipole" #navy
  map[ which(similarity_left_helix == max) ] <- "Left helix" #peru
  map[ which(similarity_narrow_dihedral == max) ] <- "Narrow dihedral" #azure
  map[ which(similarity_rand_vol == max) ] <- "Random volume" #green3
  map[ which(similarity_right_helix == max) ] <- "Right helix" #purple
  map[ which(similarity_trihedral == max) ] <- "Trihedral" #magenta
  
  return( matrix(map, nrow = nrow, ncol = ncol) )
  
}

getFilteredData <- function(scatterer, dim){
  
  hhhh <- read_data("HHHH", dim)
  hvhv <- read_data("HVHV", dim)
  vvvv <- read_data("VVVV", dim)
  
  hhhv <- read_complex_data("HHHV", dim)
  hhvv <- read_complex_data("HHVV", dim)
  hvvv <- read_complex_data("HVVV", dim)
  
  mod_kennaugh <- c(sqrt( hhhh^2 + hvhv^2 + vvvv^2 + 2*Mod(hhhv)^2 + 2*Mod(hhvv)^2 + 2*Mod(hvvv)^2 ))
  
  dist_trihedral <- (2/pi)*acos( hhhh / mod_kennaugh )
  dist_dihedral  <- (2/pi)*acos( hvhv / mod_kennaugh )
  dist_rand_vol  <- (2/pi)*acos( ( 2*hhhh + hvhv + vvvv ) / ( sqrt(6)*mod_kennaugh ) ) 
  
  dist_narrow_dihedral <- (2/pi)*acos((hhhh + 9*hvhv + 6*Re(hhhv))/
                                                  (10*mod_kennaugh) )
  
  dist_cylinder <- (2/pi)*acos( ( 9*hhhh + hvhv + 6*Re(hhhv) ) /
                                            (10*mod_kennaugh) )
  
  dist_dipole <- (2/pi)*acos( ( hhhh + hvhv - 2*Re(hhhv) ) /
                                          (2*mod_kennaugh) )
  
  dist_left_helix <- (2/pi)*acos( ( hvhv + vvvv - 2*Im(hvvv) ) /
                                              (2*mod_kennaugh) )
  
  dist_right_helix <- (2/pi)*acos( ( hvhv + vvvv + 2*Im(hvvv) ) /
                                               (2*mod_kennaugh) )
  
  dist_pos_wave <- (2/pi)*acos( ( hhhh + hvhv - 2*Im(hhhv) ) /
                                            (2*mod_kennaugh) )
  
  dist_neg_wave <- (2/pi)*acos( ( hhhh + hvhv + 2*Im(hhhv) ) /
                                            (2*mod_kennaugh) )
  
  min <- pmin(dist_trihedral, dist_dihedral, dist_rand_vol, dist_narrow_dihedral,
              dist_cylinder, dist_dipole, dist_left_helix, dist_right_helix,
              dist_pos_wave, dist_neg_wave, na.rm = TRUE)
  
  
  if(scatterer == "-1/4-wave"){
    return ( dist_neg_wave[ which(dist_neg_wave == min) ] )
  }
  
  else if(scatterer == "+1/4-wave"){
    return ( dist_pos_wave[ which(dist_pos_wave == min) ] )
  }
  
  else if(scatterer == "cylinder"){
    return ( dist_cylinder[ which(dist_cylinder == min) ] )
  }
  
  else if(scatterer == "dihedral"){
    return ( dist_dihedral[ which(dist_dihedral == min) ] )
  }
  
  else if(scatterer == "narrow dihedral"){
    return ( dist_narrow_dihedral[ which(dist_narrow_dihedral == min) ] )
  }
  
  else if(scatterer == "dipole"){
    return ( dist_dipole[ which(dist_dipole == min) ] )
  }
  
  else if(scatterer == "left helix"){
    return ( dist_left_helix[ which(dist_left_helix == min) ] )
  }
  
  else if(scatterer == "right helix"){
    return ( dist_right_helix[ which(dist_right_helix == min) ] )
  }
  
  else if(scatterer == "random volume"){
    return ( dist_rand_vol[ which(dist_rand_vol == min) ] )
  }
  
  else if(scatterer == "trihedral"){
    return ( dist_trihedral[ which(dist_trihedral == min) ] )
  }
  
  return( c(0) )
  
}

plotScattererMap <- function(dim, title = ""){
  
  my_colours <- c("-1/4-wave" = "yellow", "+1/4-wave" = "orange", "Cylinder" = "red", "Dihedral" = "blue", "Dipole" = "navy",
                  "Left helix" = "peru", "NA" = "snow", "Narrow dihedral" = "greenyellow", "Random volume" = "green3",
                  "Right helix" = "purple", "Trihedral" = "magenta")
  
  sample <- getMapMatrix(dim)
  data_frame <- data.frame(melt(sample))
  data_frame[1] <- data_frame[1] + dim[1]
  data_frame[2] <- data_frame[2] + dim[3]
  ggplot(data = data_frame, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() + scale_fill_manual(values = my_colours) + xlab("column") + ylab("row") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  
}

plotHeatmap <- function(scatterer, dim, title = ""){
  
  similarities <- getSimilarity(scatterer, dim)
  ggplot( data = data.frame(melt(similarities)), aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() + scale_fill_gradient() + ylab("row") + xlab("column") +
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  
}

plotHistogramBeta <- function(scatterer, dim, filter = FALSE, title = ""){
  
  sample <- c(0)
  #Get sample
  if(filter){
    sample <- getFilteredData(scatterer, dim)
  }
  
  else {
    sample <- getGeoDist(scatterer, dim)
  }
  
  #Estimative of Beta parameters
  mean <- mean(sample)
  var <- sd(sample) ^ 2
  alpha <- mean * ( mean * (1 - mean) / var - 1 )
  beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
  
  #Plot
  x <- seq( from = 0, to = 1, by = 0.001)
  desc <- paste("Beta(", round(alpha, 3), ", ", round(beta, 3), ")", sep="")
  
  ggplot() + 
    geom_histogram(aes(x = c(sample), y = ..density..), bins = 45) + xlab("x") +
    geom_line(aes(x = x, y = dbeta(x, alpha, beta), colour = "red"), size = 1.3) +
    scale_color_discrete(name = "Parameters", labels = c(desc)) +
    #theme_ipsum(base_family = "Times New Roman", base_size = 20, axis_title_size = 20) +
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
}

plotHistogramNorm <- function(scatterer, dim, filter = FALSE, title = ""){
  
  sample <- c(0)
  #Get sample
  if(filter){
    sample <- getFilteredData(scatterer, dim)
  }
  
  else {
    sample <- getSimilarity(scatterer, dim)
  }
  
  #Estimative of Beta parameters
  mean <- mean(sample)
  sd <- sd(sample)
  
  #Plot
  x <- seq( from = 0, to = 1, by = 0.001)
  desc <- paste("N(", round(mean, 3), ", ", round(sd, 3), "²)", sep="")
  
  ggplot() + 
    geom_histogram(aes(x = c(sample), y = ..density..)) + xlab("x") +
    geom_line(aes(x = x, y = dnorm(x, mean, sd), colour = "red"), size = 1.3) +
    scale_color_discrete(name = "Parameters", labels = c(desc)) +
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
}

plotHistogramGamma <- function(scatterer, dim, filter = FALSE, title = ""){
  
  sample <- c(0)
  #Get sample
  if(filter){
    sample <- getFilteredData(scatterer, dim)
  }
  
  else {
    sample <- getSimilarity(scatterer, dim)
  }
  
  #Estimative of Beta parameters
  mean <- mean(sample)
  sd <- sd(sample)
  alpha <- mean ^2 / sd ^2
  beta <- sd^2 / mean
  
  #Plot
  x <- seq( from = 0, to = 0.2, by = 0.001)
  desc <- paste("Gamma(", round(alpha, 3), ", ", round(beta, 3), ")", sep="")
  
  ggplot() + 
    geom_histogram(aes(x = c(sample), y = ..density..)) + xlab("x") +
    geom_line(aes(x = x, y = dgamma(x, shape = alpha, scale = beta), colour = "red"), size = 1.3) +
    scale_color_discrete(name = "Parameters", labels = c(desc)) +
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
}

logit <- function(x){
  return( log(x) / log(1-x) )
}

plotHistogramLogitnorm <- function(scatterer, dim, filter = FALSE, title = ""){
  
  sample <- c(0)
  #Get sample
  if(filter){
    sample <- getFilteredData(scatterer, dim)
  }
  
  else {
    sample <- getSimilarity(scatterer, dim)
  }
  
  #Estimative of Beta parameters
  logit <- logit(sample)
  mean <- mean( logit )
  sd <- sd(logit)
  
  #Plot
  x <- seq( from = 0, to = 1, by = 0.001)
  desc <- paste("LN(", round(mean, 3), ", ", round(sd, 3), "²)", sep="")
  
  ggplot() + 
    geom_histogram(aes(x = c(sample), y = ..density..)) + xlab("x") +
    geom_line(aes(x = x, y = dlogitnorm(x, mu = mean, sigma = sd), colour = "red"), size = 1.3) +
    scale_color_discrete(name = "Parameters", labels = c(desc)) +
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
}

# dpert <- function(x, min, mode, max){
#   
#   alpha <- (4*mode + max - 5*min)/(max - min)
#   beta <- (5*max - min - 4*mode)/(max - min)
#   return( ( (x - min)^(alpha - 1) * (max - x)^(beta - 1) ) / ( beta(alpha, beta) * (max - min) ^ (alpha + beta - 1) ) )
#   
# }

plotHistogramPert <- function(scatterer, dim, filter = FALSE, title = "", min, max){
  
  sample <- c(0)
  #Get sample
  if(filter){
    sample <- getFilteredData(scatterer, dim)
  }
  
  else {
    sample <- getSimilarity(scatterer, dim)
  }
  
  #Estimative of Beta parameters
  #shape <- 3.914062
  shape <- 4
  mode <- ((shape + 2)*mean(sample) - min - max)/shape
  
  #Plot
  x <- seq( from = min, to = max, by = 0.001)
  desc <- paste("PERT(", round(min, 3), ", ", round(mode, 3), ", ", round(max, 3), ")", sep="")
  
  ggplot() + 
    geom_histogram(aes(x = c(sample), y = ..density..)) + xlab("x") +
    geom_line(aes(x = x, y = dpert(x, min = min, mode = mode, max = max), colour = "red"), size = 1.3) +
    scale_color_discrete(name = "Parameters", labels = c(desc)) +
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
}

plotQQPlotBeta <- function(scatterer, dim, filter = FALSE, title = ""){
  
  sample <- c(0)
  #Get sample
  if(filter){
    sample <- getFilteredData(scatterer, dim)
  }
  
  else {
    sample <- getSimilarity(scatterer, dim)
  }
  
  mean <- mean(sample)
  var <- sd(sample) ^ 2
  alpha <- mean * ( mean * (1 - mean) / var - 1 )
  beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
  
  params = list(shape1 = alpha, shape2 = beta)
  
  ggplot(data.frame(sample), aes(sample = sample)) +
    stat_qq(distribution = qbeta, dparams = params) +
    stat_qq_line(distribution = qbeta, dparams = params)
}

plotQQPlotPert <- function(scatterer, dim, filter = FALSE, title = "", min, max){
  
  sample <- c(0)
  #Get sample
  if(filter){
    sample <- getFilteredData(scatterer, dim)
  }
  
  else {
    sample <- getSimilarity(scatterer, dim)
  }
  
  mode <- (6*mean(sample) - min - max)/4
  
  params <- list(min = min, mode = mode, max = max)
  ggplot(data.frame(sample), aes(sample = sample)) +
    stat_qq(distribution = qpert, dparams = params) +
    stat_qq_line(distribution = qpert, dparams = params)
  
}

ksTestBeta <- function(scatterer, dim, filter = FALSE){
  
  sample <- c(0)
  if(filter){
    sample <- getFilteredData(scatterer, dim)
  } else {
    sample <- getGeoDist(scatterer, dim)
  }
  mean <- mean(sample)
  var <- sd(sample) ^ 2
  alpha <- mean * ( mean * (1 - mean) / var - 1 )
  beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
  
  return( ks.test(sample, "pbeta", shape1 = alpha, shape2 = beta))
  
}

alpha_gd <- function(dim, filter  = FALSE){
  data = c()
  if(filter){
    data = getFilteredData("trihedral" , dim)
  } else {
    data = getGeoDist("trihedral", dim)
  }
  return(90*data)
}

helicity_gd <- function(dim){
  data = 1 - sqrt( getGeoDist("left helix", dim) * getGeoDist("right helix", dim) )
  return(45*data)
}

purity_gd <- function(dim, filter = TRUE){
  
  hhhh <- read_data("HHHH", dim)
  hvhv <- read_data("HVHV", dim)
  vvvv <- read_data("VVVV", dim)
  
  hhhv <- read_complex_data("HHHV", dim)
  hhvv <- read_complex_data("HHVV", dim)
  hvvv <- read_complex_data("HVVV", dim)
  
  mod_kennaugh <- c(sqrt( hhhh^2 + hvhv^2 + vvvv^2 + 2*Mod(hhhv)^2 + 2*Mod(hhvv)^2 + 2*Mod(hvvv)^2 ))
  inner_prod <- c( (hhhh + hvhv + vvvv) ) / 2
  
  gd_data <- (2/pi)*acos( inner_prod / mod_kennaugh )
  
  data <- ( 1.5 * gd_data )^2
  
  return( data )
  
}
