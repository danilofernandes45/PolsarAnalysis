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

  return( (2/pi)*acos( numerator / denominator ) )
}

dim <- c(90, 65, 5, 30)

dim3 <- c(110, 15, 10, 15)

setwd(wd[1])
sample1 <- getDistDihedral(dim)
setwd(wd[5])
sample5 <- getDistDihedral(dim)

ggplot() + 
  geom_histogram(aes(x = c(sample5), y = ..density..), fill = "green", alpha = 0.5, bins = 45) + xlab("x") +
  geom_histogram(aes(x = c(sample1), y = ..density..), fill = "red", alpha = 0.5, bins = 45)
