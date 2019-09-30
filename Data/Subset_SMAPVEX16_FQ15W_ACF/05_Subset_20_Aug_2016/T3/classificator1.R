classificator <- function(x){
  if(x[1] <= 0.912 && x[2] <= 0.912){
    return("Vegetation")
  }
  if(x[1] > 0.912 && x[2] > 0.912){
    return("Poor vegetation")
  }
  else{
    if(x[3] < 0.462 || x[3] > 0.512){
      return("Vegetation")
    }
    else{
      return("Poor vegetation")
    }
  }
}

dim <- c(90, 65, 5, 30)
dim3 <- c(90, 15, 10, 15)

dim <- c(100, 65, 50, 30)

dim <- c(115, 65, 360, 30)

setwd(wd[5])
data <- array(0, dim = c(1950, 3))
data[,1] <- c(getGeoDist("left helix", dim))
data[,2] <- c(getGeoDist("right helix", dim))
data[,3] <- c(getDistDihedral(dim))

table( apply(data, 1, classificator))
