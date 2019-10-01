classifier <- function(x){
  if(x[1] <= 0.912 && x[2] <= 0.912){
    return("Rich")
  }
  else if(x[1] > 0.912 && x[2] > 0.912){
    return("Poor")
  }
  else{
    if(x[3] < 0.462 || x[3] > 0.512){
      return("Rich")
    }
    else{
      return("Poor")
    }
  }
}

setwd(wd[5])
x <- array(0, dim = c(1950, 3))
x <- array(0, dim = c(225, 3))

dim <- c(90, 65, 5, 30)
dim <- c(90, 15, 10, 15)

x[,1] <- getGeoDist("left helix", dim)
x[,2] <- getGeoDist("right helix", dim)
x[,3] <- getDistDihedral(dim)

table(apply(x, 1, classifier))

