#To work, the directory of this code should be Working Directory
actual_dir <- getwd()
wd <- c(
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3", sep=""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3", sep = "")
)

#Set a dim
#dim <- dim_sb231 <- c(90, 65, 5, 30)
#Analysis of alpha index
compute_statistic <- function(i, j){
  setwd(wd[i])
  sample <- getFilteredData("trihedral", dim)
  n1 <- length(sample)
  
  mean <- mean(sample)
  var <- sd(sample)^2
  
  a1 <- mean * ( mean * (1 - mean) / var - 1 )
  b1 <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
  
  setwd(wd[j])
  sample <- getFilteredData("trihedral", dim)
  n2 <- length(sample)
  
  mean <- mean(sample)
  var <- sd(sample)^2
  
  a2 <- mean * ( mean * (1 - mean) / var - 1 )
  b2 <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
  
  a <- ( a1 + a2 ) / 2
  b <- ( b1 + b2 ) / 2
  
  s <- ( 8 * n1 * n2 / (n1 + n2) ) * ( 1 - beta(a, b) / sqrt( beta(a1, b1) * beta(a2, b2) ) )
  
  return(s)
  
}

#H_0 : (a1, b1) = (a2, b2)
#H_a : (a1, b1) != (a2, b2)
pchisq( compute_statistic(1, 2), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(1, 3), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(1, 4), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(1, 5), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(2, 3), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(2, 4), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(2, 5), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(3, 4), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(3, 5), df = 2, lower.tail = FALSE )
pchisq( compute_statistic(4, 5), df = 2, lower.tail = FALSE )
