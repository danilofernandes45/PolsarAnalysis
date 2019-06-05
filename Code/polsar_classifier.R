library(dgof)
pbeta_nstd <- function(q, min, max, shape1, shape2){
  qz <- (q - min) / (max - min)
  return( pbeta(qz, shape1 = shape1, shape2 = shape2) )
}

dbeta_nstd <- function(x, min, max, shape1, shape2){
  z <- (x - min) / (max - min)
  return(dbeta(z, shape1 = shape1, shape2 = shape2) / (max - min) )
}

#Classifier based on similarities to Dihedral
classifier <- function(x){ # x is the similarities to Dihedral
  
  min_forest <- 0
  max_forest <- 1
  alpha_forest <- 5.380
  beta_forest <- 36.870
  
  min_soil <- 0.009
  max_soil <- 0.07
  alpha_soil <- 1.327
  beta_soil <- 4.672
  
  pvalue_forest <- ks.test(x, "pbeta_nstd", min_forest, max_forest, alpha_forest, beta_forest)$p.value
  pvalue_soil <- ks.test(x, "pbeta_nstd", min_soil, max_soil, alpha_soil, beta_soil)$p.value
  
  if( ( pvalue_forest > 0.05 && pvalue_soil > 0.05 ) || ( pvalue_forest < 0.05 && pvalue_soil < 0.05 ) ){
    return("Unknown")
  }
  if( pvalue_forest > 0.05 ){
    return("Forest")
  }
  if( pvalue_soil > 0.05 ){
    return("Bare soil")
  }
  
}
