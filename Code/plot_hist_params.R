#Samples' location
dim_sb231 <- c(90, 65, 5, 30)
dim_cn43 <- c(25, 50, 30, 39)
dim_ot102 <- c(425, 65, 315, 30)
dim_wt104 <- c(515, 65, 360, 30)

#Samples' color

color_sb231 <- "#636F4B"
color_cn43 <- "#FFC840"
color_ot_102 <- "#EFE6D8"
color_wt_104 <- "#F5DEB3"

#Dataset path
actual_dir <- getwd()
wd <- c(
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3", sep=""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3", sep = "")
)

#Purity from a sample over time
get_purity_sample <- function(dim){
  sample <- array(0, dim = c(65, 30, 5))
  for(i in 1:5){
    setwd(wd[i])
    sample[,,i] <- purity_gd(dim)
  }
  return(sample)
}

#Plot purity
plot_purity <- function(dim, k, color){
  setwd(wd[k])
  sample <- purity_gd(dim)
  ggplot() +
    geom_histogram(aes(x=log10(sample), y = ..density..), fill=color, color = color, 
                   alpha=0.8, position = 'identity', bins=nclass.FD(sample)) +
    theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
    xlab("Purity [log10]") + ylab("Density") + xlim(c(-0.75, 0.75))
}

#Plot normalized alpha (distance to trihedral from most similar pixels)
plot_norm_alpha <- function(dim, k, color){
  setwd(wd[k])
  sample <- getFilteredData("trihedral", dim)
  ggplot() +
    geom_histogram(aes(x=sample, y = ..density..), fill=color, color = color, 
                   alpha=0.8, position = 'identity', bins=nclass.FD(sample)) +
    theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
    xlab("Normalized Alpha") + ylab("Density") + xlim(c(0, 1)) + ylim(c(0, 4))
}

#Compute parameters for normalized alpha from a sample at the time
compute_params_alpha <- function(dim, k){
  
  setwd(wd[k])
  sample <- getFilteredData("trihedral", dim)
  
  mean <- mean(sample)
  var <- sd(sample)^2
  
  alpha <- mean * ( mean * (1 - mean) / var - 1 )
  beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
  
  return(c(alpha, beta))
}
<<<<<<< HEAD

ksTestAlpha <- function(dim, k){
  setwd(wd[k])
  sample <- getFilteredData("trihedral", dim)
  mean <- mean(sample)
  var <- sd(sample) ^ 2
  alpha <- mean * ( mean * (1 - mean) / var - 1 )
  beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
  
  return( c( length(sample),ks.test(sample, "pbeta", shape1 = alpha, shape2 = beta)$p.value))
}
=======
>>>>>>> 91620581427de17ca8eae3fb3bf8664359e2884c
