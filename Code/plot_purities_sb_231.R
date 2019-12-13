dim <- c(90, 65, 5, 30) #Sample 231

wd <- c(
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3"
)

for(i in 1:5){
  setwd(wd[i])
  sample <- purity_gd(dim)
  
  mean <- mean(sample)
  var <- sd(sample) ^ 2
  alpha <- mean * ( mean * (1 - mean) / var - 1 )
  beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
  
  #Plot
  x <- seq( from = 0, to = max(sample), by = 0.0001)
  desc <- paste("Beta(", round(alpha, 3), ", ", round(beta, 3), ")", sep="")
  
  p <- ggplot() + 
    geom_histogram(aes(x = c(sample), y = ..density..), bins = 45) + xlab("x") + 
    geom_line(aes(x = x, y = dbeta(x, alpha, beta), colour = "red"), size = 1.3) +
    scale_color_discrete(name = "Parameters", labels = c(desc)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("~/PolsarAnalysis/plot", i, ".pdf", sep=""), p, units = "in", height = 10, width = 12)
}

