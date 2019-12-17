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
    geom_histogram(aes(x = c(sample), y = ..density..), bins = 200) + xlab("x") + 
    geom_line(aes(x = x, y = dbeta(x, alpha, beta), colour = "red"), size = 1.3) +
    scale_color_discrete(name = "Parameters", labels = c(desc)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("~/PolsarAnalysis/plot", i, ".pdf", sep=""), p, units = "in", height = 10, width = 12)
}

colors <- c("red", "orange", "yellow", "yellowgreen", "green")

sample <- array(0, dim = c(65, 30, 5))
for(i in 1:5){
  setwd(wd[i])
  sample[,,i] <- purity_gd(dim)
}

library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Build dataset with different distributions
data <- data.frame(
  day = c( rep("0", 1950), rep("24", 1950), rep("48", 1950), rep("72", 1950), rep("96", 1950) ),
  value = c( c(sample[,,1]), c(sample[,,2]), c(sample[,,3]), c(sample[,,4]), c(sample[,,5]) )
)

# Represent it
ggplot(data = data) +
  geom_histogram(aes(x=value, y = ..density.., fill=day, group = day), color="#e9ecef", alpha=0.5, position = 'identity', bins = 200) +
  scale_fill_manual(values=colors) +
  theme_ipsum() + xlab("Purity") + ylab("Density") +
  labs(fill="Day")

#===============================================================================================
k <- 1
x <- seq(from = 0, to = 0.001, by = 0.000001)
emp_cdf <- ecdf(sample[,,k])

mean <- mean(sample[,,k])
var <- sd(sample[,,k]) ^ 2
alpha <- mean * ( mean * (1 - mean) / var - 1 )
beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)

ggplot() +
  geom_line(aes(x = x, y = pbeta(x, alpha, beta), colour = "red")) + ylab("probability") +
  geom_line(aes(x = x, y = emp_cdf(x) ))

#===============================================================================================
#Sample Day 0

resample <- sample[10:15,10:20,1] #Pvalue = 0.1125
resample <- sample[30:40,10:15,1] #Pvalue = 0.1140
resample <- sample[20:25,10:20,1] #Pvalue = 0.1049

alpha <- 0.625
beta <- 2325.476

ks.test(resample, "pbeta", shape1 = alpha, shape2 = beta)

#Sample Day 24

resample <- sample[10:15,10:20,2] #Pvalue = 0.1039
resample <- sample[30:40,10:15,2] #Pvalue = 0.2175
resample <- sample[20:25,10:20,2] #Pvalue = 0.2123

alpha <- 0.917
beta <- 301.784

ks.test(resample, "pbeta", shape1 = alpha, shape2 = beta)

#Sample Day 48

resample <- sample[1:5,10:15,3] #Pvalue = 0.1497
resample <- sample[1:10,15:20,3] #Pvalue = 0.1089
resample <- sample[15:20,20:25,3] #Pvalue = 0.1084

alpha <- 0.777
beta <- 22.773

ks.test(resample, "pbeta", shape1 = alpha, shape2 = beta)
