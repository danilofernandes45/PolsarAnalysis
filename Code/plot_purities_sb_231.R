dim <- c(90, 65, 5, 30) #Sample 231

#To work, the directory of this code should be Working Directory
actual_dir <- getwd()
wd <- c(
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3", sep=""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3", sep = "")
)


# for(i in 1:5){
#   setwd(wd[i])
#   sample <- purity_gd(dim)
#   
#   mean <- mean(sample)
#   var <- sd(sample) ^ 2
#   alpha <- mean * ( mean * (1 - mean) / var - 1 )
#   beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)
#   
#   #Plot
#   x <- seq( from = 0, to = max(sample), by = 0.0001)
#   desc <- paste("Beta(", round(alpha, 3), ", ", round(beta, 3), ")", sep="")
#   
#   p <- ggplot() + 
#     geom_histogram(aes(x = c(sample), y = ..density..), bins = 200) + xlab("x") + 
#     geom_line(aes(x = x, y = dbeta(x, alpha, beta), colour = "red"), size = 1.3) +
#     scale_color_discrete(name = "Parameters", labels = c(desc)) +
#     theme(plot.title = element_text(hjust = 0.5))
#   
#   ggsave(paste("~/PolsarAnalysis/plot", i, ".pdf", sep=""), p, units = "in", height = 10, width = 12)
# }

colors <- c("red", "orange", "yellow", "greenyellow", "green4")

sample <- array(0, dim = c(65, 30, 5))
for(i in 1:5){
  setwd(wd[i])
  sample[,,i] <- purity_gd(dim)
}

### BEGIN SoyBeans Purity Plot for the GRSL paper (Alejandro, 17 March 2020)
PuritySoyBeans <- array(NA, dim=c(prod(dim(sample)), 2))
PuritySoyBeans[,1] <- sample[,,]
PuritySoyBeans[,2] <- rep(1:5, each=prod(dim(sample)[1:2]))
PuritySoyBeans <- data.frame(PuritySoyBeans)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
PuritySoyBeans[,2] <- dates[PuritySoyBeans[,2]]
names(PuritySoyBeans) <- c("Purity", "Date")
PuritySoyBeans$Date <- factor(PuritySoyBeans$Date,
                          levels = dates)

# Plot of SoyBeans Purities
ggplot(PuritySoyBeans, aes(x=Purity, fill=Date)) + 
  geom_density(alpha=.5) +
  labs(x="SoyBeans Purity 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/SoyBeansPurity.pdf", 
       width = 15, height=8, units="cm")
### END of SoyBeans Purity Plot for the GRSL paper (Alejandro, 17 March 2020)

### BEGIN SoyBeans Alpha Plot for the GRSL paper (Danilo, 18 March 2020)

AlphaSoyBeans <- NULL
for(i in 1:5){
  setwd(wd[i])
  sample <- getFilteredData("trihedral", dim)
  print(i); print(length(sample))
  id.sample <- cbind(sample, rep(i, length(sample)))
  AlphaSoyBeans <- rbind(AlphaSoyBeans, id.sample)
}

AlphaSoyBeans <- data.frame(AlphaSoyBeans)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
AlphaSoyBeans[,2] <- dates[AlphaSoyBeans[,2]]
names(AlphaSoyBeans) <- c("Alpha", "Date")
AlphaSoyBeans$Date <- factor(AlphaSoyBeans$Date,
                         levels = dates)

### END SoyBeans Alpha Plot for the GRSL paper (Danilo, 18 March 2020)

### BEGIN SoyBeans Helicity Plot for the GRSL paper (Danilo, 18 March 2020)

HelicitySoyBeans <- NULL
for(i in 1:5){
  setwd(wd[i])
  sample <- c(helicity_gd(dim) / 45)
  print(i); print(length(sample))
  id.sample <- cbind(sample, rep(i, length(sample)))
  HelicitySoyBeans <- rbind(HelicitySoyBeans, id.sample)
}

HelicitySoyBeans <- data.frame(HelicitySoyBeans)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
HelicitySoyBeans[,2] <- dates[HelicitySoyBeans[,2]]
names(HelicitySoyBeans) <- c("Helicity", "Date")
HelicitySoyBeans$Date <- factor(HelicitySoyBeans$Date,
                            levels = dates)

### END SoyBeans Helicity Plot for the GRSL paper (Danilo, 18 March 2020)


library(ggplot2)
library(dplyr)
library(hrbrthemes)
require(ggstance) # Permite gráficos ggplot2 na horizontal


# Build dataset with different distributions
data <- data.frame(
  #day = c( rep("16 May 2016", 1950), rep("09 June 2016", 1950), rep("03 July 2016", 1950), rep("27 July 2016", 1950), rep("20 Aug. 2016", 1950) ),
  day = c( rep("2016-05-16", 1950), rep("2016-06-09", 1950), rep("2016-07-03", 1950), rep("2016-07-27", 1950), rep("2016-08-20", 1950) ),
  value = c( c(sample[,,1]), c(sample[,,2]), c(sample[,,3]), c(sample[,,4]), c(sample[,,5]) )
)

### Com Alejandro, 3 janeiro 2020

df.sample <- data.frame(t1=as.vector(sample[,,1]), 
                        t2=as.vector(sample[,,2]), 
                        t3=as.vector(sample[,,3]), 
                        t4=as.vector(sample[,,4]), 
                        t5=as.vector(sample[,,5]))
range(sample)

ggplot(data=df.sample) +
  geom_histogram(aes(x=t1, y=..density..), fill="red", alpha=.5) +
  geom_histogram(aes(x=t2, y=..density..), fill="green", alpha=.5) +
  scale_x_continuous(trans="log10")

# Represent it
ggplot(data = data) +
  geom_histogram(aes(x=value, y = ..density.., fill=day, group = day), color="#e9ecef", 
                 alpha=0.5, position = 'identity', bins=nclass.FD(data$value)/4) +
  scale_fill_manual(values=colors) +
  theme_ipsum(base_family = "Times New Roman", base_size = 35, axis_title_size = 35)+
  xlab("Purity [log10]") + ylab("Density") +
  geom_boxploth(aes(x=value, y=day, fill=day, group=day), notch=TRUE, width=.2, outlier.size = .5) +
  labs(fill="Day") +
  scale_x_continuous(trans="log10")

#Individual plots

# mean <- mean(log10(sample[,,k]))
# sd <- sd(log10(sample[,,k]))
# 
# x <- seq( from = 0, to = 1, by = 0.001)
# desc <- paste("Normal(", round(mean, 3), ", ", round(sd, 3), "²)", sep="")
k <- 5
ggplot() +
  geom_histogram(aes(x=log10(c(sample[,,k])), y = ..density..), color="#636F4B", fill="#636F4B", 
                 alpha=0.7, position = 'identity', bins=nclass.scott(c(sample[,,k]))) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Purity [log10]") + ylab("Density") + xlim(c(-7, 2))

setwd(wd[5])
sample <- getFilteredData("trihedral", dim)
ggplot() +
  geom_histogram(aes(x=c(sample), y = ..density..), color="#636F4B", fill ="#636F4B", 
                 alpha=0.6, position = 'identity', bins=nclass.FD(sample)) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Normalized Alpha") + ylab("Density") + xlim(c(0, 1))

setwd(wd[1])
sample <- c(helicity_gd(dim) / 45)
ggplot() +
  geom_histogram(aes(x=c(sample), y = ..density..), color="#636F4B", 
                 alpha=0.6, position = 'identity', bins=nclass.FD(sample)/1.1) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Normalized Helicity") + ylab("Density") + xlim(c(0, 1)) + ylim(c(0, 10))

mean <- mean(sample)
var <- sd(sample)^2

alpha <- mean * ( mean * (1 - mean) / var - 1 )
beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)

ks.test(sample, "pbeta", shape1 = alpha, shape2 = beta)

shapiro.test(log10(df.sample$t1))
shapiro.test(log10(df.sample$t2))
shapiro.test(log10(df.sample$t3))
shapiro.test(log10(df.sample$t4))
shapiro.test(log10(df.sample$t5))

ggplot(data) +
  geom_qq(aes(sample=log10(value), group=day, col=day)) +
  stat_qq_line(aes(sample=log10(value), group=day, col=day)) +
  scale_color_manual(values=colors) +
  xlab("Theoretical Quantiles") + ylab("Purity [log10]") + labs(col="Day") +
  theme_ipsum(base_family = "Times New Roman", base_size = 35, axis_title_size = 35)   

##==================================================
#Alpha index (GD to trihedral)
setwd(wd[1])
sample_1 <- getFilteredData("trihedral",dim)
setwd(wd[2])
sample_2 <- getFilteredData("trihedral",dim)
setwd(wd[3])
sample_3 <- getFilteredData("trihedral",dim)
setwd(wd[4])
sample_4 <- getFilteredData("trihedral",dim)
setwd(wd[5])
sample_5 <- getFilteredData("trihedral",dim)

data <- data.frame(
  #day = c( rep("16 May 2016", 1950), rep("09 June 2016", 1950), rep("03 July 2016", 1950), rep("27 July 2016", 1950), rep("20 Aug. 2016", 1950) ),
  day = c( rep("2016-05-16", length(sample_1)), rep("2016-06-09", length(sample_2)), rep("2016-07-03", length(sample_3)), rep("2016-07-27", length(sample_4)), rep("2016-08-20", length(sample_5)) ),
  value = c( c(sample_1), c(sample_2), c(sample_3), c(sample_4), c(sample_5) )
)

ggplot(data = data) +
  geom_histogram(aes(x=value, y = ..density.., fill=day, group = day), color="#e9ecef", 
                 alpha=0.5, position = 'identity', bins=nclass.FD(data$value)/4) +
  scale_fill_manual(values=colors) +
  theme_ipsum(base_family = "Times New Roman", base_size = 35, axis_title_size = 35)+
  xlab("Normalized Alpha") + ylab("Density") +
  geom_boxploth(aes(x=value, y=day, fill=day, group=day), notch=TRUE, width=.2, outlier.size = .5) +
  labs(fill="Day")

#===============================================================================================
k <- 1
x <- seq(from = 0, to = 0.001, by = 0.00000001)
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
  

