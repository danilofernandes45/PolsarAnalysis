dim <- c(25, 50, 30, 39) #Canola 43 -> Used

#To work, the directory of this code should be Working Directory
actual_dir <- getwd()
wd <- c(
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3", sep=""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3", sep = "")
)

sample <- array(0, dim = c(50, 39, 5))
for(i in 1:5){
  setwd(wd[i])
  sample[,,i] <- purity_gd(dim)
}

### BEGIN Canola Purity Plot for the GRSL paper (Alejandro, 16-17 March 2020)
PurityCanola <- array(NA, dim=c(prod(dim(sample)), 2))
PurityCanola[,1] <- sample[,,]
PurityCanola[,2] <- rep(1:5, each=prod(dim(sample)[1:2]))
PurityCanola <- data.frame(PurityCanola)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
PurityCanola[,2] <- dates[PurityCanola[,2]]
names(PurityCanola) <- c("Purity", "Date")
PurityCanola$Date <- factor(PurityCanola$Date,
                            levels = dates)

# Plot of Canola Purities
ggplot(PurityCanola, aes(x=Purity, fill=Date)) + 
  geom_density(alpha=.5) +
  labs(x="Canola Purity 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/CanolaPurity.pdf", 
       width = 15, height=8, units="cm")
### END of Canola Purity Plot for the GRSL paper (Alejandro, 16-17 March 2020)


k <- 1

ggplot() +
  geom_histogram(aes(x=log10(c(sample[,,k])), y = ..density..), fill="#FFC840", color = "#FFC840", 
                 alpha=0.6, position = 'identity', bins=nclass.scott(c(sample[,,k]))) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Purity [log10]") + ylab("Density") + xlim(c(-7, 2))

shapiro.test(log10(c(sample[,,1])))
shapiro.test(log10(c(sample[,,2])))
shapiro.test(log10(c(sample[,,3])))
shapiro.test(log10(c(sample[,,4])))
shapiro.test(log10(c(sample[,,5])))

mean <- mean(sample)
var <- sd(sample)^2

alpha <- mean * ( mean * (1 - mean) / var - 1 )
beta <- ( 1 - mean ) * ( mean * ( 1 - mean ) / var - 1)

ks.test(sample, "pbeta", shape1 = alpha, shape2 = beta)

setwd(wd[5])
sample <- getFilteredData("trihedral", dim)
ggplot() +
  geom_histogram(aes(x=c(sample), y = ..density..), fill = "#FFC840", color="#FFC840", 
                 alpha=0.6, position = 'identity', bins=nclass.FD(sample)*2) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Normalized Alpha") + ylab("Density") + xlim(c(0, 1))

### BEGIN Canola Alphas Plot for the GRSL paper (Alejandro, 17 March 2020)

AlphaCanola <- NULL
for(i in 1:5){
  setwd(wd[i])
#  sample <- getFilteredData("trihedral", dim)
  sample <- getGeoDist("trihedral", dim)
  print(i); print(length(sample))
  id.sample <- cbind(sample, rep(i, length(sample)))
  AlphaCanola <- rbind(AlphaCanola, id.sample)
}

AlphaCanola <- data.frame(AlphaCanola)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
AlphaCanola[,2] <- dates[AlphaCanola[,2]]
names(AlphaCanola) <- c("Alpha", "Date")
AlphaCanola$Date <- factor(AlphaCanola$Date,
                            levels = dates)

# Plot of Canola Alphas
ggplot(AlphaCanola, aes(x=Alpha, fill=Date)) + 
  geom_density(alpha=.5) +
  labs(x="Canola Alpha 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) 

ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/CanolaAlpha.pdf", 
       width = 15, height=8, units="cm")

### END of Canola Alphas Plot for the GRSL paper (Alejandro, 17 March 2020)


setwd(wd[1])
sample <- c(helicity_gd(dim) / 45)
ggplot() +
  geom_histogram(aes(x=c(sample), y = ..density..), color="#636F4B", 
                 alpha=0.6, position = 'identity', bins=nclass.FD(sample)/1.1) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Normalized Helicity") + ylab("Density") + xlim(c(0, 1)) + ylim(c(0, 10))

### BEGIN Canola Helicity Plot for the GRSL paper (Alejandro, 17 March 2020)

HelicityCanola <- NULL
for(i in 1:5){
  setwd(wd[i])
  sample <- c(helicity_gd(dim) / 45)
  print(i); print(length(sample))
  id.sample <- cbind(sample, rep(i, length(sample)))
  HelicityCanola <- rbind(HelicityCanola, id.sample)
}

HelicityCanola <- data.frame(HelicityCanola)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
HelicityCanola[,2] <- dates[HelicityCanola[,2]]
names(HelicityCanola) <- c("Helicity", "Date")
HelicityCanola$Date <- factor(HelicityCanola$Date,
                           levels = dates)

# Plot of Canola Helicity
ggplot(HelicityCanola, aes(x=Helicity, fill=Date)) + 
  geom_density(alpha=.5) +
  labs(x="Canola Helicity 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/CanolaHelicity.pdf", 
       width = 15, height=8, units="cm")

### END of Canola Helicity Plot for the GRSL paper (Alejandro, 17 March 2020)
