dim <- c(425, 65, 315, 30) #Oats 102 [Used]

#To work, the directory of this code should be Working Directory
actual_dir <- getwd()
wd <- c(
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3", sep=""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3", sep = "")
)

sample <- array(0, dim = c(65, 30, 5))
for(i in 1:5){
  setwd(wd[i])
  sample[,,i] <- purity_gd(dim)
}

k <- 5
ggplot() +
  geom_histogram(aes(x=log10(c(sample[,,k])), y = ..density..), fill="#EFE6D8", color = "#EFE6D8", 
                 alpha=0.8, position = 'identity', bins=nclass.FD(c(sample[,,k]))) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Purity [log10]") + ylab("Density") + xlim(c(-0.75, 0.75))

shapiro.test(log10(c(sample[,,1])))
shapiro.test(log10(c(sample[,,2])))
shapiro.test(log10(c(sample[,,3])))
shapiro.test(log10(c(sample[,,4])))
shapiro.test(log10(c(sample[,,5])))

### BEGIN Oats Purity Plot for the GRSL paper (Alejandro, 17 March 2020)
PurityOats <- array(NA, dim=c(prod(dim(sample)), 2))
PurityOats[,1] <- sample[,,]
PurityOats[,2] <- rep(1:5, each=prod(dim(sample)[1:2]))
PurityOats <- data.frame(PurityOats)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
PurityOats[,2] <- dates[PurityOats[,2]]
names(PurityOats) <- c("Purity", "Date")
PurityOats$Date <- factor(PurityOats$Date,
                            levels = dates)

# Plot of Oats Purities
ggplot(PurityOats, aes(x=Purity, fill=Date)) + 
  geom_density(alpha=.5) +
  labs(x="Oats Purity 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/OatsPurity.pdf", 
       width = 15, height=8, units="cm")
### END of Oats Purity Plot for the GRSL paper (Alejandro, 17 March 2020)

### BEGIN Oats Alpha Plot for the GRSL paper (Danilo, 18 March 2020)

AlphaOats <- NULL
for(i in 1:5){
  setwd(wd[i])
  sample <- getFilteredData("trihedral", dim)
  print(i); print(length(sample))
  id.sample <- cbind(sample, rep(i, length(sample)))
  AlphaOats <- rbind(AlphaOats, id.sample)
}

AlphaOats <- data.frame(AlphaOats)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
AlphaOats[,2] <- dates[AlphaOats[,2]]
names(AlphaOats) <- c("Alpha", "Date")
AlphaOats$Date <- factor(AlphaOats$Date,
                           levels = dates)

### END Oats Alpha Plot for the GRSL paper (Danilo, 18 March 2020)

### BEGIN Oats Helicity Plot for the GRSL paper (Danilo, 18 March 2020)

HelicityOats <- NULL
for(i in 1:5){
  setwd(wd[i])
  sample <- c(helicity_gd(dim) / 45)
  print(i); print(length(sample))
  id.sample <- cbind(sample, rep(i, length(sample)))
  HelicityOats <- rbind(HelicityOats, id.sample)
}

HelicityOats <- data.frame(HelicityOats)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
HelicityOats[,2] <- dates[HelicityOats[,2]]
names(HelicityOats) <- c("Helicity", "Date")
HelicityOats$Date <- factor(HelicityOats$Date,
                              levels = dates)

### END Oats Helicity Plot for the GRSL paper (Danilo, 18 March 2020)
