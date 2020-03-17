dim <- c(515, 65, 360, 30) #Wheat 104 [35 -> 30]

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

### BEGIN Wheat Purity Plot for the GRSL paper (Alejandro, 17 March 2020)
PurityWheat <- array(NA, dim=c(prod(dim(sample)), 2))
PurityWheat[,1] <- sample[,,]
PurityWheat[,2] <- rep(1:5, each=prod(dim(sample)[1:2]))
PurityWheat <- data.frame(PurityWheat)
dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
PurityWheat[,2] <- dates[PurityWheat[,2]]
names(PurityWheat) <- c("Purity", "Date")
PurityWheat$Date <- factor(PurityWheat$Date,
                          levels = dates)

# Plot of Wheat Purities
ggplot(PurityWheat, aes(x=Purity, fill=Date)) + 
  geom_density(alpha=.5) +
  labs(x="Wheat Purity 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/WheatPurity.pdf", 
       width = 15, height=8, units="cm")
### END of Wheat Purity Plot for the GRSL paper (Alejandro, 17 March 2020)

k <- 5

ggplot() +
  geom_histogram(aes(x=log10(c(sample[,,k])), y = ..density..), fill="#F5DEB3", color = "#F5DEB3", 
                 alpha=0.6, position = 'identity', bins=nclass.scott(c(sample[,,k]))) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Purity [log10]") + ylab("Density") + xlim(c(-7, 2))

shapiro.test(log10(c(sample[,,1])))
shapiro.test(log10(c(sample[,,2])))
shapiro.test(log10(c(sample[,,3])))
shapiro.test(log10(c(sample[,,4])))
shapiro.test(log10(c(sample[,,5])))
