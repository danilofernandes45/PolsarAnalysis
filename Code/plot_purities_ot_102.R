dim <- c(500, 65, 310, 30) #Oats 102 [35 -> 30]

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
                 alpha=0.8, position = 'identity', bins=nclass.scott(c(sample[,,k]))) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Purity [log10]") + ylab("Density") + xlim(c(-7, 2))

shapiro.test(log10(c(sample[,,1])))
shapiro.test(log10(c(sample[,,2])))
shapiro.test(log10(c(sample[,,3])))
shapiro.test(log10(c(sample[,,4])))
shapiro.test(log10(c(sample[,,5])))

