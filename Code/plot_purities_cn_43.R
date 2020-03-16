dim <- c(25, 55, 30, 50) #Canola 43

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

### BEGIN Plot for the GRSL paper (Alejandro, 16 March 2020)
require(reshape2)
PurityCanola <- sample

# Convert the 3D matrix into a 2D structure
dim(PurityCanola) <- c(dim(sample)[1]*dim(sample)[2],5)

# melt and clean the 2D structure
melt.PurityCanola <- melt(PurityCanola)
melt.PurityCanola <- melt.PurityCanola[,-1]
names(melt.PurityCanola) <- c("Date", "Purity")
melt.PurityCanola$Date <- as.factor(melt.PurityCanola$Date)

# Plot of Canola Purities
ggplot(melt.PurityCanola, aes(x=Purity, fill=Date)) + 
  geom_density(alpha=0.25) + 
  labs(x="Canola Purity", y="Density") +
  theme_ipsum(base_family = "Times New Roman", base_size = 20, axis_title_size = 20)

ggplot(melt.PurityCanola, aes(x=Purity, fill=Date)) + geom_boxplot(alpha=0.25)
### END Plot for the GRSL paper (Alejandro, 16 March 2020)


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

setwd(wd[1])
sample <- c(helicity_gd(dim) / 45)
ggplot() +
  geom_histogram(aes(x=c(sample), y = ..density..), color="#636F4B", 
                 alpha=0.6, position = 'identity', bins=nclass.FD(sample)/1.1) +
  theme_ipsum(base_family = "Times New Roman", base_size = 70, axis_title_size = 70)+
  xlab("Normalized Helicity") + ylab("Density") + xlim(c(0, 1)) + ylim(c(0, 10))
