dim2 <- c(25, 55, 30, 50)

dim <- array(0, dim = c(4, 72))

count <- 1
#CN43
for(i in 0:7){
  for(j in 0:8){
    dim[,count] <- c(25 + i*7, 7, 30 + j*6, 6)
    count <- count + 1
  }
}

wd <- c(
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3"
)

mean_y <- c(0,0,0,0,0)
for(j in 1:5){
  setwd(wd[j])
  mean_y[j] <- mean(getFilteredData("trihedral", dim2))
} 

tri_mean <- array(0, dim = c(360))

for(i in 1:72){
  for(j in 1:5){
    setwd(wd[j])
    tri_mean[(i-1)*5 + j] <- mean(getFilteredData("trihedral", dim[,i]))
  }
}

day <- rep(c(0, 24, 48, 72, 96), times = 72)
data <- data.frame(day = day, tri_mean = tri_mean)
x <- seq(from = 0, to = 100, by = 0.1)

ggplot() +
  geom_boxplot(aes(x = day, y = tri_mean, group = day), data = data) +
  xlab("t (days)") + ylab("Mean") +
  geom_line(aes(x = x, y = -cf[4]/(cf[1]*x + cf[2]) + cf[3], colour = "red"), size = 1.5) + 
  scale_color_discrete(name = "Functions", labels = c("f(t)")) +
  #geom_smooth(aes(x = day, y = value), data = melt(data, id.vars = "day")) +
  geom_line(aes(x = c(0, 24, 48, 72, 96), y = mean_y), size = 1)
  #stat_summary(fun.y=mean, geom="line", aes(x = data$day, y = data$tri_mean, group=1))  + 
  #stat_summary(fun.y=mean, geom="point", aes(x = data$day, y = data$tri_mean))

cf <- c(0.00015508459132193003, 18.525512810907223, 120.88047266867858, 2235.550261911148)
#MSE 0.0025592296514572533

cf <- c(0.00038935410044675336, 5.747012689008474, 15.216333563714272, 86.2632963937341)
# MSE 0.0025581070853693978
