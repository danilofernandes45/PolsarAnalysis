dim <- c(90, 65, 5, 30)

dim <- array(0, dim = c(4, 45))

count <- 1
for(i in 0:8){
  for(j in 0:4){
    dim[,count] <- c(90 + i*7, 7, 5 + j*6, 6)
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

tri_mean <- array(0, dim = c(225))

for(i in 1:45){
  for(j in 1:5){
    setwd(wd[j])
    tri_mean[(i-1)*5 + j] <- mean(getFilteredData("trihedral", dim[,i]))
  }
}

day <- rep(c(0, 24, 48, 72, 96), times = 45)
data <- data.frame(day = day, tri_mean = tri_mean)
x <- seq(from = 0, to = 100, by = 0.1)

# ggplot(aes(x = day, y = value), data = melt(data, id.vars = "day")) +
#   geom_point() + geom_smooth()

cf <- c(2.415, 67.565, 0.280, 4.741)
#MSE = 0.0008371997809741466


ggplot() +
  #geom_smooth(aes(x = day, y = value), data = melt(data, id.vars = "day")) +
  geom_boxplot(aes(x = day, y = tri_mean, group = day), data = data) +
  xlab("t (days)") + ylab("Mean") +
  geom_line(aes(x = x, y = -cf[4]/(cf[1]*x + cf[2]) + cf[3], colour = "red"), size = 1.5) + 
  scale_color_discrete(name = "Functions", labels = c("f(t)")) +
  stat_summary(fun.y=mean, geom="line", aes(x = data$day, y = data$tri_mean, group=1))  + 
  stat_summary(fun.y=mean, geom="point", aes(x = data$day, y = data$tri_mean))

