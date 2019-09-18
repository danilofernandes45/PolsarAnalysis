dim1 <- c(90, 65, 5, 30) #Sample 231
dim2 <- c(100, 65, 50, 30) #Sample 232

dim <- array(0, dim = c(4, 45))

count <- 1
#SB231
#MSE = 0.000843668569069063
for(i in 0:8){
  for(j in 0:4){
    dim[,count] <- c(90 + i*7, 7, 5 + j*6, 6)
    count <- count + 1
  }
}
#SB232
#MSE = 0.0008219669350049282
for(i in 0:8){
  for(j in 0:4){
    dim[,count] <- c(100 + i*7, 7, 50 + j*6, 6)
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
  mean_y[j] <- mean(getFilteredData("trihedral", dim1))
} 

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

#1
cf <- c(2.415, 67.565, 0.276, 4.741) #pvalue = 0.166 e 0.496
#2
cf <- c(3.060, 18.586, 0.265, 1.042) #RUIM

#RESIDUALS ANALYSIS
#-----------------------------------------------
#NORMALITY TEST #===============================================
residuals <- data$tri_mean - ( -cf[4]/(cf[1]*data$day + cf[2]) + cf[3] )
#KS TEST
ks.test(residuals, "pnorm", mean = 0, sd = sd(residuals))
#SB231 => PVALUE = 0.2169
#SB232 => PVALUE = 0.0775

#HOMOSCEDASTICITY TEST
#===============================================
data_residuals <- data.frame(day = day, residuals = residuals)
sd_res <- c(0,0,0,0,0)
for(i in 1:5){
  sd_res[i] <- sd( data_residuals$residuals[ which(data_residuals$day == (i-1)*24) ] )
}

ggplot() +
  geom_point(aes(x = day, y = residuals), data = data_residuals) +
  geom_line(aes(x = x, y = 0), color = "red")

#LACK OF FIT TEST
#================================================

x_value <- c(0, 24, 48, 72, 96)
#mean_y <- tapply(data$tri_mean, data$day, mean)
y <- -cf[4] / (cf[1] * x_value + cf[2]) + cf[3]

lof_error <- sum(45*(mean_y - y)^2)
qme_lof <- lof_error / 3 #(m - p - 1)

var_y <- tapply(data$tri_mean, data$day, sd)^2
pure_error <- sum(45*var_y)
qme_pure <- pure_error / 220

F0 <- qme_lof/qme_pure
pf(F0, 3, 220, lower.tail = FALSE)

#NOTE: AMBOS ACEITOS AO NÍVEL DE SIGNIFICÂNCIA DE 0.25

#================================================

ggplot() +
  #geom_smooth(aes(x = day, y = value), data = melt(data, id.vars = "day")) +
  geom_boxplot(aes(x = day, y = tri_mean, group = day), data = data) +
  xlab("t (days)") + ylab("Mean") +
  geom_line(aes(x = x, y = -cf[4]/(cf[1]*x + cf[2]) + cf[3], colour = "red"), size = 1.5) + 
  scale_color_discrete(name = "Functions", labels = c("f(t)")) +
  geom_line(aes(x = x_value, y = mean_y), size = 1)
  #stat_summary(fun.y=mean, geom="line", aes(x = data$day, y = data$tri_mean, group=1))  + 
  #stat_summary(fun.y=mean, geom="point", aes(x = data$day, y = data$tri_mean))

