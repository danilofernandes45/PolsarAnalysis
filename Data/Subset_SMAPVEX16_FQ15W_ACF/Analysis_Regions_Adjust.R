sb_tri_alpha <- data.frame(
  SB101 = c(2.331, 3.95, 3.81, 4.314, 4.798), #SOYBEANS 101
  SB231 = c(3.109, 4.124, 3.779, 4.672, 4.864), #SOYBEANS 231
  SB232 = c(3.039, 4.752, 4.553, 4.217, 4.02) #SOYBEANS 232
)

sb_tri_beta <- data.frame(
  SB101 = c(10.086, 13.849, 11.209, 12.227, 13.957), #SOYBEANS 101
  SB231 = c(11.554, 13.649, 11.216, 13.837, 13.805), #SOYBEANS 231
  SB232 = c(11.602, 15.506, 13.68, 12.532, 11.28) #SOYBEANS 232
)

sb_tri_mean <- sb_tri_alpha / (sb_tri_alpha + sb_tri_beta)
sb_tri_mean$day <- c(1, 25, 49, 73, 97)

library(reshape2)
data <- melt(sb_tri_mean, id.vars = "day")
data <- data[2:15,]

exp_data <- data
exp_data$value <- exp(exp_data$value)

pt_data <- data
pt_data$day <- log(pt_data$day)
pt_data$value <- log(pt_data$value)

alpha <- cov(pt_data$value, pt_data$day) / sd(pt_data$day)^2
beta <- mean(pt_data$value) - alpha * mean(pt_data$day)
beta <- exp(beta)

x <- seq(from = 1, to = 100, by = 0.1)

cf <- c(-1.2499837190720555, -33.711218973385584, 0.27316860724079817, -2.3361746063775404)
#Erro quadrático médio => 3.033815285074885e-05

library(ggplot2)
#Analysis per sample
ggplot() +
  geom_point(aes(x = day, y = value, group = variable, color = variable), data = data) +
  xlab("Sample") + ylab("Proportion value") +
  #geom_line(aes(x = x, y = -2.7/(0.7*(x+45)) + 0.29 , colour = "blue")) +
  geom_line(aes(x = x, y = -cf[4]/(cf[1]*x + cf[2]) + cf[3], colour = "red"))

ggplot() +
  geom_line(aes(x = x, y = beta * x ^ alpha, colour = "red"))
