entropy <- function(vector){
  probs <- sapply(vector, function(x) length(which(x == vector))/length(vector))
  return(-sum(probs * log2(probs)))
}
  

wd <- c(
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3"
)

dim1 <- c(90, 65, 5, 30) #Sample 231
 
scatterers <- c("-1/4-wave", "+1/4-wave", "cylinder", "dihedral", "dipole", "left helix",
                "narrow dihedral", "random volume", "right helix", "trihedral")

sample <- 1:5

data <- data.frame(0,0,0,0,0,0,0,0,0,0,0)
names(data) <- c(scatterers, "class")
tmp <- array(0, dim = c(1950, 11))
for(j in 1:5){
  setwd(wd[j])
  for(k in 1:10){
    tmp[,k] <- c(getGeoDist(scatterers[k], dim1))
  }
  tmp[,11] <- sample[j]
  tmp_data <- data.frame(tmp)
  names(tmp_data) <- c(scatterers, "class")
  data <- rbind(data, tmp_data)
}

data <- data[-1,]
data$class <- as.factor(data$class)

#===============================================================================
#Decision Trees

library(party)
library(rpart)
library(rpart.plot)

data <- data[-which(data$class != 1 & data$class != 5),]

set.seed(123)
train_indices <- sample(1:nrow(data), 2*nrow(data)%/%3)
data_train <- data[train_indices, ]
data_test <- data[-train_indices, ]
rownames(data_train) <- NULL
rownames(data_test) <- NULL

#1
tree = ctree(class ~ ., data = data_train)
plot(tree, main="Conditional Inference Tree for Data")
table(predict(tree, data_test), data_test$class)

#2
tree = rpart(class ~ ., data = data_train, method = "class")
rpart.plot(tree)
predictions <- array(1, nrow(data_test))
predictions[ which(predict(tree, data_test)[,1] < predict(tree, data_test)[,5]) ] <- 5
table(predictions, data_test$class)

#=================================================================================
#Mutual Information

mutual_info <- array(0, dim = c(10))
for(i in 1:10){
  mutual_info[i] <- entropy(data[,i])
  for(j in 1:5){
    mutual_info[i] <- mutual_info[i] - 0.2*entropy(data[ which(data$class == j), i])
  }
}
#MI are equals
#=================================================================================
#Covariance

coef_var <- array(0, dim = c(10))

for(i in 1:10){
  coef_var[i] <- cov(data[,i], data$class) / (sd(data[,i]) * sd(data$class))
}

cov <- data.frame(sct = scatterers, cov = covariance) 
#Greatests covariances -> Trihedral, Left helix, right helix

#=================================================================================
#Plot 3d

library(plotly)
plot_ly(x = data[which(data$class == 1), 6], y = data[which(data$class == 1), 9], z = data[which(data$class == 1), 10])

data15 <- data[-which(data$class != 1 & data$class != 5),]
plot_ly(data = data15, x = ~`random volume`, y = ~`right helix`, z = ~trihedral, color = ~class)
