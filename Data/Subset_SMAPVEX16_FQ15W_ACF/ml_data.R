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

library(C50)
library(printr)

train.indices <- sample(1:nrow(data), nrow(data)//3)
data.train <- data[train.indices, ]
data.test <- data[-train.indices, ]

model <- C5.0(class ~., data=data.train)
results <- predict(object=model, newdata=data.test, type="class")
table(results, iris.test$class)
plot(model)
