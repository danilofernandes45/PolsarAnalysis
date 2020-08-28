dim <- array(dim = c(4,4))
dim[1,] <- c(25, 50, 30, 39) #Canola 43 [Used]
dim[2,] <- c(425, 65, 315, 30) #Oats 102 [Used]
dim[3,] <- c(90, 65, 5, 30) #Soybeans 231 [Used]
dim[4,] <- c(515, 65, 360, 30) #Wheat 104 [Used]

#To work, the directory of this code should be Working Directory
actual_dir <- getwd()
wd <- c(
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3", sep=""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3", sep = ""),
  paste(actual_dir, "/../Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3", sep = "")
)

dates <- c("16 May", "9 June", "3 July", "27 July", "20 August")
crop <- c("Canola", "Oats", "Soybeans", "Wheat")
index <- c("Alpha", "Helicity", "Purity")

sample <- array(dim = c(4*3*5*1950, 4))
begin <- 1
end <- 1950

for(c in 1:4){
  for(i in 1:3){
    for(d in 1:5){
      
      setwd(wd[d])
      if(index[i] == "Alpha"){
        sample[begin:end, 1] <- c( getGeoDist("trihedral", dim[c,]) )
      }
      else if(index[i] == "Helicity"){
        sample[begin:end, 1] <- c( helicity_gd(dim[c,]) / 45 )
      }
      else if(index[i] == "Purity"){
        sample[begin:end, 1] <- c( purity_gd(dim[c,]) )
      }
      sample[begin:end, 2] <- dates[d]
      sample[begin:end, 3] <- crop[c]
      sample[begin:end, 4] <- index[i]
        
      begin <- begin + 1950
      end <- end + 1950
    }
  }
}

sample <- data.frame(sample)
names(sample) <- c("Observation", "Date", "Crop", "Index")
setwd(paste(actual_dir, "../Data/ACFrery/", sep = ""))
save(sample, file = "Indices_Sample.RData")
