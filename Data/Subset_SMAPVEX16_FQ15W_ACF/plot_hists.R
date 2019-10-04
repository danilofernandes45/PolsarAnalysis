wd <- c(
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/01_Subset_16_May_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/02_Subset_09_June_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/03_Subset_03_July_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/04_Subset_27_July_2016/T3",
  "~/PolsarAnalysis/Data/Subset_SMAPVEX16_FQ15W_ACF/05_Subset_20_Aug_2016/T3"
)

dim <- array(0, dim = c(10, 4))
dim[1,] <- c(25, 55, 30, 50) #CN 43
dim[2,] <- c(90, 65, 5, 30) #SB 231
dim[3,] <- c(100, 65, 50, 30) #SB 232
dim[4,] <- c(160, 65, 245, 35) #WT 225
dim[5,] <- c(180, 61, 290, 45) #CN 224
dim[6,] <- c(115, 65, 360, 30) #SB 101
dim[7,] <- c(425, 65, 315, 35) #OT 102
dim[8,] <- c(500, 65, 310, 35) #OT 103
dim[9,] <- c(444, 65, 370, 35) #WT 105
dim[10,] <- c(515, 65, 360, 35) #WT 104

dest <- c("Canola_43", "Soybeans_231", "Soybeans_232", "Wheat_225", "Canola_224",
          "Soybeans_101", "Oats_102", "Oats_103", "Wheat_105", "Wheat_104")

for(i in 1:5){
  setwd(wd[i])
  for (j in 1:10){
    plotHistogramBeta("trihedral", dim[j,], filter = TRUE)
    ggsave(paste("~/PolsarAnalysis/Figures/Report_19_09/Histograms/", i, "th_observation/", dest[j], "/histogram_trihedral_", i, ".pdf", sep = ""), width = 12, height = 8)
    plotHistogramBeta("random volume", dim[j,], filter = TRUE)
    ggsave(paste("~/PolsarAnalysis/Figures/Report_19_09/Histograms/", i, "th_observation/", dest[j], "/histogram_random_volume_", i, ".pdf", sep = ""), width = 12, height = 8)
  }
}
