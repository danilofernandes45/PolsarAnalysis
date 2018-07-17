library("png")

read_file <- function(file, nrow, ncol) {

  return( matrix(
	    readBin(file, double(), n = nrow * ncol, size = 4, endian = "little"), 
            nrow = nrow, ncol = ncol, byrow = TRUE) )

}

fill_matrix <- function(file_HHHH, file_HVHV, file_VVVV, nrow, ncol) {
  
  amplitude_matrix <- array(0, dim = c(nrow, ncol, 3))
  amplitude_matrix[,,1] <- read_file(file_HHHH, nrow, ncol)
  amplitude_matrix[,,2] <- read_file(file_HVHV, nrow, ncol)
  amplitude_matrix[,,3] <- read_file(file_VVVV, nrow, ncol)
  
  return(amplitude_matrix)
  
}

pauli_decomposition <- function(matrix, nrow, ncol){
  
  pauli_matrix <- array(0, dim = c(nrow, ncol, 3))
  
  pauli_matrix[,,1] <- matrix[,,1] + matrix[,,3]
  pauli_matrix[,,2] <- abs( matrix[,,3] - matrix[,,1] )
  pauli_matrix[,,3] <- 2 * matrix[,,2]
  
  return(pauli_matrix)
  
}

equalize <- function(data, nrow, ncol){
  
  data[,,1] <- matrix(ecdf(data[,,1])(data[,,1]), nrow = nrow, ncol = ncol)
  
  data[,,2] <- matrix(ecdf(data[,,2])(data[,,2]), nrow = nrow, ncol = ncol)
  
  data[,,3] <- matrix(ecdf(data[,,3])(data[,,3]), nrow = nrow, ncol = ncol)  
  
  return(data)
}

generate_image <- function(file_HHHH, file_HVHV, file_VVVV, nrow , ncol, tag){
  
  amplitude_matrix <- fill_matrix(file_HHHH, file_HVHV, file_VVVV, nrow , ncol)
  
  #Close files
  close(file_HHHH)
  close(file_HVHV)
  close(file_VVVV)
  #Remove file from RAM memory
  rm(file_HHHH, file_HVHV, file_VVVV)
  
  #Direct projection of z on colors' space
  equalized_matrix <- equalize(amplitude_matrix, nrow, ncol)
  
  writePNG(equalized_matrix, target = paste("../Images/Result_Analysis_07_18/", tag, "_image.png", sep=""))
  
  rm(equalized_matrix)
  
  #Pauli decomposition
  pauli_matrix <- pauli_decomposition(amplitude_matrix, nrow, ncol)
  
  rm(amplitude_matrix)
  
  equalized_matrix <- equalize(pauli_matrix, nrow, ncol)
  
  rm(pauli_matrix)
  
  writePNG(equalized_matrix, target = paste("../Images/Result_Analysis_07_18/",tag ,"_pauli_image.png", sep=""))
  
  rm(equalized_matrix)
}

nrow <- 4512
ncol <- 3300

#Source: UAVSAR
#Link: https://uavsar.jpl.nasa.gov/cgi-bin/product.pl?jobName=trauns_22551_15087_016_150604_L090_CX_01#data

file_HHHH <- file("../Data/MLC_Data/HHHH.mlc", "rb")
file_HVHV <- file("../Data/MLC_Data/HVHV.mlc", "rb")
file_VVVV <- file("../Data/MLC_Data/VVVV.mlc", "rb")

generate_image(file_HHHH, file_HVHV, file_VVVV, nrow , ncol, "mlc")


#Source: UAVSAR
#Link: https://uavsar.jpl.nasa.gov/cgi-bin/product.pl?jobName=mmmmoj_18030_17050_005_170519_PL09043020_XX_01#data

nrow <- 3661
ncol <- 1525

file_HHHH <- file("../Data/GRD_Data/HHHH.grd", "rb")
file_HVHV <- file("../Data/GRD_Data/HVHV.grd", "rb")
file_VVVV <- file("../Data/GRD_Data/VVVV.grd", "rb")

generate_image(file_HHHH, file_HVHV, file_VVVV, nrow , ncol, "grd")
