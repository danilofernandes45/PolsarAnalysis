library("png")

fill_matrix_from_mlc <- function(file_HHHH, file_HVHV, file_VVVV, nrow, ncol) {
  
  data_HHHH <- matrix( readBin(file_HHHH, double(), n = nrow * ncol, size = 4, endian = "little"), 
                       nrow = nrow, ncol = ncol, byrow = TRUE)
  
  data_HVHV <- matrix( readBin(file_HVHV, double(), n = nrow * ncol, size = 4, endian = "little"), 
                       nrow = nrow, ncol = ncol, byrow = TRUE)
  
  data_VVVV <- matrix( readBin(file_VVVV, double(), n = nrow * ncol, size = 4, endian = "little"), 
                       nrow = nrow, ncol = ncol, byrow = TRUE)
  
  amplitude_matrix <- array(0, dim = c(nrow, ncol, 3))
  amplitude_matrix[,,1] <- data_HHHH
  amplitude_matrix[,,2] <- data_HVHV
  amplitude_matrix[,,3] <- data_VVVV
  
  return(amplitude_matrix)
  
}

pauli_decomposition <- function(matrix, nrow, ncol){
  
  pauli_matrix <- array(0, dim = c(nrow, ncol, 3))
  
  pauli_matrix[,,1] <- matrix[,,1] + matrix[,,2]
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
  
  amplitude_matrix <- fill_matrix_from_mlc(file_HHHH, file_HVHV, file_VVVV, nrow , ncol)
  
  #Close files
  close(file_HHHH)
  close(file_HVHV)
  close(file_VVVV)
  #Remove file from RAM memory
  rm(file_HHHH, file_HVHV, file_VVVV)
  
  #Direct projection of Z on colors' space
  equalized_matrix <- equalize(amplitude_matrix, nrow, ncol)
  
  writePNG(equalized_matrix, target = paste("Images/", tag, "_image.png", sep=""))
  
  rm(equalized_matrix)
  
  #Pauli decomposition
  pauli_matrix <- pauli_decomposition(amplitude_matrix, nrow, ncol)
  
  rm(amplitude_matrix)
  
  equalized_matrix <- equalize(pauli_matrix, nrow, ncol)
  
  writePNG(equalized_matrix, target = paste("Images/",tag ,"_pauli_image.png", sep=""))
  
}

nrow <- 4512
ncol <- 3300

#Source: UAVSAR
#Link: https://uavsar.jpl.nasa.gov/cgi-bin/product.pl?jobName=trauns_22551_15087_016_150604_L090_CX_01#data

file_HHHH <- file("MLC_Data/HHHH.mlc", "rb")
file_HVHV <- file("MLC_Data/HVHV.mlc", "rb")
file_VVVV <- file("MLC_Data/VVVV.mlc", "rb")

generate_image(file_HHHH, file_HVHV, file_VVVV, nrow , ncol, "mlc")


#Source: PolSARPro
#Link: https://earth.esa.int/documents/653194/658149/EMISAR_Foulum-C3

nrow <- 1100
ncol <- 1772

file_HHHH <- file("CO_Data/hhhh.co", "rb")
file_HVHV <- file("CO_Data/hvhv.co", "rb")
file_VVVV <- file("CO_Data/vvvv.co", "rb")

generate_image(file_HHHH, file_HVHV, file_VVVV, nrow , ncol, "co")
