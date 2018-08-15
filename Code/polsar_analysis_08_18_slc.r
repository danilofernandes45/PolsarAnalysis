library(png)

read_complex_file <- function(file, nrow, ncol){
  
  complex_numbers <- matrix(
    readBin(file, double(), n = 2*nrow * ncol, size = 4, endian = "little"), 
    nrow = nrow*ncol, ncol = 2, byrow = TRUE) #size = 2 devido formato do arquivo, no geral, será 4
  
  return(matrix(complex_numbers[,1]^2 + complex_numbers[,1]^2, nrow = nrow, ncol = ncol, byrow = TRUE))
  
}

read_complex_file1 <- function(file, nrow, ncol) {
  #readBin com argumento complex gera imagem quadruplicada
  
  return( Mod(matrix(
    readBin(file, complex(), n = nrow * ncol, endian = "big"), 
    nrow = nrow, ncol = ncol, byrow = TRUE))^2 )
  
}

fill_matrix <- function(file_HH, file_HV, file_VV, nrow, ncol) {
  
  amplitude_matrix <- array(0, dim = c(nrow, ncol, 3))
  amplitude_matrix[,,1] <- read_complex_file(file_HH, nrow, ncol)
  amplitude_matrix[,,2] <- read_complex_file(file_HV, nrow, ncol)
  amplitude_matrix[,,3] <- read_complex_file(file_VV, nrow, ncol)
  
  return(amplitude_matrix)
  
}

equalize <- function(data, nrow, ncol){
  
  data[,,1] <- matrix(ecdf(data[,,1])(data[,,1]), nrow = nrow, ncol = ncol)
  
  data[,,2] <- matrix(ecdf(data[,,2])(data[,,2]), nrow = nrow, ncol = ncol)
  
  data[,,3] <- matrix(ecdf(data[,,3])(data[,,3]), nrow = nrow, ncol = ncol)  
  
  return(data)
}

filehh <- file("hh.bin", "rb")
filehv <- file("hv.bin", "rb")
filevv <- file("vv.bin", "rb")

amplitude_matrix <- fill_matrix(filehh, filehv, filevv, 2816, 1540)
equalized_matrix <- equalize(amplitude_matrix, 2816, 1540)
writePNG(equalized_matrix, target = "image.png")
