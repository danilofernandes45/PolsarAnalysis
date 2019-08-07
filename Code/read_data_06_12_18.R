read_int_complex_file <- function(file, nrow, ncol){
  
  complex_numbers <- matrix(
    readBin(file, double(), n = 2*nrow * ncol, size = 4, endian = "little"), 
    nrow = nrow*ncol, ncol = 2, byrow = TRUE) #size = 2 devido formato do arquivo, no geral, será 4
  
  return( matrix( complex_numbers[,1]^2 + complex_numbers[,2]^2, nrow = nrow, ncol = ncol, byrow = TRUE))
  
}

read_real_complex_file <- function(file, nrow, ncol){
  
  complex_numbers <- matrix(
    readBin(file, double(), n = 2*nrow * ncol, size = 4, endian = "little"), 
    nrow = nrow*ncol, ncol = 2, byrow = TRUE) #size = 2 devido formato do arquivo, no geral, será 4
  
  return( matrix( complex_numbers[,1], nrow = nrow, ncol = ncol, byrow = TRUE))
  
}

read_im_complex_file <- function(file, nrow, ncol){
  
  complex_numbers <- matrix(
    readBin(file, double(), n = 2*nrow * ncol, size = 4, endian = "little"), 
    nrow = nrow*ncol, ncol = 2, byrow = TRUE) #size = 2 devido formato do arquivo, no geral, será 4
  
  return( matrix( complex_numbers[,2], nrow = nrow, ncol = ncol, byrow = TRUE))
  
}

read_complex_file <- function(file, nrow, ncol){
  
  complex_numbers <- matrix(
    readBin(file, double(), n = 2*nrow * ncol, size = 4, endian = "little"), 
    nrow = nrow*ncol, ncol = 2, byrow = TRUE) #size = 2 devido formato do arquivo, no geral, será 4
  
  return( matrix( complex( real = complex_numbers[,1], imaginary = complex_numbers[,2] ), nrow = nrow, ncol = ncol, byrow = TRUE))
  
}

read_complex_files(fileRe, fileIm, nrow, ncol){
  
  return(
    matrix(
      complex(
        readBin(fileRe, double(), n = nrow * ncol, size = 4, endian = "little"),
        readBin(fileIm, double(), n = nrow * ncol, size = 4, endian = "little")
      ), 
      nrow = nrow, ncol = ncol, byrow = TRUE
    )
  )
  
}

read_file <- function(file, nrow, ncol){
  
  return(matrix(
    readBin(file, double(), n = nrow * ncol, size = 4, endian = "little"), 
    nrow = nrow, ncol = ncol, byrow = TRUE) )
}

nrow <- 615
ncol <- 445


# hhhv_file <- file("hhhv.mlc", "rb")
# Re_hhhv <- read_real_complex_file(hhhv_file, nrow = nrow, ncol = ncol)
# Im_hhhv <- read_im_complex_file(hhhv_file, nrow = nrow, ncol = ncol)
# 
# hhvv_file <- file("hhvv.mlc", "rb")
# Re_hhvv <- read_real_complex_file(hhvv_file, nrow = nrow, ncol = ncol)
# Im_hhvv <- read_im_complex_file(hhvv_file, nrow = nrow, ncol = ncol)
# 
# hvvv_file <- file("hvvv.mlc", "rb")
# Re_hvvv <- read_real_complex_file(hvvv_file, nrow = nrow, ncol = ncol)
# Im_hvvv <- read_im_complex_file(hvvv_file, nrow = nrow, ncol = ncol)


hhhv_file <- file("hhhv.mlc", "rb")
hhhv <- read_complex_file(hhhv_file, nrow = nrow, ncol = ncol)

hhvv_file <- file("hhvv.mlc", "rb")
hhvv <- read_complex_file(hhvv_file, nrow = nrow, ncol = ncol)

hvvv_file <- file("hvvv.mlc", "rb")
hvvv <- read_complex_file(hvvv_file, nrow = nrow, ncol = ncol)

int_hhhv <- Mod(hhhv)^2
int_hhvv <- Mod(hhvv)^2
int_hvvv <- Mod(hvvv)^2

# hhhv_file <- file("hhhv.mlc", "rb")
# int_hhhv <- read_int_complex_file(hhhv_file, nrow = nrow, ncol = ncol)
# hhvv_file <- file("hhvv.mlc", "rb")
# int_hhvv <- read_int_complex_file(hhvv_file, nrow = nrow, ncol = ncol)
# hvvv_file <- file("hvvv.mlc", "rb")
# int_hvvv <- read_int_complex_file(hvvv_file, nrow = nrow, ncol = ncol)
# 

hhhh_file <- file("hhhh.mlc", "rb")
hhhh <- read_file(hhhh_file, nrow = nrow, ncol = ncol)

hvhv_file <- file("hvhv.mlc", "rb")
hvhv <- read_file(hvhv_file, nrow = nrow, ncol = ncol)

vvvv_file <- file("vvvv.mlc", "rb")
vvvv <- read_file(vvvv_file, nrow = nrow, ncol = ncol)

#Analysis

hhhh_file <- file("T11.bin", "rb")
hhhh <- read_file(hhhh_file, nrow = nrow, ncol = ncol)

hvhv_file <- file("T22.bin", "rb")
hvhv <- read_file(hvhv_file, nrow = nrow, ncol = ncol)

vvvv_file <- file("T33.bin", "rb")
vvvv <- read_file(vvvv_file, nrow = nrow, ncol = ncol)

matrix <- array(0, dim = c(nrow, ncol, 3))

matrix[,,1] <- ecdf(hhhh)(hhhh)
matrix[,,2] <- ecdf(hvhv)(hvhv)
matrix[,,3] <- ecdf(vvvv)(vvvv)

library("png")
writePNG(matrix, target = "test.png")

#Regions selected
writePNG(matrix[100:155, 1:80,], target = "region1_soybeans.png")
