select_sample <- function(matrix, dim){
  a <- dim[1]
  b <- a + dim[2]
  c <- dim[3]
  d <- c + dim[4]
  
  matrix[(a-2):a,(c-2):(d+2),] <- 1
  matrix[b:(b+2),(c-2):(d+2),] <- 1
  
  matrix[(a-2):(b+2), (c-2):c, ] <- 1
  matrix[(a-2):(b+2), d:(d+2), ] <- 1
  
  return(matrix)
}

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

hhhh_file <- file("HHHH.gri", "rb")
hhhh <- read_file(hhhh_file, nrow = nrow, ncol = ncol)

hvhv_file <- file("HVHV.gri", "rb")
hvhv <- read_file(hvhv_file, nrow = nrow, ncol = ncol)

vvvv_file <- file("VVVV.gri", "rb")
vvvv <- read_file(vvvv_file, nrow = nrow, ncol = ncol)

matrix <- array(0, dim = c(nrow, ncol, 3))

# matrix[,,1] <- ecdf(hhhh)(hhhh)
# matrix[,,2] <- ecdf(hvhv)(hvhv)
# matrix[,,3] <- ecdf(vvvv)(vvvv)

# library("png")
# writePNG(matrix, target = "test.png")
# 
# #Regions selected
# writePNG(matrix[100:155, 1:80,], target = "region1_soybeans_232_231.png")
# 
# writePNG(matrix[100:165, 50:80,], target = "region1_soybeans_232.png")
# writePNG(matrix[90:155, 1:30,], target = "region2_soybeans_231.png")
# writePNG(matrix[115:180, 360:390,], target = "region3_soybeans_101.png")
# 
# writePNG(matrix[25:80, 30:80,], target = "region2_canola_43.png")
# writePNG(matrix[180:241, 290:335,], target = "region1_canola224.png")
# 
# writePNG(matrix[160:225, 245:280,], target = "region3_wheat_255.png")
# writePNG(matrix[425:490, 315:350,], target = "region2_wheat_105.png")
# writePNG(matrix[500:565, 310:345,], target = "region1_wheat_104.png")
# 
# writePNG(matrix[415:480, 260:295,], target = "region2_oats_102.png")
# writePNG(matrix[490:555, 260:295,], target = "region1_oats_103.png")

#===================================================================

#Pauli Decomposition
matrix[,,1] <- ecdf(hhhh+vvvv)(hhhh+vvvv)
matrix[,,2] <- ecdf(abs(hhhh-vvvv))(abs(hhhh-vvvv))
matrix[,,3] <- ecdf(2*hvhv)(2*hvhv)

matrix <- select_sample(matrix, c(115, 65, 360, 30)) #SB 101
matrix <- select_sample(matrix, c(90, 65, 6, 30))    #SB 231
matrix <- select_sample(matrix, c(100, 65, 50, 30))  #SB 232

matrix <- select_sample(matrix, c(160, 65, 245, 35)) #WT 255
matrix <- select_sample(matrix, c(444, 65, 370, 35)) #WT 105 ***
matrix <- select_sample(matrix, c(515, 65, 360, 35)) #WT 104 ***

matrix <- select_sample(matrix, c(180, 61, 290, 45)) #CN 224
matrix <- select_sample(matrix, c(25, 55, 30, 50))   #CN 43

matrix <- select_sample(matrix, c(425, 65, 315, 35)) #OT 102
matrix <- select_sample(matrix, c(500, 65, 310, 35)) #OT 103

library(png)
writePNG(matrix, target = "regions_4.png")
