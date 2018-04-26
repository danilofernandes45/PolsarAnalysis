library("png")
library("raster")

#Função para leitura dos arquivos MLC que contem os dados Polsar
Read_RGB_mlc <- function(fileR, fileG, fileB, nrow, ncol) {
  
  dataR <- matrix(readBin(fileR, double(), n = nrow * ncol, size = 4, endian = "little"),
                  nrow = nrow, ncol = ncol, byrow = TRUE)
  dataG <- matrix(readBin(fileG, double(), n = nrow * ncol, size = 4, endian = "little"),
                  nrow = nrow, ncol = ncol, byrow = TRUE)
  dataB <- matrix(readBin(fileB, double(), n = nrow * ncol, size = 4, endian = "little"),
                  nrow = nrow, ncol = ncol, byrow = TRUE)
  
  RGB_polsar <- array(0, dim = c(nrow, ncol, 3)) #Array de matrizes
  RGB_polsar[,,1] <- dataR
  RGB_polsar[,,2] <- dataG
  RGB_polsar[,,3] <- dataB
  
  return(RGB_polsar)
  
}

resize <- function(x) {
  
  return( floor( x / 5 ) )
  
}

#Read the file jump 5 columns and rows in the image
read_mlc <- function(file, nrow, ncol) { #Função ainda muito pesada
  
  #Reduce the image to 1/5
  mini_row <- resize( nrow )
  mini_col <- resize( ncol )
  
  matrix <- array(0, dim = c(mini_row, mini_col))
  
  #excess_row <- nrow %% 5
  excess_col <- ncol %% 5
 
  next_byte <- 0
  bytes_per_element <- 4
  col_jump <- 5 #columns to ignore
  row_jump <- 4 #rows to ignore
  
  for(row in 1:mini_row) {
    
    seek(file, where = next_byte, origin = "start")
    
    for(col in 1:mini_col) {
      
      #next line maybe might improved to be more efficient
      matrix[row, col] <- readBin(file, double(), n=1, size = bytes_per_element, endian = "little") # n=1 means to read only one element
      next_byte <- next_byte + bytes_per_element * col_jump 
      
      seek(file, where = next_byte, origin = "start")
      
    }
    
    #Jump the excess of bytes
    next_byte <- next_byte + excess_col * bytes_per_element
    
    #Jump 4 rows
    next_byte <- next_byte + row_jump * ncol * bytes_per_element
    
  }
  
  return(matrix)
   
}
read_mini_RGB_mlc <- function(fileR, fileG, fileB, nrow, ncol) {
  
  dataR <- read_mlc(fileR, nrow, ncol)
  dataG <- read_mlc(fileG, nrow, ncol)
  dataB <- read_mlc(fileB, nrow, ncol)
  
  mini_row <- resize(nrow)
  mini_col <- resize(ncol)
  
  RGB_polsar <- array(0, dim = c(mini_row, mini_col, 3))
  RGB_polsar[,,1] <- dataR
  RGB_polsar[,,2] <- dataG
  RGB_polsar[,,3] <- dataB
  
  return(RGB_polsar)
  
}

#Função que equaliza o Rdata
Equal_RGB <- function(data, nrow, ncol){
  
  data[,,1] <- matrix(ecdf(data[,,1])(data[,,1]), nrow = nrow,
                      ncol = ncol)
  data[,,2] <- matrix(ecdf(data[,,2])(data[,,2]), nrow = nrow,
                      ncol = ncol)
  data[,,3] <- matrix(ecdf(data[,,3])(data[,,3]), nrow = nrow,
                      ncol = ncol)  
  return(data)
}

nrow <- 5773
ncol <- 3300

fileR <- file("Munich_Germany/HHHH.mlc", "rb")
fileG <- file("Munich_Germany/HVHV.mlc", "rb")
fileB <- file("Munich_Germany/VVVV.mlc", "rb")

UavsarRGB <- read_mini_RGB_mlc(fileR, fileG, fileB, nrow , ncol) #Resolução imagem 4512x3300

#Fechando arquivos
close(fileR)
close(fileG)
close(fileB)
#Removendo arquivos da memoria RAM
rm(fileR, fileG, fileB)

mini_row <- resize(nrow)
mini_col <- resize(ncol)

Uavsar_Eq <- Equal_RGB(UavsarRGB, mini_row, mini_col)

#Plotagem das bandas de RGB quando aplicada a ecdf()
#par(mfrow = c(2,2))
#plot(ecdf(UavsarRGB[,,1]), ylab="Probability distribution", xlab="Red band", verticals=FALSE, col.01line="gray70", pch=19)
#plot(ecdf(UavsarRGB[,,2]), ylab="Probability distribution", xlab="Green band", verticals=FALSE, col.01line="gray70", pch=19)
#plot(ecdf(UavsarRGB[,,3]), ylab="Probability distribution", xlab="Blue band", verticals=FALSE, col.01line="gray70", pch=19)

writePNG(Uavsar_Eq, target="Munich_Germany/test.png")

#Imagens das bandas RGB separadas
#writePNG(Uavsar_Eq[,,1], target="test1.png")
#writePNG(Uavsar_Eq[,,2], target="test2.png")
#writePNG(Uavsar_Eq[,,3], target="test3.png")
