#Algorithm by John Omena
library("png")

resize <- function(x) {
  return( floor( x / 5 ) )  
}

#Read the file jump 5 columns and rows in the image
read_mlc <- function(file, nrow, ncol) { #Função ainda muito pesada
  
  #Reduce the image to 1/5
  mini_row <- resize( nrow )
  mini_col <- resize( ncol )
  
  matrix <- array(0, dim = c(mini_row, mini_col))
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

#Equalization function to Rdata
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

fileR <- file("Traunstein_Germany/HHHH.mlc", "rb")
fileG <- file("Traunstein_Germany/HVHV.mlc", "rb")
fileB <- file("Traunstein_Germany/VVVV.mlc", "rb")

UavsarRGB <- read_mini_RGB_mlc(fileR, fileG, fileB, nrow , ncol) #Resolução imagem 5773x3300

#Close files
close(fileR)
close(fileG)
close(fileB)
#Remove file from RAM memory
rm(fileR, fileG, fileB)

mini_row <- resize(nrow)
mini_col <- resize(ncol)

Uavsar_Eq <- Equal_RGB(UavsarRGB, mini_row, mini_col)

writePNG(Uavsar_Eq, target="Traunstein_Germany/image.png")
