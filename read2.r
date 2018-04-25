library("png")
library("raster")

read_min_vector <- function(file, pos_file, elements_per_row, bytes_per_element) {
  
  min_vector <- array(0, dim = c(1, 9))
  
  for(i in 1:3) {
    for(j in 1:3) {
      seek(file, where = pos_file, origin = "start")
      min_vector[i*j] <- readBin(file, double(), n=1, size = bytes_per_element, endian = "little")
      pos_file <- pos_file + 2*bytes_per_element
    }
    
    pos_file <- pos_file - 7*bytes_per_element
    pos_file <- pos_file + 2*bytes_per_element*elements_per_row
    
  }
  
  return(min_vector)
  
}

new_pos_file <- function(pos_file, elements_by_row, bytes_per_element) {
  
  num_col_min_matrix <- 5*bytes_per_element
  pos_file <- (pos_file - 7*bytes_per_element*elements_by_row) + num_col_min_matrix
  
  return(pos_file)
  
}

resize <- function(x) {
  
  return( floor( x / 5 ) )
  
}

medium_value <- function(vector, size) {
  
  amount <- 0
  for(i in 1:size) {
    amount <- amount + vector[i]
  }
  
  return(amount/size)
  
}

#Read the file jump 5 columns and rows in the image
read_mlc <- function(file, nrow, ncol) { #Função ainda muito pesada
  
  #Reduce the image to 1/5
  mini_row <- resize( nrow )
  mini_col <- resize( ncol )
  
  matrix <- array(0, dim = c(mini_row, mini_col))
  
  excess_row <- nrow %% 5
  excess_col <- ncol %% 5
  
  next_byte <- 0
  bytes_per_element <- 4
  col_jump <- 5 #columns to ignore
  row_jump <- 4 #rows to ignore
  
  for(row in 1:mini_row) {
    
    seek(file, where = next_byte, origin = "start")
    
    for(col in 1:mini_col) {
      
      min_vector <- read_min_vector(file, next_byte, ncol, bytes_per_element)
      next_byte <- new_pos_file(next_byte, ncol, bytes_per_element)
      
      min_vector <- equalize(min_vector, nrow = 1, ncol = 9)
      
      matrix[row, col] <- medium_value(min_vector, size = 9)
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

equalize <- function(matrix, nrow, ncol) {
  
  matrix <- matrix(ecdf(matrix)(matrix), nrow = nrow,
                      ncol = ncol)
  return(matrix)
  
}

nrow <- 5773
ncol <- 3300

fileR <- file("HHHH.mlc", "rb")
fileG <- file("HVHV.mlc", "rb")
fileB <- file("VVVV.mlc", "rb")

UavsarRGB <- read_mini_RGB_mlc(fileR, fileG, fileB, nrow , ncol) #Resolução imagem 4512x3300

#Fechando arquivos
close(fileR)
close(fileG)
close(fileB)
#Removendo arquivos da memoria RAM
rm(fileR, fileG, fileB)


writePNG(UavsarRGB, target="test.png")

