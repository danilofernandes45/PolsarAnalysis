#Criação de cabeçalho
#nrow <- 1100
#ncol <- 1772
# 
#r <- raster(nrows = nrow, ncols = ncol)
#hdr(r, format = 'RASTER', filename='hhhh.grd')
#hdr(r, format = 'RASTER', filename='hvhv.grd')
#hdr(r, format = 'RASTER', filename='vvvv.grd')

#Utilização

library(raster)
library(png)

hhhh <- raster("hhhh.grd")
hvhv <- raster("hvhv.grd")
vvvv <- raster("vvvv.grd")

sample_size <- round( ncell(hhhh) / 2000 )

sample_hhhh <- sampleRandom(hhhh, sample_size)
sample_fn <- ecdf(sample_hhhh)

equalize <- function(x){ sample_fn(x) }
equal_hhhh <- calc(hhhh, equalize, filename="hhhh_equalized.grd")


rm(sample_fn)
rm(sample_hhhh)

sample_hvhv <- sampleRandom(hvhv, sample_size)
sample_fn <- ecdf(sample_hvhv)

equal_hvhv <- calc(hvhv, equalize, filename="hvhv_equalized.grd")

rm(sample_fn)
rm(sample_hvhv)
sample_vvvv <- sampleRandom(vvvv, sample_size)
sample_fn <- ecdf(sample_vvvv)
equal_vvvv <- calc(vvvv, equalize, filename="vvvv_equalized.grd")

nrow <- nrow(hhhh)
ncol <- ncol(hhhh)

intensities_matrix <- array(0, dim = c(nrow, ncol, 3))
intensities_matrix[,,1] <- getValuesBlock(equal_hhhh, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = "matrix")
intensities_matrix[,,2] <- getValuesBlock(equal_hvhv, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = "matrix")
intensities_matrix[,,3] <- getValuesBlock(equal_vvvv, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = "matrix")

writePNG(intensities_matrix, target = "image_raster_read.png")
rm(intensities_matrix)