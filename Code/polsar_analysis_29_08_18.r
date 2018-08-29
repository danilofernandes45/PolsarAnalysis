library(raster)
library(png)

# 1. Criação de cabeçalho

# 1.1. Dimensões da imagem
nrow <- 1100
ncol <- 1772
 
r <- raster(nrows = nrow, ncols = ncol)
hdr(r, format = 'RASTER', filename='hhhh.grd')
hdr(r, format = 'RASTER', filename='hvhv.grd')
hdr(r, format = 'RASTER', filename='vvvv.grd')

# 2. Utilização

hhhh <- raster("hhhh.grd")
hvhv <- raster("hvhv.grd")
vvvv <- raster("vvvv.grd")

parallel_cv <- function(x) localFun(x, x, ngb = 3, fun = function(x,y) sd(x)/mean(x))

beginCluster()

hhhh_cv <- clusterR(hhhh, parallel_cv, filename = "hhhh_cv.grd")
hvhv_cv <- clusterR(hvhv, parallel_cv, filename = "hvhv_cv.grd")
vvvv_cv <- clusterR(vvvv, parallel_cv, filename = "vvvv_cv.grd")

endCluster()

nrow <- nrow(hhhh_cv)
ncol <- ncol(hhhh_cv)

matrix_cv <- array(0, dim = c(nrow, ncol, 3))

matrix_cv[,,1] <- getValuesBlock(hhhh_cv, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = 'matrix')
matrix_cv[,,2] <- getValuesBlock(hvhv_cv, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = 'matrix')
matrix_cv[,,3] <- getValuesBlock(vvvv_cv, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = 'matrix')

writePNG(matrix_cv, target = "image.png")
