#Utilização

library(raster)

hhhh <- raster("HHHH.grd")
hvhv <- raster("HVHV.grd")
vvvv <- raster("VVVV.grd")

mySampleStratified <- function(raster_obj, stractum_size){
  
  dim_stractum <- floor( sqrt(stractum_size) )
  
  range_i <- floor( nrow(raster_obj) / dim_stractum )
  range_j <- floor( ncol(raster_obj) / dim_stractum )
  
  sample <- array(length(range_i*range_j))
  count <- 0
  
  for(i in 1:range_i){
    
    for(j in 1:range_j){
     
      sample[count] <- raster_obj[(i-1)*dim_stractum + sample(1:dim_stractum, 1), (j-1)*dim_stractum + sample(1:dim_stractum, 1)]
      count <- count + 1  
    }
    
  }
  
  return(sample)
  
}

num_stractums <- 2000

sample_hhhh <- mySampleStratified(hhhh, num_stractums)
sample_fn <- ecdf(sample_hhhh)

equalize <- function(x){ sample_fn(x) }
equal_hhhh <- calc(hhhh, equalize, filename="hhhh_equalized.grd")


rm(sample_fn)
rm(sample_hhhh)

sample_hvhv <- sampleMultistage(hvhv, cluster_size)
sample_fn <- ecdf(sample_hvhv)

equal_hvhv <- calc(hvhv, equalize, filename="hvhv_equalized.grd")

rm(sample_fn)
rm(sample_hvhv)
sample_vvvv <- sampleMultistage(vvvv, cluster_size)
sample_fn <- ecdf(sample_vvvv)
equal_vvvv <- calc(vvvv, equalize, filename="vvvv_equalized.grd")

nrow <- nrow(hhhh)
ncol <- ncol(hhhh)

intensities_matrix <- array(0, dim = c(nrow, ncol, 3))
intensities_matrix[,,1] <- getValuesBlock(equal_hhhh, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = "matrix")
intensities_matrix[,,2] <- getValuesBlock(equal_hvhv, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = "matrix")
intensities_matrix[,,3] <- getValuesBlock(equal_vvvv, nrow = 1, nrows = nrow, ncol = 1, ncols = ncol, format = "matrix")

library(png)

writePNG(intensities_matrix, target = "image_raster_read.png")
rm(intensities_matrix)