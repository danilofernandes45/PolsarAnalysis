#Generate Image
# library(png)
# image_matrix <- array(0, dim = c(nrow, ncol, 3))
# 
# image_matrix[,,1] <- ecdf(hhhh)(hhhh)
# image_matrix[,,2] <- ecdf(hvhv)(hvhv)
# image_matrix[,,3] <- ecdf(vvvv)(vvvv)
# 
# writePNG(image_matrix, target = "image.png")

#Similarities
sqrt_trace_kennaugh <- sqrt(hhhh^2 + hvhv^2 + vvvv^2 + 2*int_hhhv + 2*int_hhvv + 2*int_hvvv)

similarity_trihedral <- 1 - (2/pi)*acos(hhhh/sqrt_trace_kennaugh)
similarity_dihedral <- 1 - (2/pi)*acos(hvhv/sqrt_trace_kennaugh)
similarity_random_volume <- 1 - (2/pi)*acos(( 2*hhhh + hvhv + vvvv )/(sqrt(6)*sqrt_trace_kennaugh))

similarity_narrow_dihedral <- 1 - (2/pi)*acos( ( hhhh + 9*hvhv + 6*Re(hhhv) ) / (10*sqrt_trace_kennaugh) )
similarity_cylinder <- 1 - (2/pi)*acos( ( 9*hhhh + hvhv + 6*Re(hhhv) ) / (10*sqrt_trace_kennaugh) )

similarity_dipole <- 1 - (2/pi)*acos( ( hhhh + hvhv - 2*Re(hhhv) ) / (2*sqrt_trace_kennaugh) )

similarity_left_helix <- 1 - (2/pi)*acos( ( hvhv + vvvv - 2*Im(hvvv) ) / (2*sqrt_trace_kennaugh) )
similarity_right_helix <- 1 - (2/pi)*acos( ( hvhv + vvvv + 2*Im(hvvv) ) / (2*sqrt_trace_kennaugh) )

similarity_pos_wave <- 1 - (2/pi)*acos( ( hhhh + hvhv - 2*Im(hhhv) ) / (2*sqrt_trace_kennaugh) )
similarity_neg_wave <- 1 - (2/pi)*acos( ( hhhh + hvhv + 2*Im(hhhv) ) / (2*sqrt_trace_kennaugh) )