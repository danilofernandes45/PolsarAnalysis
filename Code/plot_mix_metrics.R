trihedral <- c(1, 0, 0, 0,
               0, 1, 0, 0,
               0, 0, 1, 0,
               0, 0, 0,-1)

cylinder <- c(5/8, 3/8, 0, 0,
              3/8, 5/8, 0, 0,
              0, 0, 1/2, 0,
              0, 0, 0, -1/2)

dipole <- c(1, -1, 0, 0,
            -1, 1, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0)

pos_wave <- c(1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, 0, 1,
              0, 0, 1, 0)

neg_wave <- c(1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, 0,-1,
              0, 0,-1, 0)

narrow_di <- c(5/8, 3/8, 0, 0,
               3/8, 5/8, 0, 0,
               0, 0, -1/2, 0,
               0, 0, 0, 1/2)

dihedral <- c(1, 0, 0, 0,
               0, 1, 0, 0,
               0, 0,-1, 0,
               0, 0, 0, 1)

left_helix <- c(1, 0, 0,-1,
               0, 0, 0, 0,
               0, 0, 0, 0,
               -1, 0, 0, 1)

right_helix <- c(1, 0, 0, 1,
                 0, 0, 0, 0,
                 0, 0, 0, 0,
                 1, 0, 0, 1)

despol <- c(1, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0)

norm <- function(x) sqrt(sum(x^2))

alpha_gd <- function(vector){
  inner_prod <- as.numeric( trihedral %*% vector )
  cos_angle <- inner_prod / ( norm(trihedral) * norm(vector) )
  angle <- (180/pi)*acos(cos_angle)
  return(angle)
}

helicity_gd <- function(vector){
  cos_lh <- as.numeric( left_helix %*% vector ) / ( norm(left_helix) * norm(vector) )
  gd_lh <- (2/pi)*acos(cos_lh)
  
  cos_rh <- as.numeric( right_helix %*% vector ) / ( norm(right_helix) * norm(vector) )
  gd_rh <- (2/pi)*acos(cos_rh)
  
  helicity <- 45 * (1 - sqrt(gd_lh * gd_rh))
  return(helicity)
}

purity_gd <- function(vector){
  inner_prod <- as.numeric( despol %*% vector )
  cos_angle <- inner_prod / ( norm(despol) * norm(vector) )
  gd_desp <- (2/pi)*acos(cos_angle)
  purity <- ( 1.5 * gd_desp )^2
  return(purity)
}

library(ggplot2)
library(plotly)

plot_mix <- function(vec1, vec2, adjustable = FALSE){

  tag1 = ""
  tag2 = ""
  if(all(vec1 == trihedral)){ tag1 = "Trihedral" }
  else if(all(vec1 == cylinder)){tag1 = "Cylinder"}
  else if(all(vec1 == dipole)){tag1 = "Dipole"}
  else if(all(vec1 == pos_wave)){tag1 = "+1/4-wave"}
  else if(all(vec1 == neg_wave)){tag1 = "-1/4-wave"}
  else if(all(vec1 == narrow_di)){tag1 = "Narrow dihedral"}
  else if(all(vec1 == dihedral)){tag1 = "Dihedral"}
  else if(all(vec1 == left_helix)){tag1 = "Left helix"}
  else if(all(vec1 == right_helix)){tag1 = "Right helix"}
  else if(all(vec1 == despol)){tag1 = "Despolarizer"}
  
  if(all(vec2 == trihedral)){ tag2 = "Trihedral" }
  else if(all(vec2 == cylinder)){tag2 = "Cylinder"}
  else if(all(vec2 == dipole)){tag2 = "Dipole"}
  else if(all(vec2 == pos_wave)){tag2 = "+1/4-wave"}
  else if(all(vec2 == neg_wave)){tag2 = "-1/4-wave"}
  else if(all(vec2 == narrow_di)){tag2 = "Narrow dihedral"}
  else if(all(vec2 == dihedral)){tag2 = "Dihedral"}
  else if(all(vec2 == left_helix)){tag2 = "Left helix"}
  else if(all(vec2 == right_helix)){tag2 = "Right helix"}
  else if(all(vec2 == despol)){tag2 = "Despolarizer"} 
  
  eta <- seq(from = 0, to = 1, by = 0.001)
  vec1 <- matrix(vec1, nrow = length(eta), ncol = 16, byrow = TRUE)
  vec2 <- matrix(vec2, nrow = length(eta), ncol = 16, byrow = TRUE)
  mix <- (1-eta) * vec1 + eta * vec2
  
  alphas <- apply(mix, 1, alpha_gd)
  helicities <- apply(mix, 1, helicity_gd)
  purities <- apply(mix, 1, purity_gd)
  
  len = length(alphas)
  
  p <- plot_ly(x = alphas, y = helicities, z = purities, type = 'scatter3d', mode = 'lines', name = "Trace",
          line = list(width = 6, color = eta, colorscale = "Bluered")) %>%
    add_trace(x = alphas[1], y = helicities[1], z = purities[1], name = tag1,
              mode = "markers", marker = list(color = "blue")) %>%
    add_trace(x = alphas[len], y = helicities[len], z = purities[len], name = tag2,
              mode = "markers", marker = list(color = "red"))
  
  if(adjustable){
    p %>% layout(scene = list(xaxis = list(title = 'Alpha'),
                              yaxis = list(title = 'Helicity'),
                              zaxis = list(title = 'Purity')))
  } else {
    p %>% layout(scene = list(xaxis = list(title = 'Alpha', range = c(0, 90)),
                              yaxis = list(title = 'Helicity', range = c(0,45)),
                              zaxis = list(title = 'Purity', range = c(0,1))))
  }
  
  
  # plot <- ggplot() + geom_line(aes(x = alphas, y = helicities), colour = "red", size = 1.5) + xlab("Alpha") + ylab("Helicity")
  # ggsave(paste(label, "_alpha_helicity.pdf", sep = ""), plot, width = 10, height = 10, units = "in")
  # 
  # plot <- ggplot() + geom_line(aes(x = alphas, y = purities), colour = "red", size = 1.5) + xlab("Alpha") + ylab("Purity")
  # ggsave(paste(label, "_alpha_purity.pdf", sep = ""), plot, width = 10, height = 10, units = "in")
  # 
  # plot <- ggplot() + geom_line(aes(x = helicities, y = purities), colour = "red", size = 1.5) + xlab("Helicity") + ylab("Purity")
  # ggsave(paste(label, "_helicity_purity.pdf", sep = ""), plot, width = 10, height = 10, units = "in")
  
}
