back <- array(0, dim = c(10, 16))

back[10,] <- c(1 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 ,
              0 , 0 , 0 , -1 ,
              0 , 0 , -1 , 0) #wvn
back[9,] <- c(1 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 ,
              0 , 0 , 0 , 1 ,
              0 , 0 , 1 , 0) #wvp

back[5,] <- c(5/8 , 3/8 , 0 , 0 ,
              3/8 , 5/8 , 0 , 0 ,
              0 , 0 , 1/2 , 0 ,
              0 , 0 , 0 , -1/2) #cy

back[2,] <- c(1 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 ,
              0 , 0 , -1 , 0 ,
              0 , 0 , 0 , 1) #di

back[6,] <- c(1 , -1 , 0 , 0 ,
              -1 , 1 , 0 , 0 ,
              0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0) #dip

back[4,] <- c(5/8 , 3/8 , 0 , 0 ,
              3/8 , 5/8 , 0 , 0 ,
              0 , 0 , -1/2 , 0 ,
              0 , 0 , 0 , 1/2) #nd

back[7,] <- c(1 , 0 , 0 , -1 ,
              0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 ,
              -1 , 0 , 0 , 1) #lh

back[8,] <- c(1 , 0 , 0 , 1 ,
              0 , 0 , 0 , 0 ,
              0 , 0 , 0 , 0 ,
              1 , 0 , 0 , 1) #rh

back[3,] <- c(1 , 0 , 0 , 0 ,
              0 , 1/2 , 0 , 0 ,
              0 , 0 , 1/2 , 0 ,
              0 , 0 , 0 , 0) #rv

back[1,] <- c(1 , 0 , 0 , 0 ,
              0 , 1 , 0 , 0 ,
              0 , 0 , 1 , 0 ,
              0 , 0 , 0 , -1) #tr

dist_matrix <- array(0, dim = c(10, 10))

for(i in 1:9){
  for(j in (i+1):10){
    
    inner_prod <- sum(back[i, ] * back[j, ])
    norm1 <- sqrt( sum( back[i, ]^2 ) )
    norm2 <- sqrt( sum( back[j, ]^2 ) )
    
    dist_matrix[i, j] <- (2/pi) * acos( inner_prod / ( norm1 * norm2 ) )
    dist_matrix[j, i] <- dist_matrix[i, j]
    
  }
}

dist_matrix

require(network)
require(GGally)

# distances <- network(dist_matrix, directed = FALSE)
# network.vertex.names(distances) <- c("TR", "DI", "RV", "ND", "CY", "DIP", "LH", "RH", "WVP", "WVN")
# list.edge.attributes(distances)
# ggnet2(distances, size=20, label=TRUE, label.size = 5, label.color="black")
# ggsave(file="../Figures/paper_19_05/network.pdf")

distances <- network(dist_matrix, directed = FALSE)
network.vertex.names(distances) <- c("TR", "DI", "RV", "ND", "CY", "DIP", "LH", "RH", "WVP", "WVN")
list.edge.attributes(distances)
ggnet2(distances, size=10, label=TRUE, label.size = 2, color = "steelblue", label.color="white", fontface = "bold")
ggsave(file="../Figures/paper_19_05/network.pdf")
