plot <- function(data1, data2, dist2, params2){
  
  seq <- seq( from = 0, to = 1, by = 0.01)
  
  #Estimation parameters for data1
  mean1 <- mean(data1)
  var1 <- sd(data1) ^ 2
  alpha1 <- mean1 * ( mean1 * (1 - mean1) / var1 - 1 )
  beta1 <- ( 1 - mean1 ) * ( mean1 * ( 1 - mean1 ) / var1 - 1)
  
  y1 <- dbeta(seq, alpha1, beta1)
  
  desc1 <- "Forest"
  desc2 <- "Bare soil"
  #desc1 <- paste("Beta(", round(alpha1, 3), ", ", round(beta1, 3), ")", sep="")
  
  #Estimation parameters for data2
  if(dist2 == "beta"){
    
    mean2 <- mean(data2)
    var2 <- sd(data2) ^ 2
    alpha2 <- mean2 * ( mean2 * (1 - mean2) / var2 - 1 )
    beta2 <- ( 1 - mean2 ) * ( mean2 * ( 1 - mean2 ) / var2 - 1)
    
    y2 <- dbeta(seq, alpha2, beta2)
    
    #desc2 <- paste("Beta(", round(alpha2, 3), ", ", round(beta2, 3), ")", sep="")
  }
  else if(dist2 == "pert"){
    
    mode <- (6*mean(data2) - params2[1] - params2[2])/4
    
    y2 <- dpert(seq, min = params2[1], mode, max = params2[2])
    
    #desc2 <- paste("PERT(", round(params2[1], 3), ", ", round(mode, 3), ", ", round(params2[2], 3), ")", sep="")
  }
  
  #Plot
  
  x <- c(seq, seq)
  region <- c(rep(desc1, length(seq)), rep(desc2, length(seq)) )
  value <- c(y1, y2)
  
  df <- data.frame(x, region, value)
  
  ggplot() + 
    geom_histogram(aes(x = c(data1), y = ..density..), fill = "green", alpha = 0.4) +
    geom_histogram(aes(x = c(data2), y = ..density..), fill = "red", alpha = 0.4) +
    geom_line(data = df, aes(x=x, y=value, group=region, colour=region), size=1.5) +
    scale_color_manual(values=c("red", "green")) +
    xlab("similarity") + ylab("density") +
    theme_gray(base_size = 20)
  
}

