source("SARTimeSerie.R")
source("Band&Pompe.R")

#The SAR data is available on https://drive.google.com/file/d/1jtbOcYwQfysfcUp4UhoA7lSl4_tPIqfa/view?usp=sharing and
# correspond to HHHH band of an image taken from the Cape Canaveral (acquired Sep 22, 2016)

#Ocean regions in Cape Canaveral
  #{Behavior 1}
dim <- c(100, 200, 1700, 200)
dim <- c(300, 200, 1700, 200)
dim <- c(100, 200, 1900, 200)
dim <- c(300, 200, 1900, 200)
  #{Behavior 2}
dim <- c(100, 200, 100, 200)
dim <- c(400, 200, 100, 200)
dim <- c(700, 200, 100, 200)
dim <- c(1100, 200, 100, 200)


#The SAR data is available on https://drive.google.com/file/d/1pO6p_UI9Cgdci9y6jVynAv8SrrAvv7K8/view?usp=sharing and
# correspond to HHHH band of an image taken from the Munich, Germany (acquired Jun 5, 2015) 

#Urban regions in Munich
dim <- c(3000, 200, 400, 200)
dim <- c(3000, 200, 600, 200)
dim <- c(3200, 200, 400, 200)
dim <- c(3200, 200, 600, 200)

ns <- 8
dim <- matrix(nrow = ns, ncol = 4)

#The SAR data is available on https://drive.google.com/file/d/1-tBmid6Lz_ps_L3OpVVnoR64cENGzR1O/view?usp=sharing and
# correspond to HHHH band of an image taken from the Sierra del Lacandon National Park, Guatemala (acquired Apr 10, 2015)

#Forest regions in Guatemala
dim[1,] <- c(5600, 200, 2700, 200) #region 1
dim[2,] <- c(5200, 200, 2800, 200) #region 2
dim[3,] <- c(4100, 200, 2930, 200) #region 3
dim[4,] <- c(1075, 200, 1930, 200) #region 4

#Crop regions in Guatemala
dim[5,] <- c(400, 200, 1500, 200) #region 5

#Ground regions in Guatemala
#---------------------------------------------------
#Possibilly you will have problems with this regions
#It isn't uniform
dim[6,] <- c(250, 200, 1100, 200) #region 6
dim[7,] <- c(250, 200, 1850, 200) #region 7
dim[8,] <- c(1675, 200, 1930, 200) #region 8

#Get Time Serie from Guatemala SAR data
#--------------------------------------
#Bandit and Pompe parameters - Test 1
n <- 3
tal <- 1
timeSerie <- matrix(nrow = ns, ncol = 352836)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)

for(i in c(1:ns)){
  timeSerie[i,] <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  probability[i,1:factorial(n)] <- Bandt.Pompe(timeSerie[i,], n, tal)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

png("Dimension3Tal1.png")
HCPlane(probability, ns, n)
dev.off()

#--------------------------------------
#Bandit and Pompe parameters - Test 2
n <- 6
tal <- 7
timeSerie <- matrix(nrow = ns, ncol = 352836)
probability <- matrix(nrow = ns, ncol = factorial(n) + 1)

for(i in c(1:ns)){
  timeSerie[i,] <- getTimeSerie("guatemala", dim[i,], n, tal) #timeSerie is a matrix with order K x n x n, where K is the number of observations 
  probability[i,1:factorial(n)] <- Bandt.Pompe(timeSerie[i,], n, tal)
}

probability[1:4, factorial(n) + 1] = 1
probability[5, factorial(n) + 1] = 2
probability[6:8, factorial(n) + 1] = 3

png("Dimension3Tal2.png")
HCPlane(probability, ns, n)
dev.off()
