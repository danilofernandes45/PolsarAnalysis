# Attempt with the Cullen-Frey plot

require(fitdistrplus)

## Data Preparation
load("../Data/ACFrery/Indices_Sample.RData")
GeodesicMeasures <- sample
rm(sample)
GeodesicMeasures$Observation <- as.numeric(paste(GeodesicMeasures$Observation))
summary(GeodesicMeasures)

Alpha <- subset(GeodesicMeasures, Index=="Alpha", select = c(Observation, Date, Crop))
names(Alpha) <- c("Alpha", "Date", "Crop")

Helicity <- subset(GeodesicMeasures, Index=="Helicity", select = c(Observation, Date, Crop))
names(Helicity) <- c("Helicity", "Date", "Crop")

Purity <- subset(GeodesicMeasures, Index=="Purity", select = c(Observation, Date, Crop))
names(Purity) <- c("Purity", "Date", "Crop")

rm(GeodesicMeasures)

## Cullen-Frey plot of Alpha

descdist(unlist(subset(Alpha, Date==Date[1] & Crop==Crop[1], select = Alpha)),
         obs.col = "red", obs.pch = 20)

tmp <- matrix(nrow = 20, ncol=2)
i <- 1
for(d in levels(Alpha$Date)) {
  for(c in levels(Alpha$Crop)) {
    unname(unlist(descdist(unlist(subset(Alpha, Date==d & Crop==c, select = Alpha)), graph = FALSE)[6:7])) -> tmp[i,]
    i <- i+1
  }
}

points(tmp[,1]^2, 10-tmp[,2], pch=19, col="red")

## Cullen-Frey plot of Helicity

descdist(unlist(subset(Helicity, Date==Date[1] & Crop==Crop[1], select = Helicity)),
         obs.col = "red", obs.pch = 20)

tmp <- matrix(nrow = 20, ncol=2)
i <- 1
for(d in levels(Helicity$Date)) {
  for(c in levels(Helicity$Crop)) {
    unname(unlist(descdist(unlist(subset(Helicity, Date==d & Crop==c, select = Helicity)), graph = FALSE)[6:7])) -> tmp[i,]
    i <- i+1
  }
}

points(tmp[,1]^2, 10-tmp[,2], pch=19, col="red")

## Cullen-Frey plot of Purity

descdist(unlist(subset(Purity, Date==Date[1] & Crop==Crop[1], select = Purity)),
         obs.col = "red", obs.pch = 20)

tmp <- matrix(nrow = 20, ncol=2)
i <- 1
for(d in levels(Purity$Date)) {
  for(c in levels(Purity$Crop)) {
    unname(unlist(descdist(unlist(subset(Purity, Date==d & Crop==c, select = Purity)), graph = FALSE)[6:7])) -> tmp[i,]
    i <- i+1
  }
}

points(tmp[,1]^2, 10-tmp[,2], pch=19, col="red")

# Back to Ground Zero: Alpha and Helicity are well explained by Beta laws, Purity defies the Pearson System

# Alpha and Helicity on the same Cullen-Frey plot

descdist(unlist(subset(Alpha, Date==Date[1] & Crop==Crop[1], select = Alpha)),
         obs.col = "red", obs.pch = 20)

tmp <- matrix(nrow = 20, ncol=2)
i <- 1
for(d in levels(Alpha$Date)) {
  for(c in levels(Alpha$Crop)) {
    unname(unlist(descdist(unlist(subset(Alpha, Date==d & Crop==c, select = Alpha)), graph = FALSE)[6:7])) -> tmp[i,]
    i <- i+1
  }
}

points(tmp[,1]^2, 10-tmp[,2], pch=19, col="red")

tmp <- matrix(nrow = 20, ncol=2)
i <- 1
for(d in levels(Helicity$Date)) {
  for(c in levels(Helicity$Crop)) {
    unname(unlist(descdist(unlist(subset(Helicity, Date==d & Crop==c, select = Helicity)), graph = FALSE)[6:7])) -> tmp[i,]
    i <- i+1
  }
}

points(tmp[,1]^2, 10-tmp[,2], pch=19, col="blue")

# Go back to the Pearson Plot, and plot Alpha and Helicity on the same plane with different colors ignoring dates
# Maybe add additional lines separating different shapes of the Beta law
# Proceed with the Hyperbolic distribution for Purity

