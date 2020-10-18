require(PearsonDS)
require(ggplot2)
require(ggthemes)
  theme_set(theme_tufte(base_size = 16) + 
              theme(legend.position="top"))

load("../Data/ACFrery/Indices_Sample.RData")
GeodesicMeasures <- sample
rm(sample)
GeodesicMeasures$Observation <- as.numeric(paste(GeodesicMeasures$Observation))
summary(GeodesicMeasures)

# A new dataframe with the transformed Purity

GeodesicMeasuresP <- GeodesicMeasures
levels(GeodesicMeasuresP$Index)[levels(GeodesicMeasuresP$Index)=="Purity"] <- "PurityP"
levels(GeodesicMeasuresP$Index)

GeodesicMeasuresP$Observation[GeodesicMeasuresP$Index=="PurityP"] <- 
  sqrt(GeodesicMeasuresP$Observation[GeodesicMeasuresP$Index=="PurityP"])*2/3

summary(GeodesicMeasures$Observation[GeodesicMeasures$Index=="Purity"])
summary(GeodesicMeasuresP$Observation[GeodesicMeasuresP$Index=="PurityP"])



# AlphaCanola16May <- subset(GeodesicMeasures, Index=="Alpha" & Crop=="Canola" & Date=="16 May")[,1]
# summary(AlphaCanola16May)
# rm(AlphaCanola16May)
# HelicityCanola16May <- subset(GeodesicMeasures, Index=="Helicity" & Crop=="Canola" & Date=="16 May")[,1]
# summary(HelicityCanola16May)
# rm(HelicityCanola16May)
# PurityCanola16May <- subset(GeodesicMeasures, Index=="Purity" & Crop=="Canola" & Date=="16 May")[,1]
# summary(PurityCanola16May)
# rm(PurityCanola16May) 
                               

### Check the use of model selection
#pearsonMSC(AlphaCanola16May)

# Plot each sample in the Pearson system diagram: One diagram for each type of measure

### Create a data.frame with the four empirical moments for each sample by each interaction of the factors

instances <- length(levels(GeodesicMeasures$Date)) * 
  length(levels(GeodesicMeasures$Crop)) *
  length(levels(GeodesicMeasures$Index))

EmpiricalMoments <- data.frame(
  Mean=rep(0., instances), 
  Variance=rep(0., instances), 
  Skewness=rep(0., instances), 
  Kurtosis=rep(0., instances), 
  Date=rep("", instances), 
  Crop=rep("", instances),
  Index=rep("", instances)
)

i <- 1
for(date in levels(GeodesicMeasures$Date)) {
  for(crop in levels(GeodesicMeasures$Crop)) {
      for(index in levels(GeodesicMeasures$Index)) {
        sample <- subset(GeodesicMeasures, Date==date & Crop==crop & Index==index)[,1]
        EmpiricalMoments[i, 1:4] <- unname(empMoments(sample))
        EmpiricalMoments[i, 5:7] <- c(date, crop, index)
        i <- i+1
    }
  }
}
EmpiricalMoments$Date <- as.factor(EmpiricalMoments$Date)
EmpiricalMoments$Crop <- as.factor(EmpiricalMoments$Crop)
EmpiricalMoments$Index <- as.factor(EmpiricalMoments$Index)

summary(EmpiricalMoments)

### The best fit according to the likelihood
BestFit <- data.frame(
  Best.Fit.Type=rep("", instances), 
  Date=rep("", instances), 
  Crop=rep("", instances),
  Index=rep("", instances)
)

i <- 1
for(date in levels(GeodesicMeasures$Date)) {
  for(crop in levels(GeodesicMeasures$Crop)) {
    for(index in levels(GeodesicMeasures$Index)) {
      sample <- subset(GeodesicMeasures, Date==date & Crop==crop & Index==index)[,1]
      BestFit[i, 1] <- unname((pearsonMSC(sample)$Best)$AICc[1])
      BestFit[i, 2:4] <- c(date, crop, index)
      i <- i+1
    }
  }
}

BestFit$Best.Fit.Type <- as.factor(BestFit$Best.Fit.Type)
BestFit$Date <- as.factor(BestFit$Date)
BestFit$Crop <- as.factor(BestFit$Crop)
BestFit$Index <- as.factor(BestFit$Index)

summary(BestFit)
table(BestFit$Best.Fit.Type)
table(BestFit$Best.Fit.Type, BestFit$Index)
table(BestFit$Best.Fit.Type, BestFit$Crop, BestFit$Index)
table(BestFit$Crop, BestFit$Best.Fit.Type, BestFit$Index)
table(BestFit$Index, BestFit$Crop, BestFit$Best.Fit.Type)

###


### Using MyPearsonPlot

#source("../../../../Documents/Programas/R/MyPearsonPlot.R")
b1max <- 10
npoints <- 1000
a <- seq(6.4, 1000, length.out=npoints)
b1 <- seq(from=.001, to=b1max, length.out=npoints)
b2.BetaInf <- b1+1
b2.BetaSup <- 3/2*b1+3
b2.TypeIII <- 3 + 1.5*b1
b1.TypeV <- 16*(a-2) / (a-3)^2
b2.TypeV <- 3+6*(5*a-11) / ((a-3)*(a-4))
PearsonLines <- data.frame(b1, b2.BetaInf, b2.BetaSup, b2.TypeIII, b1.TypeV, b2.TypeV)

pPearsonPlane <- ggplot(data=PearsonLines) +
  # Areas
  ## Unfeasible
  geom_ribbon(aes(x=b1, ymin=rep(0, length(b1)), ymax=b2.BetaInf), 
              fill="gray") +
  ## Type I
  geom_ribbon(aes(x=b1, ymin=b2.BetaInf, ymax=b2.BetaSup), 
              fill="yellow", alpha=.5) +
  ## Type IV
  geom_ribbon(aes(x=b1.TypeV, ymin=b2.TypeV, ymax=rep(max(b2.TypeV), length(b2.TypeV))),
              fill="purple", alpha=.3) +
  # Lines
  ## Type V
  geom_line(aes(x=b1.TypeV, y=b2.TypeV), col="purple", size=2) +
  ## Type I
  geom_line(aes(x=b1, y=b2.BetaInf), col="black", size=2) +
  geom_line(aes(x=b1, y=b2.BetaSup), col="red", size=2) +
  ## Type II
  geom_segment(aes(x=0, xend=0, y=1, yend=3), 
               col="red", size=2) +
  # Points
  geom_point(aes(x=0, y=3), size=5) + # Gaussian distribution
  geom_point(aes(x=0, y=6), size=5) + # Laplace distribution
  geom_point(aes(x=0, y=4.2), size=5) + # Logistic distribution
  geom_point(aes(x=0, y=-6/5+3), size=5) + # Uniform distribution
  # Limits
  coord_cartesian(
    xlim=c(-.3, 4),
    ylim=c(1, 9),
    expand = TRUE,
    default = TRUE,
    clip = "on"
  ) +
  # Labels
  geom_text(aes(x=2.5, y=1.5, label="Unfeasible"), size=8) +
  geom_text(aes(x=2, y=4, label="Type I"), size=8) +
  geom_text(aes(x=-.05, y=2.5, label="Type II", hjust="right"), col="red", size=8) +
  geom_text(aes(x=3.6, y=7.7, label="Type III"), col="red", size=8) +
  geom_text(aes(x=.5, y=8, label="Type IV"), size=8) +
  geom_text(aes(x=2.1, y=7.5, label="Type V", hjust="right"), col="purple", size=8) +
  geom_text(aes(x=3, y=8.5, label="Type VI"), size=8) +
  geom_text(aes(x=-.05, y=3, label="Normal", hjust="right"), size=8) +
  geom_text(aes(x=-.05, y=6, label="Laplace", hjust="right"), size=8) +
  geom_text(aes(x=-.05, y=4.2, label="Logistic", hjust="right"), size=8) +
  geom_text(aes(x=-.05, y=-6/5+3, label="Uniform", hjust="right"), size=8) +
  xlab(expression(beta[1])) + ylab(expression(beta[2])) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=24,face="bold"),
        legend.title=element_text(size=24),
        legend.text=element_text(size=18))


### Fit Alpha, Helicity, and Purity in the Pearson Plane

pPearsonPlane +
  geom_point(data=subset(EmpiricalMoments, Index=="Alpha"),
             aes(x=Skewness^2, y=Kurtosis, col=Crop), size=7)
ggsave(file="../Figures/PearsonPlotAlpha.pdf")

pPearsonPlane +  
  geom_point(data=subset(EmpiricalMoments, Index=="Helicity"),
             aes(x=Skewness^2, y=Kurtosis, col=Crop), size=7)
ggsave(file="../Figures/PearsonPlotHelicity.pdf")

Param.Lognormal.sigma <- seq(.0001, 1, by=.001)
Line.Lognormal <- data.frame(beta1=(exp(Param.Lognormal.sigma^2)-1)*(2+exp(Param.Lognormal.sigma^2)^2),
                             beta2=exp(4*Param.Lognormal.sigma^2) +
                               2*exp(3*Param.Lognormal.sigma^2) +
                               3*exp(2*Param.Lognormal.sigma^2)-3)


pPearsonPlane +  
#  geom_line(data=Line.Lognormal, aes(x=beta1, y=beta2), size=2, col="blue") + # Lognormal distribution
#  geom_text(aes(x=1.4, y=9, label="LogNor"), col="blue", size=8) +
  geom_point(data=subset(EmpiricalMoments, Index=="Purity"),
             aes(x=Skewness^2, y=Kurtosis, col=Crop), size=7)
ggsave(file="../Figures/PearsonPlotPurity.pdf")

ggplot(subset(EmpiricalMoments, Index=="PurityP")) +
  aes(x=Skewness^2, y=Kurtosis) +
  geom_point() +
  stat_smooth(method="lm")

### Another approach: using the fitdistrplus package
require(fitdistrplus)

for(d in levels(GeodesicMeasures$Date)) {
  for(c in levels(GeodesicMeasures$Crop)) {
    sample <- unlist(subset(GeodesicMeasures, Date==d & Crop==c, select=Observation))
    # sample <- GeodesicMeasures[GeodesicMeasures$Date==d & GeodesicMeasures$Crop==c,1]
    print(paste("Crop= ", c, "; Date= ", d))
    print(summary(sample))
    descdist(sample, boot = 1000)
    invisible(readline(prompt="Press [enter] to continue"))
  }
}

### It seems all but one sample is well described by Beta laws. Back to the Pearson Plane




