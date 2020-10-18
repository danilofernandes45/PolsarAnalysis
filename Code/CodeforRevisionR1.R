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
             aes(x=Skewness^2, y=Kurtosis), col="brown", size=4)
ggsave(file="../Figures/PearsonPlotAlpha.pdf")

pPearsonPlane +  
  geom_point(data=subset(EmpiricalMoments, Index=="Helicity"),
             aes(x=Skewness^2, y=Kurtosis, col=Crop), size=7)
ggsave(file="../Figures/PearsonPlotHelicity.pdf")

pPearsonPlane +  
  geom_point(data=subset(EmpiricalMoments, Index=="Purity"),
             aes(x=Skewness^2, y=Kurtosis, col=Crop), size=7)
ggsave(file="../Figures/PearsonPlotPurity.pdf")

### Alpha, Helicity, and Purity on the same plot

pPearsonPlane +
  geom_point(data=subset(EmpiricalMoments, Index=="Alpha"),
             aes(x=Skewness^2, y=Kurtosis), col="brown", size=4) +
  geom_point(data=subset(EmpiricalMoments, Index=="Helicity"),
           aes(x=Skewness^2, y=Kurtosis), col="blue", size=4) +
  geom_point(data=subset(EmpiricalMoments, Index=="Purity"),
             aes(x=Skewness^2, y=Kurtosis), col="magenta", size=4, alpha=.7)
ggsave(file="../Figures/PearsonPlotAlphaHelicityPurity.pdf")


theme_set(theme_few(base_size = 16) + 
            theme(legend.position="top"))

### Analysis of Alpha with the Beta distribution

### Sample sizes
for(d in levels(GeodesicMeasures$Date)) {
  for(c in levels(GeodesicMeasures$Crop)) {
    print(paste(d, c, length(which(GeodesicMeasures$Date==d & GeodesicMeasures$Crop==c & GeodesicMeasures$Index=="Alpha"))))
  }
}

require(Rfast)

Alpha <- subset(GeodesicMeasures, Index=="Alpha")[-4]
names(Alpha)[1] <- "Alpha"

ScatteringTypeAngleParameters <- data.frame(NULL)
### Estimating the (mean, variance) for the beta distribution
for(crop in levels(Alpha$Crop)) {
  for(date in levels(Alpha$Date)) {
    sample <- (subset(Alpha, Crop==crop & Date==date)$Alpha)
    pq <- beta.mle(sample)$param
    
    ScatteringTypeAngleParameters <- rbind(
      ScatteringTypeAngleParameters,
      data.frame(p=unname(pq[1]),
                 q=unname(pq[2]),
                 Date=date,
                 Crop=crop)
    )
  }
}
ScatteringTypeAngleParameters$Date <- as.factor(ScatteringTypeAngleParameters$Date)
ScatteringTypeAngleParameters$Crop <- as.factor(ScatteringTypeAngleParameters$Crop)

ScatteringTypeAngleParameters[ScatteringTypeAngleParameters$Crop=="Canola",]
ScatteringTypeAngleParameters[ScatteringTypeAngleParameters$Crop=="Soybeans",]
ScatteringTypeAngleParameters[ScatteringTypeAngleParameters$Crop=="Oats",]
ScatteringTypeAngleParameters[ScatteringTypeAngleParameters$Crop=="Wheat",]

theme_set(theme_few(base_size = 16) + 
            theme(legend.position="top"))

AlphaTemporal <-  ggplot(ScatteringTypeAngleParameters, aes(x=p, y=q, group=Date)) +
  geom_point(size=8,
             shape = 21, fill = "orange",
             color = "black", alpha=.7) +
  geom_text(aes(label=as.integer(Date)), size=6) +
  labs(x = expression(widehat(p)), 
       y = expression(widehat(q)),
       title = expression(Estimates~of~the~"Beta"~distribution~"for"~the~Scattering~Type~Angle~alpha[GD])
  ) +
  facet_grid(.~Crop) #+
#  scale_fill_ipsum() +
#  theme_ipsum(
#    base_family = "Times New Roman",
#    base_size = 10,
#    axis_title_size = 10
#  ) +
  # theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
  #       legend.position="top", #"bottom" for individual plots
  #     #  legend.title=element_blank(),
  #       legend.margin = margin(rep(0,4), "cm")) + 
  # facet_grid(.~ Crop) +
  # theme(
  #   strip.background = element_blank(),
  #   strip.text.x = element_blank()
  # )
AlphaTemporal
ggsave(file="../Figures/AlphaTemporal.pdf", width = 12, height = 4)


### Analysis of Helicity with the Beta distribution
Helicity <- subset(GeodesicMeasures, Index=="Helicity")[-4]
names(Helicity)[1] <- "Helicity"
Helicity$Date <- as.factor(Helicity$Date)
Helicity$Crop <- as.factor(Helicity$Crop)

HelicityParameters <- data.frame(NULL)
### Estimating the (mean, variance) for the beta distribution
for(crop in levels(Helicity$Crop)) {
  for(date in levels(Helicity$Date)) {
    sample <- (subset(Helicity, Crop==crop & Date==date)$Helicity)
    pq <- beta.mle(sample)$param
    
    HelicityParameters <- rbind(
      HelicityParameters,
      data.frame(p=unname(pq[1]),
                 q=unname(pq[2]),
                 Date=date,
                 Crop=crop)
    )
  }
}
HelicityParameters$Date <- as.factor(HelicityParameters$Date)
HelicityParameters$Crop <- as.factor(HelicityParameters$Crop)

options(digits=4)

HelicityParameters[HelicityParameters$Crop=="Canola",]
HelicityParameters[HelicityParameters$Crop=="Soybeans",]
HelicityParameters[HelicityParameters$Crop=="Oats",]
HelicityParameters[HelicityParameters$Crop=="Wheat",]


HelicityTemporal <- ggplot(HelicityParameters, aes(x=p, y=q)) +
  geom_point(aes(shape=Date, color=Date), size=8,
             shape = 21, fill = "orange", col="black",
             alpha=.7) +
  geom_text(aes(label=as.integer(Date)), size=6) +
  labs(x = expression(widehat(p)), y = expression(widehat(q)),
       title = expression(Estimates~of~the~"Beta"~distribution~"for"~the~Helicity~tau[GD])) +
  facet_grid(.~Crop) #+
  # theme_ipsum(base_family = "Times New Roman", base_size = 8, axis_title_size = 8) +
  # scale_fill_ipsum() +
  # theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
  #       legend.position="top",
  #      # legend.title=element_blank(),
  #       legend.margin = margin(rep(0,4), "cm"),
  #       strip.background = element_blank(),
  #       strip.text.x = element_blank()
  # ) 
HelicityTemporal
ggsave(file="../Figures/HelicityTemporal.pdf", width = 12, height = 4)

### Using the hyperbolic distribution

require(GeneralizedHyperbolic)

# Verification
hyperbFit(sample, method = "BFGS") -> paramHyperb
paramHyperb$param
hyperbChangePars(from=2, to=1, paramHyperb$param)

### Analysis of Purity with the Hyperbolix distribution
Purity <- subset(GeodesicMeasures, Index=="Purity")[-4]
names(Purity)[1] <- "Purity"
Purity$Date <- as.factor(Purity$Date)
Purity$Crop <- as.factor(Purity$Crop)

PurityParameters <- data.frame(NULL)
### Estimating the (mean, variance) for the beta distribution
for(crop in levels(Purity$Crop)) {
  for(date in levels(Purity$Date)) {
    sample <- (subset(Purity, Crop==crop & Date==date)$Purity)
    mu.delta.Hpi.zeta <- unname(hyperbChangePars(from=2, to=1, hyperbFit(sample, method = "BFGS", plots=TRUE)$param))
    invisible(readline(prompt="Press [enter] to continue"))
    
    PurityParameters <- rbind(
      PurityParameters,
      data.frame(mu=mu.delta.Hpi.zeta[1],
                 delta=mu.delta.Hpi.zeta[2],
                 Hpi=mu.delta.Hpi.zeta[3],
                 zeta=mu.delta.Hpi.zeta[4],
                 Date=date,
                 Crop=crop)
    )
  }
}
PurityParameters$Date <- as.factor(PurityParameters$Date)
PurityParameters$Crop <- as.factor(PurityParameters$Crop)

t(PurityParameters[PurityParameters$Crop=="Canola",])[,c(1,5:2)]
t(PurityParameters[PurityParameters$Crop=="Soybeans",])[,c(1,5:2)]
t(PurityParameters[PurityParameters$Crop=="Oats",])[,c(1,5:2)]
t(PurityParameters[PurityParameters$Crop=="Wheat",])[,c(1,5:2)]

sample <- (subset(Purity, Crop=="Canola" & Date=="16 May")$Purity)
mu.delta.Hpi.zeta <- unname(hyperbChangePars(from=2, to=1, hyperbFit(sample, method = "BFGS")$param))

theme_set(theme_tufte(base_size = 16) + 
            theme(legend.position="top"))

ggplot(data.frame(sample)) +
  aes(x=sample) +
  geom_histogram(aes(y=..density..), bins=nclass.FD(sample), fill="white", col="black") +
  stat_function(fun="dhyperb", args=list(mu=hyperbChangePars(from=1, to=2, mu.delta.Hpi.zeta)[1],
                                         delta=hyperbChangePars(from=1, to=2, mu.delta.Hpi.zeta)[2],
                                         alpha=hyperbChangePars(from=1, to=2, mu.delta.Hpi.zeta)[3],
                                         beta=hyperbChangePars(from=1, to=2, mu.delta.Hpi.zeta)[4]),
                n=500, col="darkgreen", size=6, alpha=.7) +
  xlim(.5, 1.5) +
  labs(title="Purity, Canola, 16 May",
       x="Observation",
       y="Histogram and Fitted Hyperbolic Density")
ggsave(file="../Figures/PurityCanola16May.pdf")

ggplot(data.frame(sample)) +
  aes(x=sample) +
  geom_histogram(aes(y=..density..), bins=nclass.FD(sample), fill="white", col="black") +
  stat_function(fun="dhyperb", args=list(mu=hyperbChangePars(from=1, to=2, mu.delta.Hpi.zeta)[1],
                                         delta=hyperbChangePars(from=1, to=2, mu.delta.Hpi.zeta)[2],
                                         alpha=hyperbChangePars(from=1, to=2, mu.delta.Hpi.zeta)[3],
                                         beta=hyperbChangePars(from=1, to=2, mu.delta.Hpi.zeta)[4]),
                n=500, col="darkgreen", size=6, alpha=.7) +
  xlim(.5, 1.5) +
  labs(x="Observation",
       y="Histogram and Fitted Hyperbolic Log-Density") +
  scale_y_log10()
  ggsave(file="../Figures/LogPurityCanola16May.pdf")

### Temporal Analysis of Purity
  
head(PurityParameters)

PCA.Purity <- prcomp(PurityParameters[,1:4])
100*PCA.Purity$sdev / sum(PCA.Purity$sdev)
head(PCA.Purity$x)
summary(PCA.Purity$x[,1])

PurityParameters$Date <- as.factor(PurityParameters$Date)
PurityParameters$Crop <- as.factor(PurityParameters$Crop)

head(PurityParameters)

PurityParameters$PC1 <- PCA.Purity$x[,1]
PurityParameters$PC2 <- PCA.Purity$x[,2]
PurityParameters$PC3 <- PCA.Purity$x[,3]
PurityParameters$PC4 <- PCA.Purity$x[,4]

HyperbolicPCATemporal <- ggplot(PurityParameters, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=Date, color=Date), size=8,
             shape = 21, fill = "orange", col="black",
             alpha=.7) +
  geom_text(aes(label=as.integer(Date)), size=6) +
  labs(x = "PC1", 
       y = "PC2",
       title = "First and Second Principal Components") +
  facet_grid(.~Crop)
HyperbolicPCATemporal
ggsave(file="../Figures/HyperbolicPCATemporal.pdf", width = 12, height = 4)


