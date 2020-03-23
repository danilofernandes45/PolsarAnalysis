### This script collects all the data in a single data frame
require(ggplot2)
require(gridExtra)
require(hrbrthemes)
require(Rfast)
require(goftest)
require(extrafont)


### Purity - Danilo 18/03/20

PurityCanola$Crop <- "Canola"
PurityOats$Crop <- "Oats"
PuritySoyBeans$Crop <- "SoyBeans"
PurityWheat$Crop <- "Wheat"

Purity <- rbind(PurityCanola, PurityOats, PuritySoyBeans, PurityWheat)
Purity$Crop <- as.factor(Purity$Crop)

PurityPlot <- ggplot(Purity, aes(x=Purity, fill=Date)) +
  geom_density(alpha=.5) +
  labs(x="Purity 2016", y="Purity Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="none", #"bottom" for individual plots
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop)  
PurityPlot

ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/Purity.pdf", 
       width = 210, height=60, units="mm", device = cairo_pdf)

### Alpha

AlphaCanola$Crop <- "Canola"
AlphaOats$Crop <- "Oats"
AlphaSoyBeans$Crop <- "SoyBeans"
AlphaWheat$Crop <- "Wheat"

Alpha <- rbind(AlphaCanola, AlphaOats, AlphaSoyBeans, AlphaWheat)
Alpha$Crop <- as.factor(Alpha$Crop)

AlphaPlot <- ggplot(Alpha, aes(x=Alpha, fill=Date)) +
  geom_density(alpha=.5) +
  labs(x="Alpha 2016", y="Alpha Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="none", #"bottom" for individual plots
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
AlphaPlot

ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/Alpha.pdf", 
       width = 210, height=60, units="mm", device = cairo_pdf)

### Helicity

HelicityCanola$Crop <- "Canola"
HelicityOats$Crop <- "Oats"
HelicitySoyBeans$Crop <- "SoyBeans"
HelicityWheat$Crop <- "Wheat"

Helicity <- rbind(HelicityCanola, HelicityOats, HelicitySoyBeans, HelicityWheat)
Helicity$Crop <- as.factor(Helicity$Crop)

HelicityPlot <- ggplot(Helicity, aes(x=Helicity, fill=Date)) +
  geom_density(alpha=.5) +
  labs(x="Helicity 2016", y="Helicity Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) 
HelicityPlot

ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/Helicity.pdf", 
       width = 210, height=60, units="mm", device = cairo_pdf)


#### Only one plot with facet

Purity$Index <- "Purity"
names(Purity) <- c("Observation", "Date", "Crop", "Index")
Alpha$Index <- "Alpha"
names(Alpha) <- c("Observation", "Date", "Crop", "Index")
Helicity$Index <- "Helicity"
names(Helicity) <- c("Observation", "Date", "Crop", "Index")

GeodesicIndexes <- rbind(Purity, Alpha, Helicity)
GeodesicIndexes$Index <- as.factor(GeodesicIndexes$Index)

save(file="/Users/acfrery/Documents/Alunos/Danilo Fernandes/Data/ACFrery/GeodesicIndexes.Rdata", GeodesicIndexes)

ggplot(GeodesicIndexes, aes(x=Observation, fill=Date)) +
  geom_density(alpha=.5) +
#  labs(x="Helicity 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(rep(0, 4), "cm")
  ) +
  facet_grid(Index ~ Crop)
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/BoxPlotIndexes.pdf", 
       width = 210, height=200, units="mm", device = cairo_pdf)

#### Only one plot with grid.arrange

grid.arrange(PurityPlot, AlphaPlot, HelicityPlot, nrow=3)
IndexesPlot <- arrangeGrob(PurityPlot, AlphaPlot, HelicityPlot, nrow=3)
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/Indexes.pdf", 
       width = 210, height=200, units="mm", device = cairo_pdf, IndexesPlot)


### logarithm of Purity
ggplot(subset(Purity, Date=="9 June"), aes(Purity, fill=Crop)) +
#  geom_histogram(alpha=.5, position="identity") +
  geom_density(alpha=.5) +
  scale_x_continuous(trans="log10") +
  labs(x="Purity 2016", y="Purity Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="right",
        legend.margin = margin(rep(0,4), "cm")) #+ 
  facet_grid(.~ Crop)  


### Reestimating (p,q) and the KS test for Alpha
for(crop in levels(Alpha$Crop)) {
  for(date in levels(Alpha$Date)) {
    sample <- subset(Alpha, Crop==crop & Date==date)$Alpha
    n <- length(sample)
    pq <- beta.mle(sample)$param 
    p.value <- ks.test(sample, pbeta, pq[1], pq[2])$p.value
    column <- round(c(n, pq, p.value), 4)
    print(c(crop, date, column), quote=FALSE)
  }
}

  ### Analysis of Alpha from Canola 16 May (the worst fit)

  
  Canola16MayAlpha <-
    data.frame(Alpha = subset(Alpha, Crop == "Canola" &
                                Date == "16 May")$Alpha)
  pq <- beta.mle(Canola16MayAlpha$Alpha)$param
  minmax <- range(Canola16MayAlpha$Alpha)
  nbins.fd <-
    ceiling((length(Canola16MayAlpha$Alpha) ^ (1 / 3) * (minmax[2] - minmax[1])) / (2 * IQR(Canola16MayAlpha$Alpha)))
  
  ggplot(Canola16MayAlpha, aes(x = Alpha)) +
    geom_histogram(aes(y = ..density..),
                   bins = nbins.fd,
                   fill = "white",
                   col = "black") +
    stat_function(
      fun = dbeta,
      args = list(shape1 = pq[1], shape2 = pq[2]),
      size = 3,
      col = "lightgoldenrod4"
    ) +
    labs(x = expression(alpha[GD] ~ Canola ~ 16 ~ May), y = "Histogram and Fitted Beta Density") +
    theme_ipsum(
      base_family = "Times New Roman",
      base_size = 20,
      axis_title_size = 20
    ) +
    theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  ggsave(
    file = "../../../../Figures/GRSL_2020/FactorPlots/CanolaAlphaBetaFit.pdf",
    width = 210,
    height = 140,
    units = "mm",
    device = cairo_pdf
  )
  
### Estimating (p,q) and the KS test for Helicity
  for(crop in levels(Helicity$Crop)) {
    for(date in levels(Helicity$Date)) {
      sample <- subset(Helicity, Crop==crop & Date==date)$Helicity
      n <- length(sample)
      pq <- beta.mle(sample)$param 
      p.value <- ks.test(sample, pbeta, pq[1], pq[2])$p.value
      column <- round(c(n, pq, p.value), 4)
      print(c(crop, date, column), quote=FALSE)
    }
  }
  
  ### Analysis of Helicity from Canola 16 May (the worst fit)

  Canola16MayHelicity <-
    data.frame(Helicity = subset(Helicity, Crop == "Canola" &
                                Date == "16 May")$Helicity)
  pq <- beta.mle(Canola16MayHelicity$Helicity)$param
  minmax <- range(Canola16MayHelicity$Helicity)
  nbins.fd <-
    ceiling((length(Canola16MayHelicity$Helicity) ^ (1 / 3) * (minmax[2] - minmax[1])) / (2 * IQR(Canola16MayHelicity$Helicity)))
  
  ggplot(Canola16MayHelicity, aes(x = Helicity)) +
    geom_histogram(aes(y = ..density..),
                   bins = nbins.fd,
                   fill = "white",
                   col = "black") +
    stat_function(
      fun = dbeta,
      args = list(shape1 = pq[1], shape2 = pq[2]),
      size = 3,
      col = "lightgoldenrod4"
    ) +
    labs(x = expression(tau[GD] ~ Canola ~ 16 ~ May), y = "Histogram and Fitted Beta Density") +
    theme_ipsum(
      base_family = "Times New Roman",
      base_size = 20,
      axis_title_size = 20
    ) +
    theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  ggsave(
    file = "../Figures/GRSL_2020/FactorPlots/CanolaHelicityBetaFit.pdf",
    width = 210,
    height = 140,
    units = "mm",
    device = cairo_pdf
  )

  

### Analysis of Purity samples (Alejandro 23 March 2020)

  ggplot(Purity, aes(x=Purity, fill=Date)) +
    geom_density(alpha=.5) +
    labs(x="Purity 2016", y="Purity Estimated Density") +
#    scale_x_continuous(trans="log10") + # uncomment for log10 scale
    theme_ipsum(base_family = "Times New Roman", 
                base_size = 10, axis_title_size = 10) +
    scale_fill_ipsum() +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.position="none", #"bottom" for individual plots
          legend.title=element_blank(),
          legend.margin = margin(rep(0,4), "cm")) + 
    facet_grid(.~ Crop) 
  
PurityLogNormalParameters <- data.frame(NULL)
  ### Estimating the (mean, variance) for the lognormal distribution and the KS test for Purity/2.25
  for(crop in levels(Purity$Crop)) {
    for(date in levels(Purity$Date)) {
      sample <- (subset(Purity, Crop==crop & Date==date)$Purity)
      n <- length(sample)
      pq <- lognorm.mle(sample)$param
      
      PurityLogNormalParameters <- rbind(
        PurityLogNormalParameters,
        data.frame(mu=unname(pq[1]),
                   sd=sqrt(unname(pq[2])),
                   Date=date,
                   Crop=crop)
      )
      
      p.value <- ks.test(sample, plnorm, pq[1], pq[2])$p.value
      column <- round(c(n, pq, p.value), 4)
      print(c(crop, date, column), quote=FALSE)
    }
  } ### All p-values = 0

# Temporal analysis of Purity parameters
PurityTemporal <- ggplot(PurityLogNormalParameters, aes(x=mu, y=sd, group=Date)) +
  geom_point(aes(shape=Date, color=Date), size=3) +
  labs(x = expression(widehat(mu)), 
       y = expression(widehat(sigma))
       ) +
  facet_grid(.~Crop) +
  scale_fill_ipsum() +
  theme_ipsum(
    base_family = "Times New Roman",
    base_size = 10,
    axis_title_size = 10
  ) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="none", #"bottom" for individual plots
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) 
PurityTemporal

ggsave(
  file = "../Figures/GRSL_2020/FactorPlots/TemporalLogNormalPurityParameters.pdf",
  width = 210, height = 60, units = "mm", device = cairo_pdf
)

### First, log10 transformation, then  
### estimating the (mean, variance) for the normal distribution and the KS test for Purity
  for(crop in levels(Purity$Crop)) {
    for(date in levels(Purity$Date)) {
      sample <- log10((subset(Purity, Crop==crop & Date==date)$Purity))
      n <- length(sample)
      pq <- normal.mle(sample)$param 
      p.value <- ad.test(sample, null = "pnorm", mean=pq[1], sd=sqrt(pq[2]),
                         estimated=TRUE)$p.value
      column <- round(c(n, pq, p.value), 4)
      print(c(crop, date, column), quote=FALSE)
    }
  } ### Excellent p-values

  ### Estimating the (mean, variance) for the lognormal distribution and the KS test for Purity
  for(crop in levels(Purity$Crop)) {
    for(date in levels(Purity$Date)) {
      sample <- subset(Purity, Crop==crop & Date==date)$Purity
      n <- length(sample)
      pq <- lognorm.mle(sample)$param 
      p.value <- ad.test(sample, null = "plnorm", mean=pq[1], sd=sqrt(pq[2]),
                         estimated=TRUE)$p.value
      column <- round(c(n, pq, p.value), 4)
      print(c(crop, date, column), quote=FALSE)
    }
  } ### Excellent p-values
  
### Fitting the worst Lognormal model to Purity Wheat 16 May 
  
  Wheat16MayPurity <-
    data.frame(Purity = subset(Purity, Crop == "Wheat" &
                                   Date == "16 May")$Purity)
  pq <- lognorm.mle(Wheat16MayPurity$Purity)$param 
  minmax <- range(Wheat16MayPurity$Purity)
  nbins.fd <-
    ceiling((length(Wheat16MayPurity$Purity)^(1 / 3) * (minmax[2] - minmax[1])) / (2 * IQR(Wheat16MayPurity$Purity)))
  
  ggplot(Wheat16MayPurity, aes(x = Purity)) +
    geom_histogram(aes(y = ..density..),
                   bins = nbins.fd/2,
                   fill = "white",
                   col = "black") +
    stat_function(
      fun = dlnorm,
      args = list(meanlog = pq[1], sdlog = sqrt(pq[2])),
      size = 3,
      col = "wheat4"
    ) +
    labs(x = expression(italic(P)[GD] ~ Wheat ~ 16 ~ May), 
         y = "Histogram and Fitted Lognormal Density") +
    theme_ipsum(
      base_family = "Times New Roman",
      base_size = 20,
      axis_title_size = 20
    ) +
    theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  ggsave(
    file = "../Figures/GRSL_2020/FactorPlots/WheatPurityLognormalFit.pdf", 
    width = 210, height = 140, units = "mm", device = cairo_pdf
  )

### Temporal Analysis Alpha
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
  
  # Temporal analysis of Alpha parameters
AlphaTemporal <-  ggplot(ScatteringTypeAngleParameters, aes(x=p, y=q, group=Date)) +
    geom_point(aes(shape=Date, color=Date), size=3) +
    labs(x = expression(widehat(p)), 
         y = expression(widehat(q))
    ) +
    facet_grid(.~Crop) +
    scale_fill_ipsum() +
    theme_ipsum(
      base_family = "Times New Roman",
      base_size = 10,
      axis_title_size = 10
    ) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
          legend.position="none", #"bottom" for individual plots
          legend.title=element_blank(),
          legend.margin = margin(rep(0,4), "cm")) + 
    facet_grid(.~ Crop) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

AlphaTemporal
  
ggsave(file = "../Figures/GRSL_2020/FactorPlots/TemporalBetaScatteringTypeAngleParameters.pdf",
    width = 210, height = 60, units = "mm", device = cairo_pdf)

### Temporal Analysis Helicity
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

# Temporal analysis of Helicity parameters
HelicityTemporal <- ggplot(HelicityParameters, aes(x=p, y=q, group=Date)) +
  geom_point(aes(shape=Date, color=Date), size=3) +
  labs(x = expression(widehat(p)), 
       y = expression(widehat(q))
  ) +
  facet_grid(.~Crop) +
  theme_ipsum(
    base_family = "Times New Roman",
    base_size = 8,
    axis_title_size = 8
  ) +
  scale_fill_ipsum() +
  theme_ipsum(
    base_family = "Times New Roman",
    base_size = 10,
    axis_title_size = 10
  ) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="bottom", #"bottom" for individual plots
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
HelicityTemporal

ggsave(file = "../Figures/GRSL_2020/FactorPlots/TemporalBetaHelicityParameters.pdf",
       width = 210, height = 60, units = "mm", device = cairo_pdf)

#### Only one plot with grid.arrange

grid.arrange(PurityTemporal, AlphaTemporal, HelicityTemporal, nrow=3)
TemporalIndexesPlot <- arrangeGrob(PurityTemporal, AlphaTemporal, HelicityTemporal, nrow=3)
ggsave(file="../Figures/GRSL_2020/FactorPlots/TemporalIndexes.pdf", 
       width = 210, height=200, units="mm", device = cairo_pdf, TemporalIndexesPlot)
