### This script collects all the data in a single data frame

require(gridExtra)

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
  geom_boxplot(alpha=.5) +
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
  require(Rfast)
  
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
  require(Rfast)
  
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
    labs(x = expression(alpha[GD] ~ Helicity ~ 16 ~ May), y = "Histogram and Fitted Beta Density") +
    theme_ipsum(
      base_family = "Times New Roman",
      base_size = 20,
      axis_title_size = 20
    ) +
    theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  ggsave(
    file = "../../../../Figures/GRSL_2020/FactorPlots/CanolaHelicityBetaFit.pdf",
    width = 210,
    height = 140,
    units = "mm",
    device = cairo_pdf
  )

### Estimating (p,q) and the KS test for Purity/2
  for(crop in levels(Purity$Crop)) {
    for(date in levels(Purity$Date)) {
      sample <- (subset(Purity, Crop==crop & Date==date)$Purity) / 2
      n <- length(sample)
      pq <- beta.mle(sample)$param 
      p.value <- ks.test(sample, pbeta, pq[1], pq[2])$p.value
      column <- round(c(n, pq, p.value), 4)
      print(c(crop, date, column), quote=FALSE)
    }
  }
  