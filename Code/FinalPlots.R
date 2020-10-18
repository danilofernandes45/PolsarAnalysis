require(ggplot2)
require(gridExtra)
require(hrbrthemes)
require(Rfast)
require(goftest)
require(extrafont)
require(ggrepel)

# Scripts for producing the final files of the GRSL article

### Data for marginal plots
load(file = "../Data/ACFrery/Purity.Rdata")
load(file = "../Data/ACFrery/Alpha.Rdata")
load(file = "../Data/ACFrery/Helicity.Rdata")

### Data for temporal analysis
### Parameter estimation

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

### Marginal plots

PurityPlot <- ggplot(Purity, aes(x=Purity, fill=Date)) +
  geom_density(alpha=.5) +
  labs(x=expression(2016~Purity~italic(P)[GD]), y=expression(Purity~italic(P)[GD]~Estimated~Density)) +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="none", #"bottom" for individual plots
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop)  
PurityPlot

AlphaPlot <- ggplot(Alpha, aes(x=Alpha, fill=Date)) +
  geom_density(alpha=.5) +
  labs(x=expression(2016~Scattering~Type~Angle~alpha[GD]), 
       y=expression(Scattering~Type~Angle~alpha[GD]~Estimated~Density)) +
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

HelicityPlot <- ggplot(Helicity, aes(x=Helicity, fill=Date)) +
  geom_density(alpha=.5) +
  labs(x=expression(2016~Helicity~tau[GD]), 
       y=expression(Helicity~tau[GD]~Estimated~Density)) +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="bottom", #"bottom" for individual plots
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
HelicityPlot

#### Only one plot with grid.arrange

grid.arrange(PurityPlot, AlphaPlot, HelicityPlot, nrow=3)
IndexesPlot <- arrangeGrob(PurityPlot, AlphaPlot, HelicityPlot, nrow=3)
ggsave(file="../Figures/GRSL_2020/FactorPlots/Indexes.pdf", 
       width = 210, height=200, units="mm", device = cairo_pdf, IndexesPlot)

### Temporal analysis
set.seed(3)
#### data.frame for the labels
few.labels.alone <- subset(PurityLogNormalParameters,
                           Crop == "Canola" & Date == "20 August")
few.labels.left <- subset(PurityLogNormalParameters, 
                           Crop=="Oats" & (Date == "16 May" | Date == "9 June"))
few.labels.right <- subset(PurityLogNormalParameters, 
                          Crop=="Oats" & (Date == "3 July" | Date == "27 July"))

PurityTemporal <- ggplot(PurityLogNormalParameters, aes(x=mu, y=sd, group=Date)) +
  geom_label_repel(data=few.labels.alone, aes(x=mu, y=sd, label=Date),
                   direction = "x", nudge_x = .1) +
  geom_label_repel(data=few.labels.right, aes(x=mu, y=sd, label=Date),
                  direction = "x", nudge_x = .10) +
  geom_label_repel(data=few.labels.left, aes(x=mu, y=sd, label=Date),
                  direction = "x", nudge_x = -.1) +
  geom_point(aes(shape=Date, color=Date), size=5,
             shape = 21, fill = "orange",
             color = "black", alpha=.8) +
  geom_text(aes(label=as.integer(Date))) +
  labs(x = expression(widehat(mu)), 
       y = expression(widehat(sigma)),
       title = expression(Estimates~of~the~Lognormal~distribution~"for"~the~Purity~italic(P)[GD])
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

AlphaTemporal <-  ggplot(ScatteringTypeAngleParameters, aes(x=p, y=q, group=Date)) +
  geom_point(aes(shape=Date, color=Date), size=5,
             shape = 21, fill = "orange",
             color = "black", alpha=.7) +
  geom_text(aes(label=as.integer(Date))) +
  labs(x = expression(widehat(p)), 
       y = expression(widehat(q)),
       title = expression(Estimates~of~the~"Beta"~distribution~"for"~the~Scattering~Type~Angle~alpha[GD])
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

HelicityTemporal <- ggplot(HelicityParameters, aes(x=p, y=q)) +
  geom_point(aes(shape=Date, color=Date), size=5,
             shape = 21, fill = "orange", col="black",
             alpha=.7) +
  geom_text(aes(label=as.integer(Date))) +
  labs(x = expression(widehat(p)), y = expression(widehat(q)),
       title = expression(Estimates~of~the~"Beta"~distribution~"for"~the~Helicity~tau[GD])) +
  facet_grid(.~Crop) +
  theme_ipsum(base_family = "Times New Roman", base_size = 8, axis_title_size = 8) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank()
  ) 
HelicityTemporal

#### Only one plot with grid.arrange

grid.arrange(PurityTemporal, AlphaTemporal, HelicityTemporal, nrow=3)
TemporalIndexesPlot <- arrangeGrob(PurityTemporal, AlphaTemporal, HelicityTemporal, nrow=3)
ggsave(file="../Figures/GRSL_2020/FactorPlots/TemporalIndexes.pdf", 
       width = 210, height=200, units="mm", device = cairo_pdf, TemporalIndexesPlot)
