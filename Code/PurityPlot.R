### This script collects all the data in a single data frame

### Purity - Danilo 18/03/20

PurityCanola$Crop <- "Canola"
PurityOats$Crop <- "Oats"
PuritySoyBeans$Crop <- "SoyBeans"
PurityWheat$Crop <- "Wheat"

Purity <- rbind(PurityCanola, PurityOats, PuritySoyBeans, PurityWheat)
Purity$Crop <- as.factor(Purity$Crop)

PurityPlot <- ggplot(Purity, aes(x=Purity, fill=Date)) +
  geom_density(alpha=.5) +
  labs(x="Purity 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
PurityPlot + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop)  
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
  labs(x="Alpha 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

AlphaPlot + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop)  
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
  labs(x="Helicity 2016", y="Estimated Density") +
  theme_ipsum(base_family = "Times New Roman", 
              base_size = 10, axis_title_size = 10) +
  scale_fill_ipsum() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

HelicityPlot + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.margin = margin(rep(0,4), "cm")) + 
  facet_grid(.~ Crop)  
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/Helicity.pdf", 
       width = 210, height=60, units="mm", device = cairo_pdf)


#### Only one plot

Purity$Index <- "Purity"
names(Purity) <- c("Observation", "Date", "Crop", "Index")
Alpha$Index <- "Alpha"
names(Alpha) <- c("Observation", "Date", "Crop", "Index")
Helicity$Index <- "Helicity"
names(Helicity) <- c("Observation", "Date", "Crop", "Index")

GeodesicIndexes <- rbind(Purity, Alpha, Helicity)
GeodesicIndexes$Index <- as.factor(GeodesicIndexes$Index)

save(file="/Users/acfrery/Documents/Alunos/Danilo Fernandes/Data/ACFrery/GeodesicIndexes.Rdata", GeodesicIndexes)
