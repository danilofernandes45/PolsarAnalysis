### This script collects all the data in a single data frame

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
  
PurityPlot + facet_grid(.~ Crop)  
ggsave(file="../../../../Figures/GRSL_2020/FactorPlots/Purity.pdf", 
       width = 210, height=60, units="mm")
