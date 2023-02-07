
library(tidyverse)
library(here)
library(patchwork)
library(scales)
library(viridis)

show_col(viridis_pal(option = "B")(9))


all_dat <- read_csv(here('data/processed_data', 'BP_all_June72022.csv'))


#UV 254 plot by DOY 
UV254 <- ggplot(all_dat, aes(DOY, UV_254, colour = factor(Year))) +
  geom_point() + stat_ellipse() + 
  #geom_point(data = mean_UV254, aes(x = mean_DOY, y = mean_UV254), size = 5)+
  scale_colour_viridis_d(option = "plasma", direction = 1) + 
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275)) + 
  labs(colour = "Year") + 
  labs(x = "Day of Year", y = expression(bold("UV Absorbance at 254 nm"~(dm^-1))))+ 
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        panel.background = element_blank())



#SpCond plot by DOY 
SpCond <- ggplot(all_dat, aes(DOY, SpCondShallow, colour = factor(Year))) +
  geom_point() + #stat_ellipse() + 
  scale_colour_viridis_d(option = "plasma", direction = 1) + 
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275)) + 
  labs(colour = "Year") + 
  labs(x = "Day of Year", y = "Specific Conductivity (µS/cm)")+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12),
        panel.background = element_blank())


#Sulfate vs Conductivity plot 
mean_sulph <- all_dat %>% group_by(Year) %>% summarise("mean_Sulphate" = mean(Sulphate, na.rm = TRUE),
                                                       "mean_SpCondShallow" = mean(SpCondShallow, na.rm = TRUE)) 

Sulph <- ggplot(all_dat, aes(SpCondShallow, Sulphate, colour = factor(Year))) +
  geom_point(size = 2.5) + 
  #not run (adds arrows and centroids indicating direction of change)
  #geom_curve(aes(x = 607.4092, y = 158.0400, xend = 912.7121, yend = 266.8500), colour = "#0D0887FF", curvature = .8,  size = 1, arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  #geom_curve(aes(x = 912.7121, y = 266.8500, xend = 796.1487, yend = 222.4800), colour = "#5D01A6FF", curvature = 0.75, size = 1, arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  #geom_curve(aes(x = 796.1487, y = 222.4800, xend = 873.6846, yend = 252.8800), colour = "#9C179EFF", curvature = 0.75, size = 1, arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  #geom_curve(aes(x = 873.6846, y = 252.8800, xend = 614.8507, yend = 150.8125), colour = "#CC4678FF", curvature = 1.25, size = 1,  arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  #geom_curve(aes(x = 614.8507, y = 150.8125, xend = 497.7493, yend = 105.0000), colour = "#ED7953FF", curvature = -0.75, size =1, arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  #geom_curve(aes(x = 497.7493, y = 105.0000, xend = 502.8972, yend = 102.8000), colour = "#FDB32FFF", arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  #geom_point(data = mean_sulph, aes(x = mean_SpCondShallow, y = mean_Sulphate), size = 5, colour = "black")+
  #geom_point(data = mean_sulph, aes(x = mean_SpCondShallow, y = mean_Sulphate), size = 3.5)+
  scale_colour_viridis_d(option = "plasma", direction = 1) + 
  labs(colour = "Year") + 
  labs(x ="Specific Conductivity (µS/cm)", y = "Sulfate (mg/L)") +
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        panel.background = element_blank())

#Figure 6
SpCond / UV254 / Sulph + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(face = "bold", size = 16))