library(tidyverse)
library(here)
library(patchwork)
library(ggridges)

all_dat <- read_csv(here('data/processed_data', 'BP_all_June72022.csv'))

#Ridge Density Plots

phyco <- ggplot(all_dat, aes(x = PhycoRFUShallow, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour= "grey") +
  scale_x_continuous(breaks = seq(0,55, 10)) + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "RFU") +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Phycocyanin", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))


turb <- ggplot(all_dat, aes(x = TurbShallow, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour= "grey") +
  scale_x_continuous() + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "NTU") +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Turbidity", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))

cond <- ggplot(all_dat, aes(x = SpCondShallow, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour= "grey") +
  scale_x_continuous() + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "µS/cm") +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Specific Conductivity", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))


PAR <- ggplot(all_dat, aes(x = PARW2, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour = "grey") +
  scale_x_continuous() + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = expression(bold("µmol/s/m" ^2))) +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "PAR", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))



wind <- ggplot(all_dat, aes(x = WindSp, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour = "grey") +
  scale_x_continuous(breaks = seq(0,30, 5)) + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "m/s") +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Wind Speed", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))


rain <- ggplot(all_dat, aes(x = Total_Precip_mm, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour = "grey") +
  scale_x_continuous(breaks = seq(0,70, 10)) + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "mm") +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Rain", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))


flow <- ggplot(all_dat, aes(x = Quap_Flow, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour = "grey") +
  scale_x_continuous(breaks = seq(0,20, 5)) + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = expression(bold(m^3~"/"~s))) +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Qu'Appelle R. Discharge", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))



schmidt <- ggplot(all_dat, aes(x = Schmidt, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour = "grey") +
  scale_x_continuous() + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = expression(bold(J~"/"~m^2))) +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Schmidt Stability", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))


pH_max <- ggplot(all_dat, aes(x = pHShallow_max, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour= "grey") +
  scale_x_continuous() + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "pH") +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Max pH", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))

  
temp_max <- ggplot(all_dat, aes(x = TempShallow_max, y = factor(Year), fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour= "grey") +
    scale_x_continuous(breaks = seq(0,35,5)) + 
    scale_fill_viridis_c(option = "plasma", direction = 1, name = "deg. C") +
  scale_y_discrete(expand = c(0,0))+
    labs(x = "Max Temperature", y = NULL) +
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))

  
ODO_max <- ggplot(all_dat, aes(x = ODOShallow_max, y = factor(Year), fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour= "grey") +
    scale_x_continuous() + 
    scale_fill_viridis_c(option = "plasma", direction = 1, name = "mg/L") +
  scale_y_discrete(expand = c(0,0))+
    labs(x = "Max Dissolved Oxygen", y = NULL) +
    theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))


##Figure 5 - multipanel ridge density plot

#Patchwork plot
layout <- "
AABBCC#
DDEEFF#
GGHHII#
JJKK###
"

phyco + cond + turb + schmidt + pH_max + ODO_max + temp_max + PAR + wind + rain + flow + plot_layout(design = layout)


#BPWTP Phosphorus (Figure 7)
Phos_tot <- ggplot(all_dat, aes(x = Phosphate_total, y = factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, colour= "grey", jittered_points = TRUE, point_colour = "black") +
  scale_x_continuous(limits = c(0, 250)) + 
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "µg P/L") +
  scale_y_discrete(expand = c(0,0))+
  labs(x = "Total Phosphorus", y = NULL) +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12))

  
  
  
  
  
 


