library(tidyverse)
library(here)
library(colorspace)

all_dat <- read_csv(here('data/processed_data', 'BP_all_June72022.csv'))

#create bins
all_dat$Phyco_bin <-  cut(all_dat$PhycoRFUShallow, breaks=c(0,1,5,10,15,20, 30, max(all_dat$PhycoRFUShallow)),
                          labels=c("0-1","1-5","5-10","10-15","15-20","20-30", ">30"))
#Figure 3 - Heat Map
ggplot(all_dat, aes(x =DOY, y = factor(Year))) +
  geom_tile(aes(fill = Phyco_bin)) +
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275)) + 
  scale_y_discrete(limits = rev(levels(as.factor(all_dat$Year))), expand = c(0,0)) +
  labs(x = "Day of Year", y = NULL, fill = expression(bold("Phycocyanin (RFU)"))) +
  scale_fill_discrete_sequential(palette = "Inferno", rev = FALSE)+
  #theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 14, colour = "black"),
        axis.text.y = element_text(face = "bold", size = 14, colour = "black"),
        axis.title.x = element_text(face = "bold", size =14),
        axis.title.y = element_text(face = "bold", size = 14, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.text = element_text(face = "bold", size =14),
        legend.title = element_text(face = "bold", size =14))