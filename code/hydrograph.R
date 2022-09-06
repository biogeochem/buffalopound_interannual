library(tidyverse)
library(here)

##Flow Data from ECCC Hydromet Station 05JG006 
quap_flow <- read_csv(here('data/raw_data', 'stationO5JG006_parms.csv'))

quap_flow <- quap_flow %>% filter(PARAM == 1, YEAR > 2013)%>% 
  rename('DOY' = DD) %>%
  select(ID, YEAR, DOY, Value)

#Hydrograph (Figure S1)

ggplot(quap_flow, aes(DOY, Value)) + geom_point() + geom_line()+
  scale_x_continuous(breaks = seq(0, 365, 50))+
  facet_wrap(~YEAR, scales = "free_x") +
  labs(x = "Day of Year" , y = expression(bold("Discharge (m"^3*"/s)"))) +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size =10),
        axis.title.y = element_text(face = "bold", size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold", size =10),
        strip.background = element_blank())