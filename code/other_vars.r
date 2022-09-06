library(tidyverse)
library(lubridate)
library(here)

##2014 ####
buoy_2014 <- read_csv(here("data/clean_data", "BP_Buoy_2014_KP.csv"))

buoy_2014 <- buoy_2014 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime)) %>%
                                  rename(TempShallow = TemperatureShallow)

#remove rows without phyco data 
buoy_2014 <- buoy_2014[complete.cases(buoy_2014$BGPCShallow2),]

#Daily mean, max, and min
phyc_2014_all <-  buoy_2014 %>% group_by(Date, DOY) %>%  summarize_if(is.numeric, list(~mean(., na.rm = TRUE), ~max(., na.rm =TRUE), ~min(., na.rm = TRUE)))

#Delta Temps 
DT_shallow_2014 <- phyc_2014_all$TempShallow_mean
DT_shallow_2014lag <- lag(DT_shallow_2014) 
Delta_T = DT_shallow_2014 - DT_shallow_2014lag

phyc_2014_all$Delta_TempShallow = Delta_T

DT_deep_2014 <- phyc_2014_all$TempDeep_mean
DT_deep_2014lag <- lag(DT_deep_2014) 
Delta_T_deep = DT_deep_2014 - DT_deep_2014lag

phyc_2014_all$Delta_TempDeep = Delta_T_deep

#make dataframe of vars of interestest to append to larger data set later
minmax_2014 <- phyc_2014_all %>% select(Date, DOY, TempShallow_max, TempDeep_max, TempShallow_min, TempDeep_min, pHShallow_max, pHShallow_min, 
                         pHDeep_max, pHDeep_min, SpCondShallow_max, SpCondShallow_min, SpCondDeep_max, SpCondDeep_min, BGPCShallow2_max, BGPCShallow2_min,
                         ODOShallow_max, ODOShallow_min, ODODeep_max, ODODeep_min, WindSp_max, WindSp_min, RelativeHum_max, RelativeHum_min, 
                         Delta_TempShallow, Delta_TempDeep)

minmax_2014 <- minmax_2014 %>% rename(PhycoRFUShallow_max = BGPCShallow2_max,
                       PhycoRFYShallow_min = BGPCShallow2_min)
##2015 ####
buoy_2015 <- read_csv(here("data/clean_data", "BP_Buoy_2015_KP.csv"))

buoy_2015 <- buoy_2015 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime)) 

#remove rows without phyco data 
buoy_2015 <- buoy_2015[complete.cases(buoy_2015$BGA_PCShallow2),]

#Daily mean, max, and min
phyc_2015_all <-  buoy_2015 %>% group_by(Date, DOY) %>%  summarize_if(is.numeric, list(~mean(., na.rm = TRUE), ~max(., na.rm =TRUE), ~min(., na.rm = TRUE)))

#Delta Temps 
DT_shallow_2015 <- phyc_2015_all$TempShallow_mean
DT_shallow_2015lag <- lag(DT_shallow_2015) 
Delta_T = DT_shallow_2015 - DT_shallow_2015lag

phyc_2015_all$Delta_TempShallow = Delta_T

DT_deep_2015 <- phyc_2015_all$TempDeep_mean
DT_deep_2015lag <- lag(DT_deep_2015) 
Delta_T_deep = DT_deep_2015 - DT_deep_2015lag

phyc_2015_all$Delta_TempDeep = Delta_T_deep

#make dataframe of vars of interestest to append to larger data set later
minmax_2015 <- phyc_2015_all %>% select(Date, DOY, TempShallow_max, TempDeep_max, TempShallow_min, TempDeep_min, pHShallow_max, pHShallow_min, 
                                        pHDeep_max, pHDeep_min, SpCondShallow_max, SpCondShallow_min, SpCondDeep_max, SpCondDeep_min, BGA_PCShallow2_max, BGA_PCShallow2_min,
                                        ODOShallow_max, ODOShallow_min, ODODeep_max, ODODeep_min, WindSp_max, WindSp_min, RelativeHum_max, RelativeHum_min, 
                                        Delta_TempShallow, Delta_TempDeep)

minmax_2015 <- minmax_2015 %>% rename(PhycoRFUShallow_max = BGA_PCShallow2_max,
                                      PhycoRFYShallow_min = BGA_PCShallow2_min)

##2016 ####
buoy_2016 <- read_csv(here("data/clean_data", "BP_Buoy_2016_KP.csv"))

buoy_2016 <- buoy_2016 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime)) 

#remove rows without phyco data 
buoy_2016 <- buoy_2016[complete.cases(buoy_2016$BGA_PCShallow2),]

#Daily mean, max, and min
phyc_2016_all <-  buoy_2016 %>% group_by(Date, DOY) %>%  summarize_if(is.numeric, list(~mean(., na.rm = TRUE), ~max(., na.rm =TRUE), ~min(., na.rm = TRUE)))

#Delta Temps 
DT_shallow_2016 <- phyc_2016_all$TempShallow_mean
DT_shallow_2016lag <- lag(DT_shallow_2016) 
Delta_T = DT_shallow_2016 - DT_shallow_2016lag

phyc_2016_all$Delta_TempShallow = Delta_T

DT_deep_2016 <- phyc_2016_all$TempDeep_mean
DT_deep_2016lag <- lag(DT_deep_2016) 
Delta_T_deep = DT_deep_2016 - DT_deep_2016lag

phyc_2016_all$Delta_TempDeep = Delta_T_deep

#make dataframe of vars of interestest to append to larger data set later
minmax_2016 <- phyc_2016_all %>% select(Date, DOY, TempShallow_max, TempDeep_max, TempShallow_min, TempDeep_min, pHShallow_max, pHShallow_min, 
                                        pHDeep_max, pHDeep_min, SpCondShallow_max, SpCondShallow_min, SpCondDeep_max, SpCondDeep_min, BGA_PCShallow2_max, BGA_PCShallow2_min,
                                        ODOShallow_max, ODOShallow_min, ODODeep_max, ODODeep_min, WindSp_max, WindSp_min, RelativeHum_max, RelativeHum_min, 
                                        Delta_TempShallow, Delta_TempDeep)

minmax_2016 <- minmax_2016 %>% rename(PhycoRFUShallow_max = BGA_PCShallow2_max,
                                      PhycoRFYShallow_min = BGA_PCShallow2_min)


##2017 ####
buoy_2017 <- read_csv(here("data/clean_data", "BP_Buoy_2017_KP.csv"))

buoy_2017 <- buoy_2017 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime)) 

#remove rows without phyco data 
buoy_2017 <- buoy_2017[complete.cases(buoy_2017$BGA_PCShallow2),]

#Daily mean, max, and min
phyc_2017_all <-  buoy_2017 %>% group_by(Date, DOY) %>%  summarize_if(is.numeric, list(~mean(., na.rm = TRUE), ~max(., na.rm =TRUE), ~min(., na.rm = TRUE)))

#Delta Temps 
DT_shallow_2017 <- phyc_2017_all$TempShallow_mean
DT_shallow_2017lag <- lag(DT_shallow_2017) 
Delta_T = DT_shallow_2017 - DT_shallow_2017lag

phyc_2017_all$Delta_TempShallow = Delta_T

DT_deep_2017 <- phyc_2017_all$TempDeep_mean
DT_deep_2017lag <- lag(DT_deep_2017) 
Delta_T_deep = DT_deep_2017 - DT_deep_2017lag

phyc_2017_all$Delta_TempDeep = Delta_T_deep

#make dataframe of vars of interestest to append to larger data set later
minmax_2017 <- phyc_2017_all %>% select(Date, DOY, TempShallow_max, TempDeep_max, TempShallow_min, TempDeep_min, pHShallow_max, pHShallow_min, 
                                        pHDeep_max, pHDeep_min, SpCondShallow_max, SpCondShallow_min, SpCondDeep_max, SpCondDeep_min, BGA_PCShallow2_max, BGA_PCShallow2_min,
                                        ODOShallow_max, ODOShallow_min, ODODeep_max, ODODeep_min, WindSp_max, WindSp_min, RelativeHum_max, RelativeHum_min, 
                                        Delta_TempShallow, Delta_TempDeep)

minmax_2017 <- minmax_2017 %>% rename(PhycoRFUShallow_max = BGA_PCShallow2_max,
                                      PhycoRFYShallow_min = BGA_PCShallow2_min)


##2018 ####
buoy_2018 <- read_csv(here("data/clean_data", "BP_Buoy_2018_KP.csv"))

buoy_2018 <- buoy_2018 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime)) 

#remove rows without phyco data 
buoy_2018 <- buoy_2018[complete.cases(buoy_2018$BGA_PCShallow2),]

#Daily mean, max, and min
phyc_2018_all <-  buoy_2018 %>% group_by(Date, DOY) %>%  summarize_if(is.numeric, list(~mean(., na.rm = TRUE), ~max(., na.rm =TRUE), ~min(., na.rm = TRUE)))

#Delta Temps 
DT_shallow_2018 <- phyc_2018_all$TempShallow_mean
DT_shallow_2018lag <- lag(DT_shallow_2018) 
Delta_T = DT_shallow_2018 - DT_shallow_2018lag

phyc_2018_all$Delta_TempShallow = Delta_T

DT_deep_2018 <- phyc_2018_all$TempDeep_mean
DT_deep_2018lag <- lag(DT_deep_2018) 
Delta_T_deep = DT_deep_2018 - DT_deep_2018lag

phyc_2018_all$Delta_TempDeep = Delta_T_deep

#make dataframe of vars of interestest to append to larger data set later
minmax_2018 <- phyc_2018_all %>% select(Date, DOY, TempShallow_max, TempDeep_max, TempShallow_min, TempDeep_min, pHShallow_max, pHShallow_min, 
                                        pHDeep_max, pHDeep_min, SpCondShallow_max, SpCondShallow_min, SpCondDeep_max, SpCondDeep_min, BGA_PCShallow2_max, BGA_PCShallow2_min,
                                        ODOShallow_max, ODOShallow_min, ODODeep_max, ODODeep_min, WindSp_max, WindSp_min, RelativeHum_max, RelativeHum_min, 
                                        Delta_TempShallow, Delta_TempDeep)

minmax_2018 <- minmax_2018 %>% rename(PhycoRFUShallow_max = BGA_PCShallow2_max,
                                      PhycoRFYShallow_min = BGA_PCShallow2_min)
##2019 ####
buoy_2019 <- read_csv(here("data/clean_data", "BP_Buoy_2019_KP.csv"))

buoy_2019 <- buoy_2019 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime)) 

#remove rows without phyco data 
buoy_2019 <- buoy_2019[complete.cases(buoy_2019$BGA_PCShallow2),]

#Daily mean, max, and min
phyc_2019_all <-  buoy_2019 %>% group_by(Date, DOY) %>%  summarize_if(is.numeric, list(~mean(., na.rm = TRUE), ~max(., na.rm =TRUE), ~min(., na.rm = TRUE)))

#Delta Temps 
DT_shallow_2019 <- phyc_2019_all$TempShallow_mean
DT_shallow_2019lag <- lag(DT_shallow_2019) 
Delta_T = DT_shallow_2019 - DT_shallow_2019lag

phyc_2019_all$Delta_TempShallow = Delta_T

DT_deep_2019 <- phyc_2019_all$TempDeep_mean
DT_deep_2019lag <- lag(DT_deep_2019) 
Delta_T_deep = DT_deep_2019 - DT_deep_2019lag

phyc_2019_all$Delta_TempDeep = Delta_T_deep

#make dataframe of vars of interestest to append to larger data set later
minmax_2019 <- phyc_2019_all %>% select(Date, DOY, TempShallow_max, TempDeep_max, TempShallow_min, TempDeep_min, pHShallow_max, pHShallow_min, 
                                        pHDeep_max, pHDeep_min, SpCondShallow_max, SpCondShallow_min, SpCondDeep_max, SpCondDeep_min, BGA_PCShallow2_max, BGA_PCShallow2_min,
                                        ODOShallow_max, ODOShallow_min, ODODeep_max, ODODeep_min, WindSp_max, WindSp_min, RelativeHum_max, RelativeHum_min, 
                                        Delta_TempShallow, Delta_TempDeep)

minmax_2019 <- minmax_2019 %>% rename(PhycoRFUShallow_max = BGA_PCShallow2_max,
                                      PhycoRFYShallow_min = BGA_PCShallow2_min)
##2020 ####
buoy_2020 <- read_csv(here("data/clean_data", "BP_Buoy_2020_KP.csv"))

buoy_2020 <- buoy_2020 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime)) 

#remove rows without phyco data 
buoy_2020 <- buoy_2020[complete.cases(buoy_2020$BGA_PCRFUShallow),]

#Daily mean, max, and min
phyc_2020_all <-  buoy_2020 %>% group_by(Date, DOY) %>%  summarize_if(is.numeric, list(~mean(., na.rm = TRUE), ~max(., na.rm =TRUE), ~min(., na.rm = TRUE)))

#Delta Temps 
DT_shallow_2020 <- phyc_2020_all$TempShallow_mean
DT_shallow_2020lag <- lag(DT_shallow_2020) 
Delta_T = DT_shallow_2020 - DT_shallow_2020lag

phyc_2020_all$Delta_TempShallow = Delta_T

DT_deep_2020 <- phyc_2020_all$TempDeep_mean
DT_deep_2020lag <- lag(DT_deep_2020) 
Delta_T_deep = DT_deep_2020 - DT_deep_2020lag

phyc_2020_all$Delta_TempDeep = Delta_T_deep

#make dataframe of vars of interestest to append to larger data set later
minmax_2020 <- phyc_2020_all %>% select(Date, DOY, TempShallow_max, TempDeep_max, TempShallow_min, TempDeep_min, pHShallow_max, pHShallow_min, 
                                        pHDeep_max, pHDeep_min, SpCondShallow_max, SpCondShallow_min, SpCondDeep_max, SpCondDeep_min, BGA_PCRFUShallow_max, BGA_PCRFUShallow_min,
                                        ODOShallow_max, ODOShallow_min, ODODeep_max, ODODeep_min, WindSp_max, WindSp_min, RelativeHum_max, RelativeHum_min, 
                                        Delta_TempShallow, Delta_TempDeep)

minmax_2020 <- minmax_2020 %>% rename(PhycoRFUShallow_max = BGA_PCRFUShallow_max,
                                      PhycoRFYShallow_min = BGA_PCRFUShallow_min)


##combine and save file ####
minmax_all <- rbind(minmax_2014, minmax_2015, minmax_2016, minmax_2017, minmax_2018, minmax_2019, minmax_2020)

write_csv(minmax_all, here('data/processed_data', 'buoy_minmax_vars.csv'))

## append to larger data file and save

all_dat <- read_csv(here('data/processed_data', 'BP_all_May132022.csv'))

new_dat <- all_dat %>% left_join(minmax_all, by = ("Date" = "Date"))

new_dat <- new_dat %>% select(everything(), -DOY.y) %>% rename ("DOY"= DOY.x)

write_csv(new_dat, here('data/processed_data', 'BP_all_May262022.csv'))

## edited coding of change categories for 2020 data in the .csv file

#read back in and remove "Inf" from max/min calcs caused by missing values
new_dat <- read_csv(here("data/processed_data", "BP_all_May262022.csv"),
                    col_types = cols(
                      TempDeep_max = col_double(),
                      pHDeep_max = col_double(),
                      SpCondDeep_max = col_double(),
                      ODOShallow_max = col_double(),
                      ODODeep_max = col_double(),
                      WindSp_max = col_double(),
                      RelativeHum_max = col_double()))

new_dat[sapply(new_dat, is.infinite)] <- NA

#add in some plant data
plant_dat <- read_csv(here('data/processed_data', "plantdat_2014_to_2020.csv"), col_types =
                        cols(SUVA = col_double(),
                             Crustaceans = col_double()))
#fill in missing DOYs for 2020
plant_dat <- plant_dat %>% mutate(DOY = yday(Date))

#join to buoy data
new_dat <- new_dat %>% left_join(plant_dat, by = c("Date" = "Date", "DOY" = "DOY"))

write_csv(new_dat, here('data/processed_data', 'BP_all_June72022.csv'))

new_dat <- read_csv(here('data/processed_data', 'BP_all_June72022.csv'))

#some thresholds
new_dat$thresh10 <- if_else(new_dat$PhycoRFUShallow > 10, "true", "false", "na") 

new_dat$thresh25 <- if_else(new_dat$PhycoRFUShallow > 25, "true", "false", "na") 



threshRFU <- new_dat %>% 
  group_by(Year, Change) %>% 
  summarise(n_over20 = sum(PhycoRFUShallow >20), 
            n_over10 = sum(PhycoRFUShallow > 10),
            n_over5 = sum(PhycoRFUShallow > 5),
            n_total = sum(PhycoRFUShallow > 0))

threshRFU_2 <- new_dat %>% 
  group_by(Year) %>% 
  summarise(n_over20 = sum(PhycoRFUShallow >20), 
            n_over10 = sum(PhycoRFUShallow > 10),
            n_over5 = sum(PhycoRFUShallow > 5),
            n_less5 = sum(PhycoRFUShallow < 5))

threshRFU_2 <- threshRFU_2 %>% pivot_longer(cols = c(2:5), names_to = "n_over", values_to = "count")

ggplot(threshRFU_2, aes(Year, count, colour= n_over)) +
  geom_line() 

thresh10 <- new_dat %>% select(Year, DOY, PhycoRFUShallow) %>%
  group_by(Year) %>% 
  summarise(
    n10 = sum(PhycoRFUShallow >10),
    n = n())

ggplot(thresh10, aes(x = n, y = n10, fill= Year)) + 
  geom_point(alpha = 1/10)

new_dat$thresh10 <- if_else(new_dat$PhycoRFUShallow > 10, "true", "false", "na") 
new_dat <-  new_dat %>% mutate(Month = month(Date))

ggplot(new_dat, aes(x = DOY, y = PhycoRFUShallow, shape = thresh10)) +
  geom_point(aes(colour= factor(Year))) +
  stat_density2d() +
  geom_hline(yintercept = 10)

ggplot(new_dat, aes(x = DOY, y = thresh10)) +
  geom_violin(scale ="count") +
  geom_point(data = new_dat, aes(x = DOY, y = thresh10, colour = factor(Year)), position = position_jitter(width = 0.05))  

ggplot(new_dat, aes(x = PhycoRFUShallow, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2, binwidth = 5) +
  geom_density()
 
library(ggbeeswarm)

ggplot(new_dat, aes(x = DOY, y = thresh10)) +
  geom_violin() +
  geom_quasirandom(data = new_dat, aes(x = DOY, y = thresh10, colour = factor(Month)), groupOnX = FALSE)  


ggplot((new_dat %>% filter(thresh10 == "true")), aes(x = Change, y = PhycoRFUShallow)) +
  geom_violin() +
  geom_quasirandom(data = (new_dat %>% filter(thresh10 == "true")), aes(x = Change, y = PhycoRFUShallow, colour = factor(Year))) 


##Flow Data (see other script for full file read-in and wrangling)

quap_flow <- read_csv(here('data/raw_data', 'stationO5JG006_parms.csv'))

quap_flow <- quap_flow %>% filter(PARAM == 1, YEAR > 2013)%>% 
  rename('DOY' = DD) %>%
  select(ID, YEAR, DOY, Value)

#Hydrograph

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

new_dat <- new_dat %>% left_join(quap_flow, by = c("Year" = "YEAR", "DOY" = "DOY"))

up_flow <- up_flow %>% select (ID, YEAR, DOY, Value, Residence_T)
new_dat <- new_dat %>% left_join(up_flow, by = c("Year" = "YEAR", "DOY" = "DOY"))


ggplot((new_dat %>% filter(Residence_T < 5)), aes(x=Change, y = Residence_T, fill = factor(Year)))+
  geom_boxplot()


ggplot(new_dat, aes(x=Change, y = Value.y, fill = factor(Year)))+
  geom_boxplot() +
  ylab("Upstream Inflow")

ggplot(new_dat, aes(x=Change, y = PhycoRFUShallow_max, fill = factor(Year)))+
  geom_boxplot() 

ggplot(new_dat, aes(Residence_T, PhycoRFUShallow_max, colour= factor(Year)))+
  geom_point() + coord_cartesian(xlim = c(0, 3)) 

ggplot(new_dat, aes(Value.y, PhycoRFUShallow_max, colour= factor(Year)))+
  geom_point() + coord_cartesian(xlim = c(0, 10)) 



ggplot(all_dat, aes(x=Year, y = Phosphate_total, fill = factor(Year)))+
  geom_boxplot() 

ggplot(new_dat, aes(x=Year, y = PhycoRFUShallow, fill = factor(Year)))+
  geom_boxplot() 

ggplot(new_dat, aes(DOY, SpCondShallow, colour = factor(Year))) +
  geom_point() +  stat_ellipse() + scale_colour_viridis_d(option = "plasma", direction = 1) + 
  labs(colour = "Year") + theme_dark()

mean_UV254 <- new_dat %>% group_by(Year) %>% summarise("mean_UV254" = mean(UV_254, na.rm = TRUE),
                                                       "mean_DOY" = mean(DOY, na.rm = TRUE)) 
#UV 254 plot by DOY 
UV254 <- ggplot(all_dat, aes(DOY, UV_254, colour = factor(Year))) +
  geom_point() + stat_ellipse() + 
  #geom_point(data = mean_UV254, aes(x = mean_DOY, y = mean_UV254), size = 5)+
  scale_colour_viridis_d(option = "plasma", direction = 1) + 
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275)) + 
  labs(colour = "Year") + 
  labs(x = "Day of Year", y = expression(bold(UV[254]~(Abs/10~cm))))+ 
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        legend.position = "none",
        panel.background = element_rect(fill = "gray"))

  

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
        legend.position = "none",
        panel.background = element_rect(fill = "gray"))


#Sulfate vs Conductivity plot 
mean_sulph <- all_dat %>% group_by(Year) %>% summarise("mean_Sulphate" = mean(Sulphate, na.rm = TRUE),
                                                       "mean_SpCondShallow" = mean(SpCondShallow, na.rm = TRUE)) 

Sulph <- ggplot(all_dat, aes(SpCondShallow, Sulphate, colour = factor(Year))) +
  geom_point(size = 2.5) + #stat_ellipse() + 
  geom_curve(aes(x = 607.4092, y = 158.0400, xend = 912.7121, yend = 266.8500), colour = "#0D0887FF", curvature = .8,  size = 1, arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  geom_curve(aes(x = 912.7121, y = 266.8500, xend = 796.1487, yend = 222.4800), colour = "#5D01A6FF", curvature = 0.75, size = 1, arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  geom_curve(aes(x = 796.1487, y = 222.4800, xend = 873.6846, yend = 252.8800), colour = "#9C179EFF", curvature = 0.75, size = 1, arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  geom_curve(aes(x = 873.6846, y = 252.8800, xend = 614.8507, yend = 150.8125), colour = "#CC4678FF", curvature = 1.25, size = 1,  arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  geom_curve(aes(x = 614.8507, y = 150.8125, xend = 497.7493, yend = 105.0000), colour = "#ED7953FF", curvature = -0.75, size =1, arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  #geom_curve(aes(x = 497.7493, y = 105.0000, xend = 502.8972, yend = 102.8000), colour = "#FDB32FFF", arrow = arrow(angle = 30, type = "closed", length = unit(0.20, "inches"))) +
  geom_point(data = mean_sulph, aes(x = mean_SpCondShallow, y = mean_Sulphate), size = 5, colour = "black")+
  geom_point(data = mean_sulph, aes(x = mean_SpCondShallow, y = mean_Sulphate), size = 3.5)+
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
        legend.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size =12),
        panel.background = element_rect(fill = "gray"))

ggplot(data = mean_sulph, aes(mean_SpCondShallow, mean_Sulphate)) +
  geom_point()+ geom_line()

library(scales)
show_col(viridis_pal(option = "B")(9))

library(patchwork)

SpCond + UV254 + Sulph + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(face = "bold", size = 16))

##Heat Map 
all_dat$Phyco_bin <-  cut(all_dat$PhycoRFUShallow, breaks=c(0,1,5,10,15,20, 30, max(all_dat$PhycoRFUShallow)),
                                           labels=c("0-1","1-5","5-10","10-15","15-20","20-30", ">30"))

library(colorspace)

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

ggplot(new_dat, aes(x =DOY, y = factor(Year))) +
  geom_tile(aes(fill = Change)) +
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275)) + 
  scale_y_discrete(limits = rev(levels(as.factor(new_dat$Year))), expand = c(0,0)) +
  labs(x = "Day of Year", y = NULL, fill = ("Change Period")) +
  scale_fill_discrete_sequential(palette = "Inferno", limits = c("none", "increase", "plateau", "decrease"), rev = FALSE) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, face = "bold", size = 10),
        axis.text.y = element_text(face = "bold.italic", size = 10),
        axis.title.x = element_text(face = "bold", size =10),
        axis.title.y = element_text(face = "bold", size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank())


#dot plot
ggplot(new_dat, aes(x = DOY, y = factor(Year))) +
  geom_point(size = 2, aes(colour = new_dat$Change)) +
  scale_y_discrete(limits = rev(levels(as.factor(new_dat$Year)))) +
  scale_x_continuous(breaks = c(150, 175, 200, 225, 250, 275)) + 
  labs(x = "Day of Year", y = "Year")+
  scale_colour_viridis_d(option = "inferno", limits = c("none", "increase", "plateau", "decrease")) +
  labs(colour = "Change") 