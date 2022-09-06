##Schmidt Stability Calculation using rLakeAnayzer##

library(rLakeAnalyzer)
library(tidyverse)
library(here)
library(lubridate)

## load and prep buoy data for rLakeAnalyzer wtr dataframe 

buoy2019 <- read_csv(here("data/clean_data/", "BP_Buoy_2019_KP.csv"), 
                     guess_max = 20000) 

wtr <- buoy2019 %>% select(DateTime, wtr_0.45 = Temp00, 
                    wtr_0.77 = Temp01, wtr_1.23 = Temp02, 
                    wtr_2.18 = Temp03, wtr_3.18 = Temp04)

## load bathymetric data and format it for rLakeAnalyzer bathy df

bathy <- read_csv(here("data/raw_data/", "BP_HypsometricData_Kehoe.csv")) 

bathy <- bathy %>% rename(depths = Depth, areas = Area)

##run time series schmidt stability

TSSchmidt_2019 <- ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

## calculate daily avg stability 

Schmidt_DOY_2019 <- TSSchmidt_2019 %>% mutate(DOY = yday(DateTime)) %>% 
  group_by(DOY) %>% summarise(avg_stability = mean(schmidt.stability, na.rm = TRUE))

ggplot(Schmidt_DOY_2019, aes(DOY, avg_stability)) + 
  geom_area(fill = "cornflowerblue", colour = "black")

## save file for plots 
write_csv(Schmidt_DOY_2019, here("data/processed_data", "BP_Daily_Schmidt_2019.csv"))


## 2018 ####
buoy2018 <- read_csv(here("data/clean_data/", "BP_Buoy_2018_KP.csv"), 
                     guess_max = 20000) 

wtr <- buoy2018 %>% select(DateTime, wtr_0.45 = Temp00, 
                           wtr_0.77 = Temp01, wtr_1.23 = Temp02, 
                           wtr_2.18 = Temp03, wtr_3.18 = Temp04)

bathy <- read_csv(here("data/raw_data/", "BP_HypsometricData_Kehoe.csv")) 

bathy <- bathy %>% rename(depths = Depth, areas = Area)


TSSchmidt_2018 <- ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

Schmidt_DOY_2018 <- TSSchmidt_2018 %>% mutate(DOY = yday(DateTime)) %>% 
  group_by(DOY) %>% summarise(avg_stability = mean(schmidt.stability, na.rm = TRUE))

ggplot(Schmidt_DOY_2018, aes(DOY, avg_stability)) + 
  geom_area(fill = "cornflowerblue", colour = "black")

write_csv(Schmidt_DOY_2018, here("data/processed_data", "BP_Daily_Schmidt_2018.csv"))
## 2017 #### 

buoy2017 <- read_csv(here("data/clean_data/", "BP_Buoy_2017_KP.csv"), 
                     guess_max = 20000) 

wtr <- buoy2017 %>% select(DateTime, wtr_0.45 = Temp00, 
                           wtr_0.77 = Temp01, wtr_1.23 = Temp02, 
                           wtr_2.18 = Temp03, wtr_3.18 = Temp04)

bathy <- read_csv(here("data/raw_data/", "BP_HypsometricData_Kehoe.csv")) 

bathy <- bathy %>% rename(depths = Depth, areas = Area)


TSSchmidt_2017 <- ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

Schmidt_DOY_2017 <- TSSchmidt_2017 %>% mutate(DOY = yday(DateTime)) %>% 
  group_by(DOY) %>% summarise(avg_stability = mean(schmidt.stability, na.rm = TRUE))

ggplot(Schmidt_DOY_2017, aes(DOY, avg_stability)) + 
  geom_area(fill = "cornflowerblue", colour = "black")

write_csv(Schmidt_DOY_2017, here("data/processed_data", "BP_Daily_Schmidt_2017.csv"))