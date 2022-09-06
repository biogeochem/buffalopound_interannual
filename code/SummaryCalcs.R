#Calculations
library(tidyverse)
library(lubridate)
library(here)
library(rLakeAnalyzer)
library(summarytools)

## Calculation used for Schmidt Stability
#Note that 2014 was calculated separately due to less temperature sensors on the buoy that year
## (see data/processed_data for individual year files which have been appended together)

wtr <- dataframe %>% select(Date, wtr_0.45 = Temp00, 
                         wtr_0.77 = Temp01, wtr_1.23 = Temp02, 
                         wtr_2.18 = Temp03, wtr_3.18 = Temp04)

## load bathymetric data and format it for rLakeAnalyzer 

bathy <- read_csv(here("data/processed_data/", "BP_HypsometricData_Kehoe.csv")) 

bathy <- bathy %>% rename(depths = Depth, areas = Area)

## run time series schmidt stability

TSSchmidt <- ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

##Summary statistics

all_dat <- read_csv(here('data/processed_data', 'BP_all_June72022.csv'))

#Bloom days (proportions by year)
ctable(x = all_dat$Year, 
       y = all_dat$Bloom, 
       prop = "r") 

#Blooming days comparison - standardized
common_days <- pca_varsd %>% filter(DOY %in% c(163:247)) 
ctable(x = common_days$Year, 
       y = common_days$Bloom, 
       prop = "r") 

#PCA-only variables - summary stats by year

pca_varsd <- all_dat %>% select(Year, DOY, Bloom, Change, WindSp, TempShallow_max, Total_Precip_mm, Quap_Flow, PARW2,
                                SpCondShallow, TurbShallow, PhycoRFUShallow, ODOShallow_max, pHShallow_max, Schmidt)

pca_descr <- stby(data    = pca_varsd,
                  INDICES = pca_varsd$Year, 
                  FUN     = descr,
                  stats   = "common")
pca_vars_table <-  as.data.frame(pca_descr %>% tb())

write_csv(pca_vars_table, here('data/processed_data', 'summarystats_pca_R.csv'))


#Rain summary
with(pca_varsd, 
     stby(data    = Total_Precip_mm, 
          INDICES = Year, 
          FUN     = descr))
#total rain by year
pca_varsd %>% group_by(Year) %>% summarize(rain_sum = sum(Total_Precip_mm, na.rm = TRUE)) 

#Sulfate summary 
stby(data    = all_dat$Sulphate,
     INDICES = all_dat$Year, 
     FUN     = descr,
     stats   = "common")

#Sulfate vs. Sp. Cond. correaltion test

cor.test(all_dat$SpCondShallow, all_dat$Sulphate, method = "pearson")