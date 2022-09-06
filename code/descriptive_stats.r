library(summarytools)



year_descr <- stby(data    = all_dat,
                      INDICES = all_dat$Year, 
                      FUN     = descr,
                      stats   = "common")

year_descr_df <- year_descr %>% tb()

bloom_descr <- stby(data    = pca_dat_2ref,
                   INDICES = pca_dat_2ref$Bloom, 
                   FUN     = descr,
                   stats   = "common")

bloom_descr_df <- bloom_descr %>% tb()

ctable(x = all_dat$Year, 
       y = all_dat$Bloom, 
       prop = "r") 

ctable(x = all_dat$Year, 
       y = all_dat$Change, 
       prop = "r") 

#bins used for heatmap figure
all_dat$Phyco_bin <-  cut(all_dat$PhycoRFUShallow, breaks=c(0,1,5,10,15,20, 30, max(all_dat$PhycoRFUShallow)),
                          labels=c("0-1","1-5","5-10","10-15","15-20","20-30", ">30"))

ctable(x = all_dat$Year, 
       y = all_dat$Phyco_bin, 
       prop = "r")

#PCA variables + categorical only 

pca_varsd <- all_dat %>% select(Year, DOY, Bloom, Change, WindSp, TempShallow_max, Total_Precip_mm, Quap_Flow, PARW2,
                               SpCondShallow, TurbShallow, PhycoRFUShallow, ODOShallow_max, pHShallow_max, Schmidt)

plant_vars <- all_dat %>% select(Year, DOY, Bloom, Change, Phosphate_total, Phosphate_ortho, Ammonia, Nitrate, Organic_N, UV_254, SUVA, Sulphate)


pca_descr <- stby(data    = pca_varsd,
                   INDICES = pca_varsd$Year, 
                   FUN     = descr,
                   stats   = "common")
pca_vars_table <-  as.data.frame(pca_descr %>% tb())

write_csv(pca_vars_table, here('data/processed_data', 'summarystats_pca_R.csv'))

freq(pca_varsd$Year, plain.ascii = FALSE, style = "rmarkdown")

freq(pca_varsd$Total_Precip_mm)

plant_descr <- stby(data    = plant_vars,
                  INDICES = plant_vars$Year, 
                  FUN     = descr,
                  stats   = "common")
plant_vars_table <-  as.data.frame(plant_descr %>% tb())

ctable(x = pca_varsd$Year, 
       y = pca_varsd$Phyco_bin, 
       prop = "r") 

ctable(x = pca_varsd$Year, 
       y = pca_varsd$Bloom, 
       prop = "r") 


#Blooming days comparison - standardized
common_days <- pca_varsd %>% filter(DOY %in% c(163:247)) 
ctable(x = common_days$Year, 
       y = common_days$Bloom, 
       prop = "r") 


#RAIN
with(pca_varsd, 
     stby(data    = Total_Precip_mm, 
          INDICES = Year, 
          FUN     = descr))

pca_varsd$Raining <- if_else(pca_varsd$Total_Precip_mm > 0, "true", "false", "na") 

#frequency of precip thorughout the year
pca_varsd %>% filter(Year %in% c(2020)) %>% freq(Total_Precip_mm)

with(pca_varsd, ctable(Year, Raining, prop = "c", totals = FALSE))

rain_tb <- pca_varsd %>% group_by(Year) %>% summarize(rain_sum = sum(Total_Precip_mm, na.rm = TRUE)) 


stby(data    = all_dat$Sulphate,
     INDICES = all_dat$Year, 
     FUN     = descr,
     stats   = "common")

#Looking at differences between years in specific variables

m1<-kruskal.test(TurbShallow ~ Year, data=all_dat)
print(m1)

library(FSA)
posthocm1 <- dunnTest(TurbShallow ~ factor(Year), data = all_dat, method = "holm")
print(posthocm1)

#Correlation test
cor.test(all_dat$SpCondShallow, all_dat$Sulphate, method = "pearson")