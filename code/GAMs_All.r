library(tidyverse)
library(lubridate)
library(here)
library(mgcv)
library(gratia)
library(patchwork)

##common theme

theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
                  axis.text.y = element_text(face = "bold", size = 12),
                  axis.title.x = element_text(face = "bold", size =12),
                  axis.title.y = element_text(face = "bold", size = 12),
                  plot.title = element_text(vjust = -6, hjust = 0.05, face = "bold"),
                  axis.line = element_line(),
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

#2014 ####
buoy_2014 <- read_csv(here("data/clean_data", "BP_Buoy_2014_KP.csv"))

buoy_2014 <- buoy_2014 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime))

#Daily mean values 
phyc_2014 <-  buoy_2014 %>% group_by(Date, DOY) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)

phyc_2014 <- phyc_2014 %>% mutate(Week = week(Date),
                                  Month = month(Date))
#remove rows without phyco data 
phyc_2014 <- phyc_2014[complete.cases(phyc_2014$BGPCShallow2),]

#Gam
m_14 <- gam(BGPCShallow2 ~ s(DOY, k = 50), data = phyc_2014, method = "REML")

summary(m_14)
gam.check(m_14)
gam_14 <- draw(m_14, residuals = TRUE)

layout(matrix(1:2, ncol = 2))
acf(resid(m_14))
pacf(resid(m_14))
layout(1)

#Gamm with correlation process (no difference in residual autocorr.)
m_14a <-  gamm(BGPCShallow2 ~ s(DOY, k = 40, bs = "cr"), data = phyc_2014,
               correlation = corCAR1(form = ~ 1|DOY), method = "REML")

summary(m_14a$gam)
gam.check(m_14a$gam)
draw(m_14a$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m_15a$lme, type = "normalized"))
pacf(resid(m_15a$lme, type = "normalized"))
layout(1)

#Derivatives 
d_14<- derivatives(m_14, type = "central")
draw(d_14)

#does it change?
d_14<- d_14 %>% mutate(Null_test = 0 >= lower & 0 <= upper)

#new data to be used with predictors (length = output of derivatives f(x))
pdat14 <- with(phyc_2014,
             data.frame(DOY = seq(min(DOY), max(DOY),
                                  length = 200)))
#extract predictors and standard errors
p_14 <- predict(m_14, newdata = pdat14, se.fit = TRUE)

#calculate confidence intervals for predictors
p_14 <- as.data.frame(p_14)
p_14 <- p_14 %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                        "upper.p" = (fit+(se.fit*1.96))) 

#bind to derivatives data frame
d_14 <- cbind(d_14, p_14, pdat14)

#create continuous variable for Null_test (for figure)
d_14$sig = if_else(d_14$Null_test == "FALSE", 1, 2) 

#figure
p2014 <- ggplot(d_14, aes(DOY, fit)) +
  scale_x_continuous(limits = c(129, 285), breaks = seq(135, 275, 15)) +
  scale_y_continuous(limits = c(-2.5, 52), breaks = seq(0,45, 15)) +
  xlab("DOY") + ylab("") +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "slategray1") +
  geom_point(data = phyc_2014, aes(DOY, BGPCShallow2), alpha = 0.6, size = 0.85) +
  geom_line(data = d_14, aes(x = DOY, y = fit, colour= sig), size = 1.2, alpha = 0.8) +
  scale_colour_gradient(low = "violetred", high = "slategray", guide = NULL) +
  geom_line(data = d_14, aes(x = DOY, y = lower.p), colour = "slategray") +
  geom_line(data = d_14, aes(x = DOY, y = upper.p), colour = "slategray") +
  ggtitle("2014")+ coord_cartesian(xlim = c(145, 275), ylim = c(-1.5, 51)) +
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size =12),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.title = element_text(vjust = -6, hjust = 0.05, face = "bold"),
        axis.line = element_line(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#make a new variable for change periods and append to buoy data
new_14 <- d_14 %>% select(DOY, sig) 

#round to nearest DOY
new_14 <- new_14 %>% mutate(DOY = floor(new_14$DOY)) 

#drop repeat values
new_14 <- new_14 %>% distinct(DOY, sig)

#join to buoy data
new_14 <- left_join(phyc_2014, new_14, by = c("DOY" = "DOY")) 

#print to csv for further processing 
write_csv(new_14, here("data/processed_data", "buoy_gam_2014.csv")) 
#2015 ####
buoy_2015 <- read_csv(here("data/clean_data", "BP_Buoy_2015_KP.csv"))

buoy_2015 <- buoy_2015 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime))

#Daily mean values 
phyc_2015 <-  buoy_2015 %>% group_by(Date, DOY) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)

phyc_2015 <- phyc_2015 %>% mutate(Week = week(Date),
                                  Month = month(Date))
#remove rows without phyco data 
phyc_2015 <- phyc_2015[complete.cases(phyc_2015$BGA_PCShallow2),]

#Gam (need to maximize k due to complexity of this one)
m_15 <- gam(BGA_PCShallow2 ~ s(DOY, k = 80), data = phyc_2015, method = "REML")

summary(m_15)
gam.check(m_15)
gam_15 <- draw(m_15, residuals = TRUE)

layout(matrix(1:2, ncol = 2))
acf(resid(m_15))
pacf(resid(m_15))
layout(1)

#Gamm with correlation process (no difference in residual autocorr.)
m_15a <-  gamm(BGA_PCShallow2 ~ s(DOY, k = 50, bs = "cr"), data = phyc_2015,
                correlation = corCAR1(form = ~ 1|DOY), method = "REML")

summary(m_15a$gam)
gam.check(m_15a$gam)
draw(m_15a$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m_15a$lme, type = "normalized"))
pacf(resid(m_15a$lme, type = "normalized"))
layout(1)

#Derivatives 
d_15<- derivatives(m_15, type = "central")
draw(d_15)

#does it change?
d_15<- d_15 %>% mutate(Null_test = 0 >= lower & 0 <= upper)

#new data to be used with predictors (length = output of derivatives f(x))
pdat <- with(phyc_2015,
                   data.frame(DOY = seq(min(DOY), max(DOY),
                                        length = 200)))
#extract predictors and standard errors
p_15 <- predict(m_15, newdata = pdat, se.fit = TRUE)

#calculate confidence intervals for predictors
p_15 <- as.data.frame(p_15)
p_15 <- p_15 %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                        "upper.p" = (fit+(se.fit*1.96))) 

#bind to derivatives data frame
d_15 <- cbind(d_15, p_15, pdat)

#create continuous variable for Null_test (for figure)
d_15$sig = if_else(d_15$Null_test == "FALSE", 1, 2) 

#figure
p2015 <- ggplot(d_15, aes(DOY, fit)) +
  scale_x_continuous(limits = c(129, 285), breaks = seq(135, 275, 15)) +
  scale_y_continuous(limits = c(-2.5, 52), breaks = seq(0,45, 15)) +
  xlab("DOY") + ylab("") +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "slategray1") +
  geom_point(data = phyc_2015, aes(DOY, BGA_PCShallow2), alpha = 0.6, size = 0.85) +
  geom_line(data = d_15, aes(x = DOY, y = fit, colour= sig), size = 1.2, alpha = 0.8) +
  scale_colour_gradient(low = "violetred", high = "slategray", guide = NULL) +
  geom_line(data = d_15, aes(x = DOY, y = lower.p), colour = "slategray") +
  geom_line(data = d_15, aes(x = DOY, y = upper.p), colour = "slategray") +
  ggtitle("2015")+ coord_cartesian(xlim = c(145, 275), ylim = c(-1.5, 51)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 12))+
  theme(axis.title.x = element_text(face = "bold", size =12))+
  theme(axis.title.y = element_text(face = "bold", size = 12)) + 
  theme(plot.title = element_text(vjust = -6, hjust = 0.05))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#make a new variable for change periods and append to buoy data
new_15 <- d_15 %>% select(DOY, sig) 

#round to nearest DOY
new_15 <- new_15 %>% mutate(DOY = floor(new_15$DOY)) 

#drop repeat values
new_15 <- new_15 %>% distinct(DOY, sig)

#join to buoy data
new_15 <- left_join(phyc_2015, new_15, by = c("DOY" = "DOY")) 

#print to csv for further processing 
write_csv(new_15, here("data/processed_data", "buoy_gam_2015.csv")) 

#2016 ####
buoy_2016 <- read_csv(here("data/clean_data", "BP_Buoy_2016_KP.csv"))

buoy_2016 <- buoy_2016 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime))

#Daily mean values 
phyc_2016 <-  buoy_2016 %>% group_by(Date, DOY) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)

phyc_2016 <- phyc_2016 %>% mutate(Week = week(Date),
                                  Month = month(Date))

#remove rows without phyco data 
phyc_2016 <- phyc_2016[complete.cases(phyc_2016$BGA_PCShallow2),]

#Gam
m_16 <- gam(BGA_PCShallow2 ~ s(DOY, k = 80), data = phyc_2016, method = "REML")

summary(m_16)
gam.check(m_16)
gam_16 <- draw(m_16, residuals = TRUE)

layout(matrix(1:2, ncol = 2))
acf(resid(m_16))
pacf(resid(m_16))
layout(1)

#Gamm with correlation process (does not improve at k = 40)
m_16a <-  gamm(BGA_PCShallow2 ~ s(DOY, k = 70), data = phyc_2016,
               correlation = corCAR1(form = ~ 1|DOY), method = "REML")

summary(m_16a$gam)
gam.check(m_16a$gam)
draw(m_16a$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m_16a$lme, type = "normalized"))
pacf(resid(m_16a$lme, type = "normalized"))
layout(1)

#Derivatives 
d_16<- derivatives(m_16, type = "central")
draw(d_16)

d_16<- d_16 %>% mutate(Null_test = 0 >= lower & 0 <= upper)

pdat16 <- with(phyc_2016,
             data.frame(DOY = seq(min(DOY), max(DOY),
                                  length = 200)))

#extract predictors and standard errors
p_16 <- predict(m_16, newdata = pdat16, se.fit = TRUE)

#calculate confidence intervals for predictors
p_16 <- as.data.frame(p_16)
p_16 <- p_16 %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                        "upper.p" = (fit+(se.fit*1.96))) 

#bind to derivatives data frame
d_16 <- cbind(d_16, p_16, pdat16)

#create continuous variable for Null_test (for figure)
d_16$sig = if_else(d_16$Null_test == "FALSE", 1, 2) 

#figure
p2016 <- ggplot(d_16, aes(DOY, fit)) +
  scale_x_continuous(limits = c(129, 285), breaks = seq(135, 275, 15)) +
  scale_y_continuous(limits = c(-2.8, 52), breaks = seq(0,45, 15)) +
  xlab("DOY") + ylab("") +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "slategray1") +
  geom_point(data = phyc_2016, aes(DOY, BGA_PCShallow2), alpha = 0.6, size = 0.85) +
  geom_line(data = d_16, aes(x = DOY, y = fit, colour= sig), size = 1.2, alpha = 0.8) +
  scale_colour_gradient(low = "violetred", high = "slategray", guide = NULL) +
  geom_line(data = d_16, aes(x = DOY, y = lower.p), colour = "slategray") +
  geom_line(data = d_16, aes(x = DOY, y = upper.p), colour = "slategray") +
  ggtitle("2016")+ coord_cartesian(xlim = c(145, 275), ylim = c(-1.5, 51)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 12))+
  theme(axis.title.x = element_text(face = "bold", size =12))+
  theme(axis.title.y = element_text(face = "bold", size = 12)) + 
  theme(plot.title = element_text(vjust = -6, hjust = 0.05))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#make a new variable for change periods and append to buoy data
new_16 <- d_16 %>% select(DOY, sig) 

#round to nearest DOY
new_16 <- new_16 %>% mutate(DOY = floor(new_16$DOY)) 

#drop repeat values
new_16 <- new_16 %>% distinct(DOY, sig)

#join to buoy data
new_16 <- left_join(phyc_2016, new_16, by = c("DOY" = "DOY")) 

#print to csv for further processing 
write_csv(new_16, here("data/processed_data", "buoy_gam_2016.csv")) 


#2017 ####
buoy_2017 <- read_csv(here("data/clean_data", "BP_Buoy_2017_KP.csv"))

buoy_2017 <- buoy_2017 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime))

#Daily mean values 
phyc_2017 <-  buoy_2017 %>% group_by(Date, DOY) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)

phyc_2017 <- phyc_2017 %>% mutate(Week = week(Date),
                                  Month = month(Date))
#remove rows without phyco data 
phyc_2017 <- phyc_2017[complete.cases(phyc_2017$BGA_PCShallow2),]

#GAM (with k = 50 is better than GAMM for resid. autocorr.)
m_17 <- gam(BGA_PCShallow2 ~ s(DOY, k = 70), data = phyc_2017, method = "REML")

summary(m_17)
gam.check(m_17)
gam_17 <- draw(m_17, residuals = TRUE)

layout(matrix(1:2, ncol = 2))
acf(resid(m_17))
pacf(resid(m_17))
layout(1)

#Gamm with correlation process 
m_17a <-  gamm(BGA_PCShallow2 ~ s(DOY, k = 50, bs = "cr"), data = phyc_2017,
               correlation = corCAR1(form = ~ 1|DOY), method = "REML")

summary(m_17a$gam)
gam.check(m_17a$gam)
draw(m_17a$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m_16a$lme, type = "normalized"))
pacf(resid(m_16a$lme, type = "normalized"))
layout(1)

#Derivatives 
d_17<- derivatives(m_17, type = "central")
draw(d_17)

d_17<- d_17 %>% mutate(Null_test = 0 >= lower & 0 <= upper)

pdat17 <- with(phyc_2017,
               data.frame(DOY = seq(min(DOY), max(DOY),
                                    length = 200)))

#extract predictors and standard errors
p_17 <- predict(m_17, newdata = pdat17, se.fit = TRUE)

#calculate confidence intervals for predictors
p_17 <- as.data.frame(p_17)
p_17 <- p_17 %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                        "upper.p" = (fit+(se.fit*1.96))) 

#bind to derivatives data frame
d_17 <- cbind(d_17, p_17, pdat17)

#create continuous variable for Null_test (for figure)
d_17$sig = if_else(d_17$Null_test == "FALSE", 1, 2) 

#figure
p2017 <- ggplot(d_17, aes(DOY, fit)) +
  scale_x_continuous(limits = c(129, 285), breaks = seq(135, 275, 15)) +
  scale_y_continuous(limits = c(-2.5, 52), breaks = seq(0,45, 15)) +
  xlab("DOY") + ylab("Phycocyanin (RFU)") +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "slategray1") +
  geom_point(data = phyc_2017, aes(DOY, BGA_PCShallow2), alpha = 0.6, size = 0.85) +
  geom_line(data = d_17, aes(x = DOY, y = fit, colour= sig), size = 1.2, alpha = 0.8) +
  scale_colour_gradient(low = "violetred", high = "slategray", guide = NULL) +
  geom_line(data = d_17, aes(x = DOY, y = lower.p), colour = "slategray") +
  geom_line(data = d_17, aes(x = DOY, y = upper.p), colour = "slategray") +
  ggtitle("2017")+ coord_cartesian(xlim = c(145, 275), ylim = c(-1.5, 51)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 12))+
  theme(axis.title.x = element_text(face = "bold", size =12))+
  theme(axis.title.y = element_text(face = "bold", size = 12)) + 
  theme(plot.title = element_text(vjust = -6, hjust = 0.05))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#make a new variable for change periods and append to buoy data
new_17 <- d_17 %>% select(DOY, sig) 

#round to nearest DOY
new_17 <- new_17 %>% mutate(DOY = floor(new_17$DOY)) 

#drop repeat values
new_17 <- new_17 %>% distinct(DOY, sig)

#join to buoy data
new_17 <- left_join(phyc_2017, new_17, by = c("DOY" = "DOY")) 

#print to csv for further processing 
write_csv(new_17, here("data/processed_data", "buoy_gam_2017.csv")) 



#2018 ####
buoy_2018 <- read_csv(here("data/clean_data", "BP_Buoy_2018_KP.csv"))

buoy_2018 <- buoy_2018 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime))

#Daily mean values 
phyc_2018 <-  buoy_2018 %>% group_by(Date, DOY) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)

phyc_2018 <- phyc_2018 %>% mutate(Week = week(Date),
                                  Month = month(Date))
#remove rows without phyco data
phyc_2018 <- phyc_2018[complete.cases(phyc_2018$BGA_PCShallow2),]

#Gam (even with high k, edf is only ~ 20 and fails k-check)
m_18 <- gam(BGA_PCShallow2 ~ s(DOY, k = 80), data = phyc_2018, method = "REML")

summary(m_18)
gam.check(m_18)
gam_18 <- draw(m_18, residuals = TRUE)

layout(matrix(1:2, ncol = 2))
acf(resid(m_18))
pacf(resid(m_18))
layout(1)

#Gamm with correlation process - no better
m_18a <-  gamm(BGA_PCShallow2 ~ s(DOY, k = 65), data = phyc_2018,
               correlation = corCAR1(form = ~ 1|DOY), method = "REML")

summary(m_18a$gam)
gam.check(m_18a$gam)
draw(m_18a$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m_18a$lme, type = "normalized"))
pacf(resid(m_18a$lme, type = "normalized"))
layout(1)

#Derivatives - use GAM because GAMM does not make a difference for autocorrelation
d_18<- derivatives(m_18, type = "central")
draw(d_18)

d_18<- d_18 %>% mutate(Null_test = 0 >= lower & 0 <= upper)

pdat18 <- with(phyc_2018,
               data.frame(DOY = seq(min(DOY), max(DOY),
                                    length = 200)))

#extract predictors and standard errors
p_18 <- predict(m_18, newdata = pdat18, se.fit = TRUE)

#calculate confidence intervals for predictors
p_18 <- as.data.frame(p_18)
p_18 <- p_18 %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                        "upper.p" = (fit+(se.fit*1.96))) 

#bind to derivatives data frame
d_18 <- cbind(d_18, p_18, pdat18)

#create continuous variable for Null_test (for figure)
d_18$sig = if_else(d_18$Null_test == "FALSE", 1, 2) 

#figure
p2018 <- ggplot(d_18, aes(DOY, fit)) +
  scale_x_continuous(limits = c(129, 285), breaks = seq(135, 275, 15)) +
  scale_y_continuous(limits = c(-2.5, 52), breaks = seq(0,45, 15)) +
  xlab("DOY") + ylab("") +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "slategray1") +
  geom_point(data = phyc_2018, aes(DOY, BGA_PCShallow2), alpha = 0.6, size = 0.85) +
  geom_line(data = d_18, aes(x = DOY, y = fit, colour= sig), size = 1.2, alpha = 0.8) +
  scale_colour_gradient(low = "violetred", high = "slategray", guide = NULL) +
  geom_line(data = d_18, aes(x = DOY, y = lower.p), colour = "slategray") +
  geom_line(data = d_18, aes(x = DOY, y = upper.p), colour = "slategray") +
  ggtitle("2018")+ coord_cartesian(xlim = c(145, 275), ylim = c(-1.5, 51)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 12))+
  theme(axis.title.x = element_text(face = "bold", size =12))+
  theme(axis.title.y = element_text(face = "bold", size = 12)) + 
  theme(plot.title = element_text(vjust = -6, hjust = 0.05))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#make a new variable for change periods and append to buoy data
new_18 <- d_18 %>% select(DOY, sig) 

#round to nearest DOY
new_18 <- new_18 %>% mutate(DOY = floor(new_18$DOY)) 

#drop repeat values
new_18 <- new_18 %>% distinct(DOY, sig)

#join to buoy data
new_18 <- left_join(phyc_2018, new_18, by = c("DOY" = "DOY")) 

#print to csv for further processing 
write_csv(new_18, here("data/processed_data", "buoy_gam_2018.csv")) 

#2019 ####
buoy_2019 <- read_csv(here("data/clean_data", "BP_Buoy_2019_KP.csv"))

buoy_2019 <- buoy_2019 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime))

#Daily mean values 
phyc_2019 <-  buoy_2019 %>% group_by(Date, DOY) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)

phyc_2019 <- phyc_2019 %>% mutate(Week = week(Date),
                                  Month = month(Date))

phyc_2019 <- phyc_2019[complete.cases(phyc_2019$BGA_PCShallow2),]

#Gam (fails k.check even at high k, edf stays the same)
m_19 <- gam(BGA_PCShallow2 ~ s(DOY, k = 40), data = phyc_2019, method = "REML")

summary(m_19)
gam.check(m_19)
gam_19 <- draw(m_19, residuals = TRUE)

layout(matrix(1:2, ncol = 2))
acf(resid(m_19))
pacf(resid(m_19))
layout(1)

#Gamm with correlation process (no better)
m_19a <-  gamm(BGA_PCShallow2 ~ s(DOY, k = 35, bs = "cr"), data = phyc_2019,
               correlation = corCAR1(form = ~ 1|DOY), method = "REML")

summary(m_19a$gam)
gam.check(m_19a$gam)
draw(m_19a$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m_19a$lme, type = "normalized"))
pacf(resid(m_19a$lme, type = "normalized"))
layout(1)

#Derivatives 
d_19<- derivatives(m_19, type = "central")
draw(d_19)

d_19<- d_19 %>% mutate(Null_test = 0 >= lower & 0 <= upper)

pdat19 <- with(phyc_2019,
               data.frame(DOY = seq(min(DOY), max(DOY),
                                    length = 200)))

#extract predictors and standard errors
p_19 <- predict(m_19, newdata = pdat19, se.fit = TRUE)

#calculate confidence intervals for predictors
p_19 <- as.data.frame(p_19)
p_19 <- p_19 %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                        "upper.p" = (fit+(se.fit*1.96))) 

#bind to derivatives data frame
d_19 <- cbind(d_19, p_19, pdat19)

#create continuous variable for Null_test (for figure)
d_19$sig = if_else(d_19$Null_test == "FALSE", 1, 2) 

#figure
p2019 <- ggplot(d_19, aes(DOY, fit)) +
  scale_x_continuous(limits = c(129, 285), breaks = seq(135, 275, 15)) +
  scale_y_continuous(limits = c(-2.5, 52), breaks = seq(0,45, 15)) +
  xlab("DOY") + ylab("") +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "slategray1") +
  geom_point(data = phyc_2019, aes(DOY, BGA_PCShallow2), alpha = 0.6, size = 0.85) +
  geom_line(data = d_19, aes(x = DOY, y = fit, colour= sig), size = 1.2, alpha = 0.8) +
  scale_colour_gradient(low = "violetred", high = "slategray", guide = NULL) +
  geom_line(data = d_19, aes(x = DOY, y = lower.p), colour = "slategray") +
  geom_line(data = d_19, aes(x = DOY, y = upper.p), colour = "slategray") +
  ggtitle("2019")+ coord_cartesian(xlim = c(145, 275), ylim = c(-1.5, 51)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 12))+
  theme(axis.title.x = element_text(face = "bold", size =12))+
  theme(axis.title.y = element_text(face = "bold", size = 12)) + 
  theme(plot.title = element_text(vjust = -6, hjust = 0.05))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#make a new variable for change periods and append to buoy data
new_19 <- d_19 %>% select(DOY, sig) 

#round to nearest DOY
new_19 <- new_19 %>% mutate(DOY = floor(new_19$DOY)) 

#drop repeat values
new_19 <- new_19 %>% distinct(DOY, sig)

#join to buoy data
new_19 <- left_join(phyc_2019, new_19, by = c("DOY" = "DOY")) 

#print to csv for further processing 
write_csv(new_19, here("data/processed_data", "buoy_gam_2019.csv")) 

#2020 ####
buoy_2020 <- read_csv(here("data/clean_data", "BP_Buoy_2020_KP.csv"))

buoy_2020 <- buoy_2020 %>% mutate(Date = as_date(DateTime),
                                  Hour = hour(DateTime),
                                  Time = as.numeric(DateTime))

#Daily mean values 
phyc_2020 <-  buoy_2020 %>% group_by(Date, DOY) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)

phyc_2020 <- phyc_2020 %>% mutate(Week = week(Date),
                                  Month = month(Date))

phyc_2020 <- phyc_2020[complete.cases(phyc_2020$BGA_PCRFUShallow),]

#GAM
m_20 <- gam(BGA_PCRFUShallow ~ s(DOY, k = 45), data = phyc_2020, method = "REML")

summary(m_20)
gam.check(m_20)
gam_20 <- draw(m_20, residuals = TRUE)

layout(matrix(1:2, ncol = 2))
acf(resid(m_20))
pacf(resid(m_20))
layout(1)

#Gamm with correlation process - does not improve residual autocorrelation
m_20a <-  gamm(BGA_PCRFUShallow ~ s(DOY, k = 60), data = phyc_2020,
               correlation = corCAR1(form = ~ 1|DOY), method = "REML")

summary(m_20a$gam)
gam.check(m_20a$gam)
draw(m_20a$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(m_20a$lme, type = "normalized"))
pacf(resid(m_20a$lme, type = "normalized"))
layout(1)

#Derivatives (uses regular GAM)
d_20<- derivatives(m_20, type = "central")
draw(d_20)

d_20<- d_20 %>% mutate(Null_test = 0 >= lower & 0 <= upper)

pdat20 <- with(phyc_2020,
               data.frame(DOY = seq(min(DOY), max(DOY),
                                    length = 200)))

p_20 <- predict(m_20, newdata = pdat20, se.fit = TRUE)

#calculate confidence intervals for predictors
p_20 <- as.data.frame(p_20)
p_20 <- p_20 %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                        "upper.p" = (fit+(se.fit*1.96))) 

#bind to derivatives dataframe
d_20 <- cbind(d_20, p_20, pdat20)

#create continuous variable for Null_test
d_20$sig = if_else(d_20$Null_test == "FALSE", 1, 2) 

#figure
p2020 <- ggplot(d_20, aes(DOY, fit)) +
  scale_x_continuous(limits = c(129, 285), breaks = seq(135, 275, 15)) +
  scale_y_continuous(limits = c(-2.5, 52), breaks = seq(0,45, 15)) +
  xlab("DOY") + ylab("") +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "slategray1") +
  geom_point(data = phyc_2020, aes(DOY, BGA_PCRFUShallow), alpha = 0.6, size = 0.85) +
  geom_line(data = d_20, aes(x = DOY, y = fit, colour= sig), size = 1.2, alpha = 0.8) +
  scale_colour_gradient(low = "violetred", high = "slategray", guide = NULL) +
  geom_line(data = d_20, aes(x = DOY, y = lower.p), colour = "slategray") +
  geom_line(data = d_20, aes(x = DOY, y = upper.p), colour = "slategray") +
  ggtitle("2020")+ coord_cartesian(xlim = c(145, 275), ylim = c(-1.5, 51)) + 
  theme_classic()+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1, face = "bold", size = 12)) +
  theme(axis.text.y = element_text(face = "bold", size = 12))+
  theme(axis.title.x = element_text(face = "bold", size =12))+
  theme(axis.title.y = element_text(face = "bold", size = 12)) + 
  theme(plot.title = element_text(vjust = -6, hjust = 0.05))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#make a new variable for change periods and append to buoy data
new_20 <- d_20 %>% select(DOY, sig) 

#round to nearest DOY
new_20 <- new_20 %>% mutate(DOY = floor(new_20$DOY)) 

#drop repeat values
new_20 <- new_20 %>% distinct(DOY, sig)

#join to buoy data
new_20 <- left_join(phyc_2020, new_20, by = c("DOY" = "DOY")) 

#print to csv for further processing 
write_csv(new_20, here("data/processed_data", "buoy_gam_2020.csv")) 



##Figure 2 - GAM multipanel ####

(p2014 + p2015 + p2016)/(p2017 + p2018 + p2019)/(p2020 + plot_spacer() + plot_spacer())

layout_2 <- "
 AABBCCDD
 EEFFGG##
"

#Fig S2 - plot of partial effects
gam_14 + gam_15 + gam_16 + gam_17+ gam_18 + gam_19 + gam_20 + plot_layout(design = layout_2) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold", size = 16))