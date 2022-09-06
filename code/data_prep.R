library(tidyverse)
library(lubridate)
library(here)
library(rLakeAnalyzer)

## Data Prep ####

dat_2015 <- read_csv(here("data/processed_data", "buoy_gam_2015_vars.csv"))

dat_2016 <- read_csv(here("data/processed_data", "buoy_gam_2016_vars.csv"))

dat_2017 <- read_csv(here("data/processed_data", "buoy_gam_2017_vars.csv"))

dat_2018 <- read_csv(here("data/processed_data", "buoy_gam_2018_vars.csv"))

dat_2019 <- read_csv(here("data/processed_data", "buoy_gam_2019_vars.csv"))

dat_2020 <- read_csv(here("data/processed_data", "buoy_gam_2020_vars.csv"))

dat_all <- rbind(dat_2015, dat_2016, dat_2017, dat_2018, dat_2019, dat_2020)

## Calculate Schmidt Stability

wtr <- dat_all%>% select(Date, wtr_0.45 = Temp00, 
                           wtr_0.77 = Temp01, wtr_1.23 = Temp02, 
                           wtr_2.18 = Temp03, wtr_3.18 = Temp04)

## load bathymetric data and format it for rLakeAnalyzer 

bathy <- read_csv(here("data/processed_data/", "BP_HypsometricData_Kehoe.csv")) 

bathy <- bathy %>% rename(depths = Depth, areas = Area)

## run time series schmidt stability

TSSchmidt <- ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

## add year, reorder cols and remove date and multiple temperatures

dat_all <- dat_all %>% mutate(Year = year(Date),
                                  Schmidt = TSSchmidt$schmidt.stability)

dat_all <- dat_all %>% select(Change, Year, Date, everything(), -Temp00, -Temp01, -Temp02, -Temp03, -Temp04)

write_csv(dat_all, here('data/processed_data', 'buoy_all_vars.csv'))

#### reload and append 2014 data

dat_all <- read_csv(here("data/processed_data", "buoy_all_vars.csv"))

dat_2014 <- read_csv(here("data/processed_data", "buoy_2014_vars.csv"))

dat_all <- rbind(dat_2014, dat_all)

#ratios
dat_all <- dat_all %>% mutate("ODO_ratio" = (ODOShallow/ODODeep),
                                "pH_ratio" = (pHShallow/pHDeep),
                                "SpCond_ratio" = SpCondShallow/SpCondDeep)

#update .csv file
write_csv(dat_all, here('data/processed_data', 'buoy_all_vars.csv'))

#boxplots
ggplot(dat_all, aes(x=Change, y = ODODeep, fill = factor(Year)))+
  geom_boxplot()

## + facet_wrap(~Year, scale = "free_y")

ggplot(dat_all, aes(x =Change, y = pHShallow, fill = factor(Year))) +
  geom_boxplot()### + facet_wrap(~Year, scale = "free_y")

ggplot(dat_all, aes(x = Change, y=Schmidt, fill = factor(Year)))+
  geom_boxplot() # facet_wrap(~Year, scale = "free_y")


## linear mod
library(lme4)
mod <- lmer(PhycoRFUShallow ~ SpCondShallow +(1|Year) + (1|Change), data = dat_all, REML = F)
summary(mod)

mod.null <- lmer(PhycoRFUShallow ~ 1 + (1|Year) + (1|Change), data = dat_all, REML = F)

anova(mod.null, mod)


#PCA (using Clause Wilke's blog post)
library(broom)

dat_all_1 <- na.omit(dat_all)

dat_all_1 <- dat_all_1 %>% select(DOY, WindSp, AirTemp, PARW1, PhycoRFUShallow, TurbShallow, TempShallow, pHShallow, SpCondShallow, ODOShallow,
                     Schmidt, pH_ratio, SpCond_ratio, ODO_ratio, Change, Year)
#for PCA wihtout Conductivity
dat_all_2 <- dat_all_1 %>% select(DOY, WindSp, AirTemp, PARW1, PhycoRFUShallow, TurbShallow, TempShallow, pHShallow, ODOShallow,
                                  Schmidt, pH_ratio, ODO_ratio, Change, Year)

pca_fit <- dat_all_1 %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  scale() %>% # scale data
  prcomp() # do PCA

pca_fit2 <- dat_all_2 %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  scale() %>% # scale data
  prcomp() # do PCA


pca_fit %>%
  augment(dat_all_1) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = Change)) + 
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(increase = "green", decrease = "red", none = "blue", plateau = "orange" )) 

pca_year <- pca_fit %>%
  augment(dat_all_1) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = factor(Year))) + 
  geom_point(size = 1.5) 

pca_fit2 %>%
  augment(dat_all_2) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = factor(Year))) + 
  geom_point(size = 1.5) 


pca_fit %>%
  tidy(matrix = "rotation")

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt"))

# plot rotation matrix
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed()  # fix aspect ratio to 1:1

#variance explained by each principle component
pca_fit %>%
  tidy(matrix = "eigenvalues")

pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01)))

#biplot
biplot(pca_fit)

library(ggrepel)

pca_vecs <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value")

pca_year + 
  geom_segment(data = pca_vecs, aes(x = 0, xend=PC1, y=0, yend=PC2), arrow = arrow(length = unit(0.25, "cm")), colour = "black", lwd=0.8) + 
  geom_text_repel(data = pca_vecs, aes(x=PC1, y=PC2, label = column), size = 3, bg.colour = "white", 
                  bg.r = 0.15, colour = "black", fontface = "bold", direction = "both", segment.size = NA, segment.angle = 180)+ 
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() 

##mrpp
library(vegan)

dat_num <- dat_all_1 %>% select(DOY, WindSp, AirTemp, PARW1, PhycoRFUShallow, TurbShallow, TempShallow, pHShallow, SpCondShallow, ODOShallow,
                                  Schmidt, pH_ratio, SpCond_ratio, ODO_ratio, Year)
dat_num2 <- dat_num %>% select(everything(), -Year)

mrpp(dat_num, factor(dat_all_1$Change), permutations = 999, distance = "euclidean",
     weight.type = 1, strata = NULL, parallel = getOption("mc.cores"))

adonis(dat_num2 ~ factor(dat_all_1$Year), distance = "bray")

#pairewise comparisons using permutation MANOVAs on a distance matrix (See which groups are different from each other)
library(RVAideMemoire)
library(ecodist)
bray=bcdist(dat_num)
bray2 =bcdist(dat_num2)
pairwise.perm.manova(bray, factor(dat_all_1$Change), p.method="holm", nperm=9999)