library(tidyverse)
library(lubridate)
library(broom)
library(here)
library(RVAideMemoire)
library(vegan)
library(factoextra)

##Data ####

#buoy/WTP data file
all_dat <- read_csv(here('data/processed_data', 'BP_all_June72022.csv'))


#Qu'appelle at Elbow drop structure daily flow data
quap_dat <- read_csv(here('data/raw_data', 'stationO5JG006_parms.csv'))

quap_dat <- quap_dat %>% filter(PARAM == 1, YEAR > 2013)%>% 
  rename('DOY' = DD,
         "Quap_Flow" = Value) %>%
  select(YEAR, DOY, Quap_Flow)

#join flow and weather to larger data file
all_dat <- all_dat %>% left_join(quap_dat, by = c("Year" = "YEAR", "DOY" = "DOY"))


#Select daily variables excl. plant data  for PCA and remove NAs 

pca_dat <- all_dat %>% select(-c(63:99)) 
##pca_dat_1 <- na.omit(pca_dat) #2019 loses deep vars

pca_dat_2 <- pca_dat %>% select(WindSp, TempShallow_max, Total_Precip_mm, Quap_Flow, PARW2,
                     SpCondShallow, TurbShallow, PhycoRFUShallow, ODOShallow_max, pHShallow_max, Schmidt) %>% 
                             na.omit()

pca_dat_2 <- pca_dat_2 %>% rename("WindSpeed" = WindSp, "Max_Temp" = TempShallow_max, "Rain" = Total_Precip_mm, "QR_Flow" = Quap_Flow,
                      "PAR" = PARW2, "SpecCond" = SpCondShallow, "Turbidity" = TurbShallow, "Phycocyanin" = PhycoRFUShallow,
                         "Max_DissOxy" = ODOShallow_max, "Max_pH" = pHShallow_max)

pca_dat_2ref <- pca_dat %>% select(Year,DOY, Bloom, WindSp_min, TempShallow_max, Total_Precip_mm, Quap_Flow, PARW2,
                                SpCondShallow, TurbShallow, PhycoRFUShallow, ODOShallow_max, pHShallow_max, Schmidt) %>% 
  na.omit()

#arrow style for plots
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt"))


#pairewise comparisons using permutation MANOVAs on a distance matrix (See which groups are different from each other)

#create new transformed dataframe (function to log 10 + 1 transform)
fun_log <- function(x){
  return (log10(x+1))}

pca_2_dat_trans <- data.frame(lapply(pca_dat_2,fun_log)) ##THIS ONE KEEP

#PERMANOVA (differences between years)
euclid <-  vegdist(pca_2_dat_trans, method = "euclidean")
pairwise.perm.manova(euclid, factor(pca_dat_2ref$Year), p.method="holm", nperm=9999)

##PCA 

pca_2_tran <- pca_2_dat_trans %>% 
  scale() %>%
  prcomp()

eig.val <- get_eigenvalue(pca_2_tran)
eig.val

#Scree Plot (Fig S3)
fviz_eig(pca_2_tran, addlabels = TRUE, ylim = c(0, 40),
         labelsize = 5,
         title = "",
         ggtheme = theme(axis.text.x = element_text(face = "bold", size = 14),
                         axis.text.y = element_text(face = "bold", size = 14),
                         axis.title.x = element_text(face = "bold", size =14),
                         axis.title.y = element_text(face = "bold", size = 14),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(),
                         axis.ticks.y = element_blank(),
                         legend.title = element_text(face = "bold", size =14),
                         legend.text = element_text(face = "bold", size =14)))


# Results for Variables (using factoextra functions)
res.var <- get_pca_var(pca_2_tran)
corr_tab <- as.data.frame(res.var$cor)
write_csv(corr_tab, here('data/processed_data', "PCA_corr_table.csv"))
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
fviz_cos2(pca_2_tran, choice="var", axes = 1, top = 10 )
fviz_cos2(pca_2_tran, choice="var", axes = 2, top = 10 )


pca_fviz <-  fviz_pca_var(pca_2_tran, col.var = "cos2", 
             gradient.cols = c("#000004FF", "#89226AFF", "#F98C0AFF"), 
             labelsize = 5,
             repel = TRUE,
             title = "",
             xlab = "PC1 (33.1%)",
             ylab = "PC2 (20.2%)",
             ggtheme = theme(axis.text.x = element_text(face = "bold", size = 14),
                                     axis.text.y = element_text(face = "bold", size = 14),
                                     axis.title.x = element_text(face = "bold", size =14),
                                     axis.title.y = element_text(face = "bold", size = 14),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.background = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     axis.line = element_line(),
                                     legend.title = element_text(face = "bold", size =14),
                                     legend.text = element_text(face = "bold", size =14))) 

#Supplemental grouping variable coordinates (Blooming)
blooming <- pca_dat_2ref$Bloom
# 1. Individual coordinates
res.ind <- get_pca_ind(pca_2_tran)
# 2. Coordinate of groups
coord.groups <- res.ind$coord %>%
  as_data_frame() %>%
  select(Dim.1, Dim.2) %>%
  mutate(Blooming = blooming) %>%
  group_by(Blooming) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2)
  )
coord.groups

ellipse <- fviz_pca_ind(pca_2_tran, label="none", habillage=pca_dat_2ref$Bloom,
             addEllipses=TRUE, ellipse.level=0.95, mean.point.size = 6,
             palette = c("#56106EFF", "#F98C0AFF"),
                title = "",
                legend.title = "Blooming",
                xlab = "PC1 (33.1%)",
                ylab = "PC2 (20.2%)",
                ggtheme = theme(axis.text.x = element_text(face = "bold", size = 14),
                                axis.text.y = element_text(face = "bold", size = 14),
                                axis.title.x = element_text(face = "bold", size =14),
                                axis.title.y = element_text(face = "bold", size = 14),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.line = element_line(),
                                legend.title = element_text(face = "bold", size =14),
                                legend.text = element_text(face = "bold", size =14))) 



#variance explained be each PC tidy way

bloom_pca_stats <- pca_2_tran %>%
               tidy(matrix = "eigenvalues")

#Figure 4 - PCA plots

pca_fviz  + ellipse +  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(face = "bold", size = 16)) ##KEEP THIS ONE

