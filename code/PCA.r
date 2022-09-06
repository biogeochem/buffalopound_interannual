library(tidyverse)
library(lubridate)
library(broom)
library(here)
library(factoextra)

##Data ####

#big buoy and plant data file
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

pca_dat <- all_dat %>% select(-c(63:107)) 
pca_dat_1 <- na.omit(pca_dat) #2019 loses deep vars

pca_dat_2 <- pca_dat %>% select(WindSp, TempShallow_max, Total_Precip_mm, Quap_Flow, PARW2,
                     SpCondShallow, TurbShallow, PhycoRFUShallow, ODOShallow_max, pHShallow_max, Schmidt) %>% 
                             na.omit()
pca_dat_2 <- pca_dat_2 %>% rename("WindSpeed" = WindSp, "Max_Temp" = TempShallow_max, "Rain" = Total_Precip_mm, "QR_Flow" = Quap_Flow,
                      "PAR" = PARW2, "SpecCond" = SpCondShallow, "Turbidity" = TurbShallow, "Phycocyanin" = PhycoRFUShallow,
                         "Max_DissOxy" = ODOShallow_max, "Max_pH" = pHShallow_max)

pca_dat_2ref <- pca_dat %>% select(Year,DOY, Bloom, WindSp_min, TempShallow_max, Total_Precip_mm, Quap_Flow, PARW2,
                                SpCondShallow, TurbShallow, PhycoRFUShallow, ODOShallow_max, pHShallow_max, Schmidt) %>% 
  na.omit()

#arrow for plots
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt"))


#pairewise comparisons using permutation MANOVAs on a distance matrix (See which groups are different from each other)
library(RVAideMemoire)
library(vegan)

#create new transformed dataframe schindler 
fun_log <- function(x){
  return (log10(x+1))}

# applying the custom function to every value and converting 
# it to dataframe, as lapply returns result in list 
# we have to convert it to data frame

pca_2_dat_trans <- data.frame(lapply(pca_dat_2,fun_log)) ##THIS ONE KEEP

euclid <-  vegdist(pca_2_dat_trans, method = "euclidean")
pairwise.perm.manova(euclid, factor(pca_dat_2ref$Year), p.method="holm", nperm=9999)

##PCA with data transformed before it is scaled 


pca_2_tran <- pca_2_dat_trans %>% ##KEEP 
  scale() %>%
  prcomp()

eig.val <- get_eigenvalue(pca_2_tran)
eig.val
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


# Results for Variables (using factorextra functions)
res.var <- get_pca_var(pca_2_tran)
corr_tab <- as.data.frame(res.var$cor)
write_csv(corr_tab, here('data/processed_data', "PCA_corr_table.csv"))
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
fviz_cos2(pca_2_tran, choice="var", axes = 1, top = 10 )
fviz_cos2(pca_2_tran, choice="var", axes = 2, top = 10 )
fviz_cos2(pca_1_tran, choice="var", axes = 1:2, top = 10 )

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

#tidy PCA (Clause Wilke tutorial)
t_bloom <- pca_2_tran %>%
    augment(pca_dat_2ref) %>% # add original dataset back in
    ggplot(aes(.fittedPC1, .fittedPC2, color = factor(Bloom))) + 
    geom_point(size = 1.5) + 
    scale_color_manual(values = c("#000004FF", "#F98C0AFF"))+
    geom_point(aes(x=0.656, y=0.157), size = 8, colour = "#000004FF" )+
    geom_point(aes(x=-1.59, y = -0.379), size = 8, colour = "#F98C0AFF" ) + 
    labs(x = "PC1", y = "PC2", color = "Blooming")+
    theme(axis.text.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size =14),
          axis.title.y = element_text(face = "bold", size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),
          legend.text = element_text(face = "bold", size =14),
          legend.title = element_text(face = "bold", size =14),
          legend.position = "right")

t_year<- pca_2_tran %>%
  augment(pca_dat_2ref) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = factor(Year))) + 
  geom_point(size = 1.5) + 
  scale_color_viridis_d(option = "plasma", direction = 1)+
  labs(x = "PC1", y = "PC2", color = "Year")+
  theme_dark()+
  theme(axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size =14),
        axis.title.y = element_text(face = "bold", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(face = "bold", size =14),
        legend.title = element_text(face = "bold", size =14),
        legend.position = "bottom")


t_rotation <- pca_2_tran %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "gray40",
    fontface = "bold") +
  labs(x = "PC1 (32.72 %)", y= "PC2 (17.26 %)")+
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_light()+
  theme(axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size =14),
        axis.title.y = element_text(face = "bold", size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(face = "bold", size =14))

#variance explained be each PC
pca_1_tran %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01)))

bloom_pca_stats <- pca_1_tran %>%
               tidy(matrix = "eigenvalues")

#multiplot
t_bloom + t_rotation + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold", size = 16))


t_bloom + pca_fviz  + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold", size = 16)) 

pca_fviz  + ellipse +  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(face = "bold", size = 16)) ##KEEP THIS ONE

