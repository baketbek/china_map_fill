#---------------------------------------------------#
######## Project: china_map_fill.Rproj ########
# !/usr/bin/env Rscript
# -*- coding: utf-8 -*-
#
# File:        province_fill_map
# Author:      Baha
# Createdate:  Tue Dec 01 13:57:33 2020
# Software:    R version 4.0.2 (2020-06-22)
# Filepath:    E:/Projects/graduate_projects/bw_tmperature_figs/china_map_fill
# Desc: 
#---------------------------------------------------#

# load_packages -------------------------------------------------------------------------------
pacman::p_load(tidyverse,sf,fs,RColorBrewer,hrbrthemes,ggspatial)

# load_data -----------------------------------------------------------------------------------
data_prov <- read_csv("dataset/data_prov.csv")
cn <- st_read("dataset/china_full_map.json") %>%
      mutate(prov_id = str_sub(NAME, 1, 2))
cn %>%
  left_join(data_prov,
            by = c("prov_id" = "prov")) %>%
  mutate(casenum = as.double(casenum)) %>%
  mutate(casenum = case_when(
        is.na(casenum) ~ 0,
        T ~ casenum)) %>%
  mutate(classify = cut(casenum,
         breaks = c(0.0, 70.99, 139.99, 307.99, 499.99, 999.9, 100000),
         labels = c("None", "71 - 139", "140 - 307", "308 - 599",
               "600 - 999","> 1000"),include.lowest = TRUE)) -> map_data_prov



# color_options -------------------------------------------------------------------------------
col_standard <- c("#542275",
                  "#933581",
                  "#B2407E",
                  "#E2697A",
                  "#F3846F",
                  "#FDB783")
col_light <- c(
  #"#fee5d9",
  "white"  ,
  #"#fcbba1",
  "#fc9272",
  "#fb6a4a",
  "#ef3b2c",
  "#cb181d",
  "#99000d")

col_white <- c("#542275",
               "#933581",
               "#B2407E",
               "#E2697A",
               "#F3846F",
               "#F5F5F5")
col_rmy <- c(
             "#933581",
             "#B2407E",
             "#E2697A",
             "#F3846F",
             "#FDB783",
             "#F5F5F5")

# Fonts_options -------------------------------------------------------------------------------
windowsFonts(enfont=windowsFont("Times New Roman"))

# visualize_province --------------------------------------------------------------------------

ggplot(map_data_prov) + 
  geom_sf(data=subset(map_data_prov,
                      NAME!="国界线"),
          aes(fill=classify), 
          color = "gray50", size = 0.2) + 
  geom_sf(data=subset(map_data_prov,
                      NAME=="国界线"), 
          color = "black",
          size = 0.05)+
  geom_sf(data=subset(map_data_prov,
                      NAME=="小地图框格"), 
          color = "black", size = 0.5)+
  theme_ipsum(base_family = "enfont", grid = "") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(legend.position = c(0.2, 0.1),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle =element_text(hjust = 0.5) )+
  guides(fill = guide_legend(title = "Population",
                             title.position = "top",
                             title.hjust = 0.3,
                             nrow = 3)) + 
  labs(title = "Province Distribution") + 
  #scale_fill_gradient(low = "white",high = "#99000d")+
  scale_fill_manual(values = rev(col_standard))+
  annotation_scale(location = "tl", width_hint = 0.1,
                   pad_x = unit(0.1, "cm"),
                   pad_y = unit(1.65, "cm"),
                   text_family = "enfont") +
  annotation_north_arrow(location = "tl", which_north = "False",
                         height = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.65, "cm"),
                         style = north_arrow_fancy_orienteering(text_family = "enfont"))  -> prov_p
prov_p

ggsave(filename = "output/province_fig_standard_color.svg",plot =prov_p,width = 12,height = 9,dpi = 800)


