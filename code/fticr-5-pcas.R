# March-12-2021
# FTICR
# Water Analysis
# PCAs

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

fticr_data_water = read.csv("fticr_data_water.csv") 

fticr_meta_water = read.csv("fticr_meta_water.csv") 
meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass) 
### ^^ the files above have aliph as well as aromatic for the same sample, which can be confusing/misleading
### create an index combining them

fticr_water = 
  fticr_data_water %>% 
  select(ID, formula, slopepos, cover_type, plot) %>% 
  mutate(slopenum = recode(slopepos, "backslope" = "1-backslope",
                           "low backslope" = "2-low backslope",
                           "footslope" = "3-footslope")) %>% 
  na.omit()

fticr_data_water_summarized = 
  fticr_water %>% 
  distinct(ID, slopepos, slopenum, cover_type, plot, formula) %>% mutate(presence = 1) 


fticr_water_relabund = 
  fticr_data_water_summarized %>% 
  left_join(select(fticr_meta_water, formula, Class), by = "formula") %>% 
  ## create a column for group counts
  group_by(slopepos, slopenum, cover_type, plot, Class) %>% 
  dplyr::summarize(counts = n()) %>% 
  ## create a column for total counts
  group_by(slopepos, cover_type, plot) %>%
  dplyr::mutate(totalcounts = sum(counts)) %>% 
  ungroup() %>% 
  mutate(relabund = (counts/totalcounts)*100,
         relabund = round(relabund, 2)) 



###################
###################
###################

# PCA ---------------------------------------------------------------------
## install the ggbiplot package from github
## install the miraKlein version, not 

## you will need relative abundance data for PCA 


#devtools::install_github("miraKlein/ggbiplot")
library(ggbiplot)


## all samples ----
## first, make wider
relabund_pca =
  fticr_water_relabund %>%
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  #filter(slopepos == 'CON') %>% 
  ungroup %>% 
  dplyr::select(-c(counts, totalcounts),
                slopepos, cover_type) %>%
  pivot_wider(names_from = "Class", values_from = "relabund") %>% 
  replace(is.na(.),0) 
#dplyr::select(-1)


num = 
  relabund_pca %>% 
  dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))

grp = 
  relabund_pca %>% 
  dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`),
                slopenum, cover_type) %>% 
  dplyr::mutate(row = row_number())

pca = prcomp(num, scale. = T)

pca_fig = ggbiplot(pca, obs.scale = 1, var.scale = 1,
         groups = as.character(grp$slopenum), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=4,stroke=1, aes(color = groups, shape = grp$cover_type))+
  xlim(-5,5)+
  ylim(-5,5)+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  theme_er()+
  theme(legend.position = "none", panel.border = element_rect(color="white",size=0.2, fill = NA))+
  #theme(legend.position = "right", panel.border = element_rect(color="white",size=0.2, fill = NA))+
  NULL

ggsave("output/pcafig.tiff", plot = pca_fig, height = 5, width = 5)
ggsave("output/pcafig.jpeg", plot = pca_fig, height = 5, width = 5)
   
pca_fig_legend = ggbiplot(pca, obs.scale = 1, var.scale = 1,
                   groups = as.character(grp$slopenum), 
                   ellipse = TRUE, circle = FALSE, var.axes = TRUE) +
  geom_point(size=4,stroke=1, aes(color = groups, shape = grp$cover_type))+
  xlim(-5,5)+
  ylim(-5,5)+
  scale_color_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  theme_er()+
  theme(legend.position = "right", panel.border = element_rect(color="white",size=0.2, fill = NA))+
  #theme(legend.position = "right", panel.border = element_rect(color="white",size=0.2, fill = NA))+
  NULL

ggsave("output/pcafiglegend.tiff", plot = pca_fig_legend, height = 5, width = 8)



