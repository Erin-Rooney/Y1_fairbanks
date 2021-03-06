# 3-12-2021
# FTICR
# Van Krevelen
# y1 fairbanks

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
  na.omit()

fticr_data_water_summarized = 
  fticr_water %>% 
  distinct(ID, slopepos, cover_type, plot, formula) %>% mutate(presence = 1)


# van krevelen plots_water------------------------------------------------------

fticr_water_hcoc =
  fticr_data_water_summarized %>% 
  left_join(fticr_meta_water) %>% 
  dplyr::select(formula, slopepos, cover_type, plot, HC, OC) %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) 

fticr_water_hcoc_summarized =
  fticr_data_water_summarized %>% 
  left_join(fticr_meta_water) %>% 
  dplyr::select(formula, slopepos, cover_type, plot, HC, OC) %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  group_by(cover_type, slopepos) %>% 
  dplyr::mutate(n= n()) %>% 
  group_by(cover_type, slopepos) %>% 
  dplyr::summarise(n = n())


# 

gglabel2 = tribble(
 ~x, ~y, ~label, ~slopepos,
 1.0, 2.25, "aliphatic", "footslope",
 1.0, 1.25, "lignin-like", "footslope",
 0.2, 0.85, "aromatic", "footslope",
 0.4, 0.35, "condensed aromatic", 'footslope',
) %>% 
  mutate(slopepos = factor (slopepos, levels = c("backslope", "low backslope", "footslope")))



# method figure ------------------------------

vankrev_method = 
  fticr_water_hcoc %>% 
  ggplot(aes(x=OC, y=HC, color = OC))+
  geom_point(alpha = 0.6, size = 2)+
  #stat_ellipse(show.legend = F)+
  #stat_ellipse()+
  #facet_grid(slopepos ~ .)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  geom_text(data = gglabel2, aes(x = x, y = y, label = label), color = "black", size = 4.5)+
  #guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  scale_color_gradientn(colors = rev(pnw_palette("Sailboat")))+
  labs(x = "O/C",
       y = "H/C",
       color = "")+
  theme_er()+
  theme(panel.border = element_rect(color="black",size=0.5, fill = NA), legend.position = "none")+
  NULL

ggsave("output/vankrev_method.tiff", plot = vankrev_method, height = 6, width = 6.5)


vankrev_covertype = 
  fticr_water_hcoc %>% 
  mutate(slopepos = factor (slopepos, levels = c("backslope", "low backslope", "footslope"))) %>%
  ggplot(aes(x=OC, y=HC, color = slopepos))+
  geom_point(alpha = 0.2, size = 1)+
  # stat_ellipse(show.legend = F)+
  # stat_ellipse()+
  facet_grid(.~cover_type)+
  # geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  # geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  # geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  labs(x = "O/C",
       y = "H/C",
       color = "")+
  scale_color_manual(values = rev(pnw_palette("Bay", 3)))+
  theme_er()+
  theme(legend.position = "bottom", panel.border = element_rect(color="black",size=0.2, fill = NA))
    

ggsave("output/vankrev_covertype.tiff", plot = vankrev_covertype, height = 4, width = 6)
ggsave("output/vankrev_covertype.jpeg", plot = vankrev_covertype, height = 4, width = 6)


## calculate peaks lost/gained ---- 

fticr_data_water_summarized_unique = 
  fticr_water %>% 
  distinct(slopepos, cover_type, plot, formula) %>% mutate(presence = 1)


# this does only unique loss/gain by open vs. canopy
fticr_water_covertype_unique = 
  fticr_data_water_summarized_unique %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  # calculate n to see which peaks were unique vs. common
  group_by(formula, slopepos, plot) %>% 
  dplyr::mutate(n = n()) %>% 
  # n = 1 means unique to CON or FTC Trtmt
  # n = 2 means common to both
  filter(n == 1) %>% 
  #PROBLEMS START NOW
  mutate(loss_gain = if_else(cover_type == "open", "open", "closed")) %>% 
  left_join(meta_hcoc_water) %>% 
  mutate(slopepos = factor (slopepos, levels = c("backslope", "low backslope", "footslope")))

#common isn't really used anywhere in the analysis/figures 

fticr_water_covertype_unique_common = 
  fticr_data_water_summarized_unique %>% 
  # calculate n to see which peaks were unique vs. common
  group_by(formula, slopepos, cover_type) %>% 
  dplyr::mutate(n = n()) %>% 
  # n = 1 means unique to CON or FTC Trtmt
  # n = 2 means common to both
  # filter(n == 1) %>% 
  mutate(loss_gain = case_when(n == 2 ~ "common",
                               (n == 1 & cover_type == "open") ~ "open",
                               (n == 1 & cover_type == "closed") ~ "closed")) %>% 
  left_join(meta_hcoc_water) %>% 
  mutate(slopepos = factor (slopepos, levels = c("backslope", "low backslope", "footslope"))) 


uniquepeaks_vankrev = 
  fticr_water_covertype_unique %>%
  ggplot(aes(x = OC, y = HC, color = loss_gain))+
  geom_point(alpha = 0.2, size = 1)+
  stat_ellipse(show.legend = F)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  geom_text(data = gglabel2, aes(x = x, y = y, label = label), color = "black", size = 3.5)+
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  labs(x = "O/C",
       y = "H/C")+
  scale_color_manual(values = c('#006d77', '#e29578'))+
  facet_grid(slopepos ~ .)+
  theme_er() +
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.2, fill = NA))

ggsave("output/uniquepeaks_vankrev.tiff", plot = uniquepeaks_vankrev, height = 7, width = 3)

  