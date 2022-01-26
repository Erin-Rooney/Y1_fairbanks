# 3-12-2021
# FTICR
# NOSC
# Fairbanks Y1

#load packages
source("code/fticr-0-packages.R")

# 1. Load files-----------------------------------

fticr_data_water = read.csv("fticr_data_water.csv")
fticr_meta_water = read.csv("fticr_meta_water.csv")
meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv")

# 2. NOSC and AImod plots_water-------------------------------
fticr_water = 
  fticr_data_water %>% 
  select(ID, formula, slopepos, cover_type, plot) 

fticr_data_water_summarized = 
  fticr_water %>% 
  distinct(ID, slopepos, cover_type, plot, formula) %>% mutate(presence = 1)


#3. NOSC--------------------------------

#merge _summarized with _meta

fticr_water_nosc = 
  fticr_data_water_summarized %>%
  left_join(fticr_meta_water) %>%
  dplyr::select(formula, NOSC, HC, OC, Class, slopepos, cover_type, plot)


######################
#unique peaks

# this does only unique loss/gain by open vs. canopy

fticr_data_water_summarized_unique = 
  fticr_water %>% 
  distinct(slopepos, cover_type, plot, formula) %>% mutate(presence = 1)

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
  mutate(slopepos = factor (slopepos, levels = c("Backslope", "Low Backslope", "Footslope")))

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
  mutate(slopepos = factor (slopepos, levels = c("Backslope", "Low Backslope", "Footslope"))) 



fticr_water_slopepos_unique = 
  fticr_data_water_summarized_unique %>% 
  # calculate n to see which peaks were unique vs. common
  group_by(formula, cover_type, plot) %>% 
  dplyr::mutate(n = n()) %>% 
  # n = 1 means unique to CON or FTC Trtmt
  # n = 2 means common to both
  filter(n == 1) %>% 
  mutate(uniquepeak = case_when(slopepos == 'Backslope' ~ "backslope",
                               slopepos == 'Low Backslope' ~ "low backslope",
                               slopepos == 'Footslope' ~ "footslope")) %>% 
  left_join(meta_hcoc_water) %>% 
  mutate(slopepos = factor (slopepos, levels = c("Backslope", "Low Backslope", "Footslope")))

fticr_water_slopepos_unique %>% 
  mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>% 
  ggplot(aes(y = NOSC, x = slopepos, fill = slopepos)) +
  geom_boxplot(alpha = 0.5)+
  geom_jitter(width = 0.2, alpha = 0.7, size = 0.5)+
  labs(x = "")+
  #geom_boxplot(aes(y = 800), width = 100, fill = NA)+
  #facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual(values = PNWColors::pnw_palette("Bay", 3))+
  scale_color_manual(values = PNWColors::pnw_palette("Bay", 3))+
  facet_grid(cover_type~.)


backslope_unique_nosc = 
  fticr_water_slopepos_unique %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  #mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>%
  filter(slopepos %in% "Backslope") %>% 
  ggplot(aes(y = NOSC, x = cover_type, fill = cover_type)) +
  geom_boxplot(alpha = 0.5)+
  geom_jitter(width = 0.2, alpha = 0.7, size = 0.5)+
  labs(x = "")+
  scale_fill_manual(values = c('#006d77', '#e29578'))+
  #geom_boxplot(aes(y = 800), width = 100, fill = NA)+
  #facet_grid(Material ~ .) +
  theme_er() +
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.2, fill = NA))


ggsave("output/backslope_unique_nosc.tiff", plot = backslope_unique_nosc, height = 3, width = 3)
ggsave("output/backslope_unique_nosc.jpeg", plot = backslope_unique_nosc, height = 3, width = 3)


unique_nosc = 
  fticr_water_slopepos_unique %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  #mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>%
  #filter(slopepos %in% "Backslope") %>% 
  ggplot(aes(y = NOSC, x = cover_type, fill = cover_type)) +
  geom_boxplot(alpha = 0.5)+
  geom_jitter(width = 0.2, alpha = 0.7, size = 0.5)+
  labs(x = "")+
  scale_fill_manual(values = c('#006d77', '#e29578'))+
  #geom_boxplot(aes(y = 800), width = 100, fill = NA)+
  facet_grid(slopepos ~ .) +
  theme_er() +
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.2, fill = NA))

ggsave("output/unique_nosc.tiff", plot = unique_nosc, height = 7, width = 3)
  

# BACKSLOPE STATS---------------------------

library(nlme)


backslope_unique_nosc_stats = 
  fticr_water_slopepos_unique %>% 
  #mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>%
  filter(slopepos %in% "Backslope") 

aov_backslope = aov(NOSC ~ cover_type, data = backslope_unique_nosc_stats)
summary(aov_backslope)
print(aov_backslope)


aov_nosc = aov(NOSC ~ cover_type*slopepos, data = fticr_water_slopepos_unique)
summary(aov_nosc)
print(aov_nosc)


