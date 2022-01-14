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

fticr_water_nosc =
  fticr_water_nosc  
#mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))

ggplot(fticr_water_nosc, aes(NOSC, color = cover_type, fill = cover_type)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
  geom_boxplot(aes(y = 800), width = 100, fill = NA)+
  facet_grid(slopepos ~ .) +
  theme_er() +
  #scale_fill_manual(values = soil_palette("redox", 2)) +
  #scale_color_manual(values = soil_palette("redox", 2)) + 
  #scale_color_manual(values = rev(nord("afternoon_prarie", 2)))+
  #scale_fill_manual(values =rev(nord("afternoon_prarie", 2)))+
  scale_fill_nord("victory_bonds", 2)+
  scale_color_nord("victory_bonds", 2)+
  ggtitle("NOSC, by slope position")
#facet_grid(Material~Trtmt)

# NOSC by compound class
fticr_water_nosc %>% 
  mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>% 
  ggplot(aes(NOSC, color = slopepos, fill = slopepos)) +
  geom_histogram(alpha = 0.6, position = "identity", binwidth = 0.1) +
  geom_boxplot(aes(y = 800), width = 100, fill = NA)+
  #facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual(values = PNWColors::pnw_palette("Bay", 3))+
  scale_color_manual(values = PNWColors::pnw_palette("Bay", 3))+
  ggtitle("NOSC, cover type")+
  facet_grid(cover_type~.)



fticr_water_nosc %>% 
  mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>% 
  ggplot(aes(y = NOSC, x = slopepos, color = slopepos, fill = slopepos)) +
  geom_boxplot(alpha = 0.6, position = "identity") +
  labs(x = "")+
  #geom_boxplot(aes(y = 800), width = 100, fill = NA)+
  #facet_grid(Material ~ .) +
  theme_er() +
  scale_fill_manual(values = PNWColors::pnw_palette("Bay", 3))+
  scale_color_manual(values = PNWColors::pnw_palette("Bay", 3))+
  facet_grid(cover_type~.)


######################
#unique peaks

# this does only unique loss/gain by open vs. canopy
fticr_water_slopepos_unique = 
  fticr_data_water_summarized %>% 
  # calculate n to see which peaks were unique vs. common
  group_by(formula, cover_type, plot) %>% 
  dplyr::mutate(n = n()) %>% 
  # n = 1 means unique to CON or FTC Trtmt
  # n = 2 means common to both
  filter(n == 1) %>% 
  #PROBLEMS START NOW
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





# fticr_water_nosc %>% 
#   filter(Site == "TOOL") %>% 
#   ggplot(aes(NOSC, color = Trtmt, fill = Trtmt)) +
#   geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
#   facet_grid(Material ~ .) +
#   theme_er() +
#   scale_fill_manual(values = PNWColors::pnw_palette("Bay", 2))+
#   scale_color_manual(values = PNWColors::pnw_palette("Bay", 2))+
#   ggtitle("NOSC, Water Extracted TOOL")+
#   facet_grid(Material~Class)
# 
# fticr_water_nosc %>% 
#   filter(Trtmt == "FTC") %>% 
#   ggplot(aes(NOSC, color = Site, fill = Site)) +
#   geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
#   facet_grid(Material ~ .) +
#   theme_er() +
#   scale_fill_nord("victory_bonds", 2)+
#   scale_color_nord("victory_bonds", 2)+
#   ggtitle("NOSC, Water Extracted FTC")+
#   facet_grid(Material~Class)
# 
# fticr_water_nosc %>% 
#   filter(Trtmt == "CON") %>% 
#   ggplot(aes(NOSC, color = Site, fill = Site)) +
#   geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
#   facet_grid(Material ~ .) +
#   theme_er() +
#   scale_fill_nord("victory_bonds", 2)+
#   scale_color_nord("victory_bonds", 2)+
#   ggtitle("NOSC, Water Extracted CON")+
#   facet_grid(Material~Class)
# 
# 
# ggplot(fticr_water_nosc, aes(NOSC, color = Trtmt, fill = Trtmt)) +
#   geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.05) +
#   facet_grid(Material ~ .) +
#   theme_er() +
#   scale_fill_manual (values = soil_palette("eutrostox", 2)) +
#   scale_color_manual(values = soil_palette("eutrostox", 2)) +
#   ggtitle("NOSC, Water Extracted by Treatment")
# 
# ggplot(fticr_water_nosc, aes(NOSC, color = Trtmt, fill = Trtmt)) +
#   geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.1) +
#   facet_grid(Material ~ Site) +
#   theme_er() +
#   scale_color_manual(values = rev(PNWColors::pnw_palette("Starfish", 2)))+
#   scale_fill_manual(values = rev(PNWColors::pnw_palette("Starfish", 2)))+
#   ggtitle("NOSC, Water Extracted")
# 
# ggplot(fticr_water_nosc, aes(NOSC, color = Site))+
#   geom_histogram(aes(y = stat(count)/sum(count))) +
#   scale_y_continuous(labels = scales::percent)
# 
# ggplot(fticr_water_nosc, aes(x = NOSC, color = Site, fill = Site))+
#   geom_histogram(alpha = 0.5, position = "identity")+
#   facet_grid(Material~.) + 
#   theme_er() +
#   scale_fill_manual (values = soil_palette("gley", 2)) +
#   scale_color_manual (values = soil_palette("gley", 2)) +
#   ggtitle("NOSC, Water Extracted")