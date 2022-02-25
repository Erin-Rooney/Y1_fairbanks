#NOSC unused

backslope_unique_nosc = 
  fticr_water_slopepos_unique %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  #mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>%
  filter(slopepos %in% "backslope") %>% 
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


# Van Krevelen usused



# fticr_water = 
#   fticr_data_water %>% 
#   select(formula, Site, Trtmt, Material) 
# 
# fticr_water_trt = 
#   fticr_water %>% 
#   distinct(Site, Trtmt, Material, formula) %>% 
#   left_join(fticr_water_nosc_trt) %>% 
#   mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))
