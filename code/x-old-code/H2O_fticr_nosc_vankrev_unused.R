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


# plot only lost/gained
backslope_vankrev = 
  fticr_water_covertype_unique %>%
  filter(slopepos %in% "backslope") %>% 
  ggplot(aes(x = OC, y = HC, color = loss_gain))+
  geom_point(alpha = 0.4, size = 1)+
  stat_ellipse(show.legend = F)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  labs(x = "O/C",
       y = "H/C")+
  scale_color_manual(values = c('#006d77', '#e29578'))+
  #facet_grid(slopepos ~ .)+
  theme_er() +
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.2, fill = NA))


ggsave("output/vankrev_backslope.tiff", plot = backslope_vankrev, height = 3, width = 3)
ggsave("output/vankrev_backslope.jpeg", plot = backslope_vankrev, height = 3, width = 3)


#not working anyway

# plot common as well as lost/gained
fticr_water_covertype_unique_common %>% 
  filter(loss_gain == "common") %>% 
  ggplot()+
  geom_point(aes(x = OC, y = HC), color = "grey80", alpha = 0.2, size = 1)+
  geom_point(data = fticr_water_covertype_unique_common %>% filter(loss_gain != "common"), 
             aes(x = OC, y = HC, color = loss_gain), alpha = 0.2, size = 1)+
  #geom_point(alpha = 0.2, size = 1)+
  #stat_ellipse(show.legend = F)+
  geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
  geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
  guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
  labs(caption = "grey = common to both")+
  facet_grid(Material ~ Site)+
  theme_er() +
  scale_color_manual (values = rev(soil_palette("redox", 2)))



# fticr_water_hcoc %>% 
#   filter(site %in% "catalina", 
#          slopepos %in% "divergent") %>% 
#   ggplot(aes(x=OC, y=HC, color = OC))+
#   geom_point(alpha = 0.2, size = 1)+
#   #stat_ellipse(show.legend = F)+
#   #stat_ellipse()+
#   facet_grid(meshbag ~ substrate)+
#   #geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
#   #geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
#   #geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
#   guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
#   labs(title = "FTICR-MS, Catalina Divergent",
#        x = "O:C",
#        y = "H:C",
#        color = "NOSC")+
#   theme_er()+
#   scale_color_gradientn(colors = pnw_palette("Starfish"))


# fticr_water_hcoc %>% 
#   filter(site %in% "catalina", 
#          slopepos %in% "convergent") %>% 
#   ggplot(aes(x=OC, y=HC, color = OC))+
#   geom_point(alpha = 0.2, size = 1)+
#   #stat_ellipse(show.legend = F)+
#   #stat_ellipse()+
#   facet_grid(meshbag ~ substrate)+
#   #geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
#   #geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
#   #geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
#   guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
#   labs(title = "FTICR-MS, Catalina Convergent",
#        x = "O:C",
#        y = "H:C",
#        color = "NOSC")+
#   theme_er()+
#   scale_color_gradientn(colors = pnw_palette("Starfish"))

# fticr_water_hcoc %>% 
#   ggplot(aes(x=OC, y=HC, color = Site))+
#   geom_point(alpha = 0.2, size = 1)+
#   stat_ellipse(show.legend = F)+
#   facet_grid(Material ~.)+
#   geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
#   geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
#   geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
#   guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
#   ggtitle("Water extracted FTICR-MS")+
#   theme_er() +
#   scale_color_manual (values = soil_palette("redox", 2))
# 
# fticr_water_hcoc %>% 
#   ggplot(aes(x=OC, y=HC, color = Site))+
#   geom_point(alpha = 0.2, size = 1)+
#   stat_ellipse(show.legend = F)+
#   facet_grid(Material ~ Trtmt)+
#   geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
#   geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
#   geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
#   guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
#   theme_bw()


# fticr_water_hcoc %>% 
#   ggplot(aes(x=OC, y=HC, color = Trtmt))+
#   geom_point(alpha = 0.2, size = 1)+
#   stat_ellipse(show.legend = F)+
#   facet_grid(Material ~.)+
#   geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
#   geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
#   geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
#   guides(colour = guide_legend(override.aes = list(alpha=1, size=2)))+
#   ggtitle("Water extracted FTICR-MS")+
#   facet_grid(Material ~ Site)+
#   theme_er() +
#   scale_color_manual (values = soil_palette("redox", 2))
