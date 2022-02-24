#Old code

#keeping for code structure for histograms (nosc) with boxplots


# ggplot(fticr_water_nosc, aes(NOSC, color = cover_type, fill = cover_type)) +
#   geom_histogram(alpha = 0.3, position = "identity", binwidth = 0.1) +
#   geom_boxplot(aes(y = 800), width = 100, fill = NA)+
#   facet_grid(slopepos ~ .) +
#   theme_er() +
#   #scale_fill_manual(values = soil_palette("redox", 2)) +
#   #scale_color_manual(values = soil_palette("redox", 2)) +
#   #scale_color_manual(values = rev(nord("afternoon_prarie", 2)))+
#   #scale_fill_manual(values =rev(nord("afternoon_prarie", 2)))+
#   scale_fill_nord("victory_bonds", 2)+
#   scale_color_nord("victory_bonds", 2)+
#   ggtitle("NOSC, by slope position")
# 
# # NOSC by compound class
# fticr_water_nosc %>% 
#   mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>% 
#   ggplot(aes(NOSC, color = slopepos, fill = slopepos)) +
#   geom_histogram(alpha = 0.6, position = "identity", binwidth = 0.1) +
#   geom_boxplot(aes(y = 800), width = 100, fill = NA)+
#   #facet_grid(Material ~ .) +
#   theme_er() +
#   scale_fill_manual(values = PNWColors::pnw_palette("Bay", 3))+
#   scale_color_manual(values = PNWColors::pnw_palette("Bay", 3))+
#   ggtitle("NOSC, cover type")+
#   facet_grid(cover_type~.)


# fticr_water_nosc %>% 
#   mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>% 
#   ggplot(aes(y = NOSC, x = slopepos, color = slopepos, fill = slopepos)) +
#   geom_boxplot(alpha = 0.6, position = "identity") +
#   labs(x = "")+
#   #geom_boxplot(aes(y = 800), width = 100, fill = NA)+
#   #facet_grid(Material ~ .) +
#   theme_er() +
#   scale_fill_manual(values = PNWColors::pnw_palette("Bay", 3))+
#   scale_color_manual(values = PNWColors::pnw_palette("Bay", 3))+
#   facet_grid(cover_type~.)



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