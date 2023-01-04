#xrd data
#Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

soil_data = read.csv("processed/soilproperties_OSUanalysis.csv") 
om_data = read.csv("processed/OMdepth.csv")


# 2. Process data---------------------------------

om_data_processed =
  om_data %>% 
  group_by(slopepos, cover_type) %>%
  dplyr::summarize(om_mean = round(mean(depth), 1),
                   om_se = round(sd(depth)/sqrt(n()),1),
                   om_min = round(min(depth),1),
                   om_max = round(max(depth),1),
                   om_max_min = ((om_max+om_min)/2)) %>% 
  mutate(om_summary = paste(om_mean, "\u00b1", om_se)) %>% 
  select(-c(om_mean, om_se))


om_data_processed %>% knitr::kable()

write.csv(om_data_processed, "output/om_data_processed.csv", row.names = FALSE)



# 4. Figures

tree_data_table =
  soil_data %>%
  group_by(slopepos, cover_type) %>%
  dplyr::summarize(DBH_mean = round(mean(DBH..cm.), 3),
                   DBH_se = round(sd(DBH..cm.)/sqrt(n()),3)) %>%
  na.omit()


dbh = tree_data_table %>%
  mutate(slopepos = factor(slopepos, levels = c("Backslope", "Low Backslope", "Footslope"))) %>%
  ggplot(aes(x = slopepos, y = DBH_mean))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin=(DBH_mean-DBH_se/2),ymax=(DBH_mean+DBH_se/2)),width=.2,position=position_dodge(.9))+
  labs(y = "DBH, cm",
       x = "")+
  theme_er()+
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+
  #facet_grid(.~slopepos)+
  NULL

ggsave("output/dbh.tiff", plot = dbh, height = 5, width = 4)
ggsave("output/dbh.jpeg", plot = dbh, height = 5, width = 4)



om_aov <- aov(depth ~ slopepos*cover_type, data = om_data)
summary.aov(om_aov)

om_hsd = HSD.test(om_aov, "slopepos")
print(om_hsd)

####

#Stats


library(nlme)


soil_properties_stats =
  soil_data %>% 
  #filter(cover_type %in% "Canopy") %>% 
  select(c(slopepos, cover_type, grav, LOI, Horizonation)) 
  
  

LOI_aov <- aov(LOI ~ slopepos*cover_type, data = soil_properties_stats)
summary.aov(LOI_aov)


LOI_slopepos_hsd = HSD.test(LOI_aov, "slopepos")
print(LOI_slopepos_hsd)

LOI_slopepos_hsd = HSD.test(LOI_aov, "cover_type")
print(LOI_slopepos_hsd)
#

LOI2_aov <- aov(LOI ~ cover_type, data = soil_properties_stats %>%  filter(slopepos == "Backslope"))
summary.aov(LOI2_aov)

LOI2_slopepos_hsd = HSD.test(LOI2_aov, "cover_type")
print(LOI2_slopepos_hsd)


LOI_interaction = lme(LOI ~ slopepos, random = ~1|Horizonation, na.action = na.omit, data = soil_properties_stats)

summary(LOI_interaction)
print(LOI_interaction)
anova(LOI_interaction)

LOI3_slopepos_hsd = HSD.test(LOI_interaction, "slopepos")
print(LOI2_slopepos_hsd)

#

grav_aov <- aov(grav ~ slopepos*cover_type, data = soil_properties_stats)
summary.aov(grav_aov)

grav_slopepos_hsd = HSD.test(grav_aov, "slopepos")
print(grav_slopepos_hsd)

grav_slopepos_hsd = HSD.test(grav_aov, "cover_type")
print(grav_slopepos_hsd)

#

# 
# 
# grav_aov <- aov(grav ~ cover_type, data = soil_properties_stats %>%  filter(slopepos == "Backslope"))
# summary.aov(grav_aov)
# 
# grav_slopepos_hsd = HSD.test(grav_aov, "slopepos")
# print(grav_slopepos_hsd)
# 
# grav_slopepos_hsd = HSD.test(grav_aov, "cover_type")
# print(grav_slopepos_hsd)

#

dbh_stats <- aov(DBH..cm. ~ slopepos, data = soil_data)
summary.aov(dbh_stats)

dbh_hsd = HSD.test(dbh_stats, "slopepos")
print(dbh_hsd)
