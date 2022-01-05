#xrd data
#Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

soil_data = read.csv("processed/soilproperties_OSUanalysis.csv") 

# 2. Process data---------------------------------

soil_data_processed =
  soil_data %>% 
  dplyr::mutate(grav = as.numeric(grav),
                LOI = as.numeric(LOI)) %>% 
  group_by(slopepos, cover_type) %>%
  dplyr::summarize(grav_mean = round(mean(grav), 3)*100,
                   grav_se = round(sd(grav)/sqrt(n()),3)*100,
                   LOI_mean = round(mean(LOI), 3)*100,
                   LOI_se = round(sd(LOI)/sqrt(n()),3)*100) %>% 
  mutate(grav_summary = paste(grav_mean, "\u00b1", grav_se),
         LOI_summary = paste(LOI_mean, "\u00b1", LOI_se))


tree_data_processed =
  soil_data %>% 
  group_by(slopepos, cover_type) %>%
  dplyr::summarize(DBH_mean = round(mean(DBH..cm.), 3),
                   DBH_se = round(sd(DBH..cm.)/sqrt(n()),3)) %>% 
  na.omit() %>% 
  mutate(DBH_summary = paste(DBH_mean, "\u00b1", DBH_se)) %>% 
  select(-c(DBH_mean, DBH_se))
  
soil_data_table =
  soil_data_processed %>% 
  select(-c(LOI_mean, LOI_se, grav_mean, grav_se))

soil_data_table %>% knitr::kable()
tree_data_processed %>% knitr::kable() 

write.csv(tree_data_processed, "output/tree_data_table.csv", row.names = FALSE)
write.csv(soil_data_table, "output/soil_data_table.csv", row.names = FALSE)


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



tree_data_stats =
  soil_data %>% 
  filter(cover_type %in% "Canopy") %>% 
  select(c(slopepos, DBH..cm.)) %>% 
  group_by(slopepos)  

dbh_aov <- aov(DBH..cm. ~ slopepos, data = tree_data_stats)
summary.aov(dbh_aov)

dbh_hsd = HSD.test(dbh_aov, "slopepos")
print(dbh_hsd)

####

#Stats


library(nlme)


soil_properties_stats =
  soil_data %>% 
  #filter(cover_type %in% "Canopy") %>% 
  select(c(slopepos, cover_type, grav, LOI, Horizonation)) %>% 
  
  

LOI_aov <- aov(LOI ~ slopepos*cover_type, data = soil_properties_stats)
summary.aov(LOI_aov)

LOI_slopepos_hsd = HSD.test(LOI_aov, "slopepos")
print(LOI_slopepos_hsd)

#

LOI2_aov <- aov(LOI ~ slopepos, data = soil_properties_stats)
summary.aov(LOI2_aov)

LOI2_slopepos_hsd = HSD.test(LOI2_aov, "slopepos")
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
