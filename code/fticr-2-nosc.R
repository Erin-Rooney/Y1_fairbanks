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
  select(formula, slopepos, cover_type, plot) %>% 
  na.omit()

fticr_data_water_summarized = 
  fticr_water %>% 
  distinct(slopepos, cover_type, plot, formula) %>% mutate(presence = 1)


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
  left_join(fticr_meta_water) %>% 
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
  mutate(uniquepeak = case_when(slopepos == 'backslope' ~ "backslope",
                               slopepos == 'low backslope' ~ "low backslope",
                               slopepos == 'footslope' ~ "footslope")) %>% 
  left_join(meta_hcoc_water) %>% 
  left_join(fticr_meta_water) %>% 
  mutate(slopepos = factor (slopepos, levels = c("backslope", "low backslope", "footslope")))

fticr_water_slopepos_unique %>% 
  mutate(slopepos = factor (slopepos, levels = c("footslope", "low backslope","backslope"))) %>% 
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


nosc_classes = 
  fticr_water_slopepos_unique %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  ggplot(aes(x = cover_type, y = NOSC, fill = cover_type)) +
  geom_boxplot(alpha = 0.5)+
  geom_jitter(width = 0.2, alpha = 0.7, size = 0.5)+
  facet_grid(slopepos~Class)+
  labs(x = "")+
  ylim(-2,2)+
  scale_fill_manual(values = c('#006d77', '#e29578'))+
  theme_er()+
  theme(legend.position = "none", 
        axis.text.x.bottom = element_text 
        (vjust = 0.5, hjust=0.6, angle = 0),
        panel.border = element_rect(color="black",size=0.5, fill = NA) 
  )

ggsave("output/nosc_class.tiff", plot = nosc_classes, height = 6, width = 9)
ggsave("output/nosc_class.jpeg", plot = nosc_classes, height = 6, width = 9)
  

# BACKSLOPE STATS---------------------------

library(nlme)


# backslope_unique_nosc_stats = 
#   fticr_water_slopepos_unique %>% 
#   #mutate(slopepos = factor (slopepos, levels = c("Footslope", "Low Backslope","Backslope"))) %>%
#   filter(slopepos %in% "backslope") 
# 
# aov_backslope = aov(NOSC ~ cover_type, data = backslope_unique_nosc_stats)
# summary(aov_backslope)
# print(aov_backslope)


aov_nosc = aov(NOSC ~ cover_type*slopepos, data = fticr_water_slopepos_unique)
summary(aov_nosc)
print(aov_nosc)

nosc_hsd <- HSD.test(aov_nosc, "slopepos")
print(nosc_hsd)

nosc_hsd <- HSD.test(aov_nosc, "cover_type")
print(nosc_hsd)

###Class 

aov_nosc_class = aov(NOSC ~ Class*slopepos*cover_type, data = fticr_water_slopepos_unique)
summary(aov_nosc_class)
print(aov_nosc_class)

###backslope

BACKaliph = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                  filter(slopepos == 'backslope', Class == 'aliphatic'))
summary(BACKaliph)
print(BACKaliph)
anova(BACKaliph)

BACKaro = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                filter(slopepos == 'backslope', Class == 'aromatic'))
summary(BACKaro)
print(BACKaro)
anova(BACKaro)


BACKcaro = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                 filter(slopepos == 'backslope', Class == 'condensed aromatic'))
summary(BACKcaro)
print(BACKcaro)
anova(BACKcaro)


BACKlig = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                 filter(slopepos == 'backslope', Class == 'unsaturated/lignin'))
summary(BACKlig)
print(BACKlig)
anova(BACKlig)

###low backslope

LOWBACKaliph = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                  filter(slopepos == 'low backslope', Class == 'aliphatic'))
summary(LOWBACKaliph)
print(LOWBACKaliph)
anova(LOWBACKaliph)

LOWBACKaro = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                filter(slopepos == 'low backslope', Class == 'aromatic'))
summary(LOWBACKaro)
print(LOWBACKaro)
anova(LOWBACKaro)


LOWBACKcaro = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                 filter(slopepos == 'low backslope', Class == 'condensed aromatic'))
summary(LOWBACKcaro)
print(LOWBACKcaro)
anova(LOWBACKcaro)


LOWBACKlig = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                filter(slopepos == 'low backslope', Class == 'unsaturated/lignin'))
summary(LOWBACKlig)
print(LOWBACKlig)
anova(LOWBACKlig)


###footslope

FOOTaliph = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                     filter(slopepos == 'footslope', Class == 'aliphatic'))
summary(FOOTaliph)
print(FOOTaliph)
anova(FOOTaliph)

FOOTaro = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                   filter(slopepos == 'footslope', Class == 'aromatic'))
summary(FOOTaro)
print(FOOTaro)
anova(FOOTaro)


FOOTcaro = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                    filter(slopepos == 'footslope', Class == 'condensed aromatic'))
summary(FOOTcaro)
print(FOOTcaro)
anova(FOOTcaro)


FOOTlig = aov(NOSC ~ cover_type, data = fticr_water_slopepos_unique %>% 
                   filter(slopepos == 'footslope', Class == 'unsaturated/lignin'))
summary(FOOTlig)
print(FOOTlig)
anova(FOOTlig)
