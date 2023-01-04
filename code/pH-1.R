#ECRooney
#2022 3 23
#soil properties and texture
#Y1 fairbanks

#load libraries-------------------------------
source("code/fticr-0-packages.R")


#load data-------------------------------------
#properties = read.csv("processed/2021_12_6_shl_soilproperties.csv")
pH_data = read.csv("processed/y1_pH.csv")
metadata = read.csv("processed/Y1_metadata.csv") %>% rename(sample_id = ID)


pH_stats = 
  pH_data %>%
  left_join(metadata, by = "sample_id")

pH_summary = 
  pH_data %>%
  left_join(metadata, by = "sample_id") %>% 
  group_by(slopepos, cover_type) %>% 
  dplyr::summarise(mean = round(mean(pH), 2),
                   se = round(sd(pH)/sqrt(n()),2)) %>% 
  mutate(summary = paste(mean, "\u00b1", se)) %>% 
  dplyr::select(-mean, -se)


#clean

pH_summary %>% knitr::kable() # prints a somewhat clean table in the console

#export

write.csv(pH_summary, "output/pH_summary.csv", row.names = FALSE)


#stats
library(nlme)
pH_lme = lme(pH ~ slopepos*cover_type, random = ~1|morph, na.action = na.omit, data = pH_stats)
summary(pH_lme)
print(pH_lme)
anova(pH_lme)

pH_aov <- aov(pH ~ slopepos*cover_type, data = pH_stats)
summary.aov(pH_aov)


pH_hsd <- HSD.test(pH_aov, "slopepos")
print(pH_hsd)

pH_hsd <- HSD.test(pH_aov, "cover_type")
print(pH_hsd)


#####

pH_aov <- aov(pH ~ cover_type, data = pH_stats %>% filter(slopepos == "footslope"))
summary.aov(pH_aov)

pH_hsd <- HSD.test(pH_aov, "slopepos")
print(pH_hsd)

pH_hsd <- HSD.test(pH_aov, "cover_type")
print(pH_hsd)

