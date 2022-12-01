#ECRooney
#2022 3 23
#soil properties and texture
#Y1 fairbanks

#load libraries-------------------------------
source("code/fticr-0-packages.R")


#load data-------------------------------------
#properties = read.csv("processed/2021_12_6_shl_soilproperties.csv")
texture = read.csv("processed/y1_texture.csv")
metadata = read.csv("processed/Y1_metadata.csv") %>% rename(sample_id = ID)




texture_long = 
  texture %>%
  left_join(metadata, by = "sample_id") %>% 
  pivot_longer(-c(sample_id, slopepos_num, slopepos, cover_type, rep, top_cm, 
                  mid_cm, bottom_cm, morph), names_to = "particle_size", 
               values_to = "data") %>% 
  mutate (particle_size = factor(particle_size, levels = c("clay_perc", "silt_perc",
                                                           "sand_perc"))) %>%
  na.omit() %>% 
  group_by(slopepos, cover_type, particle_size) %>% 
  dplyr::summarise(mean = round(mean(data), 2),
                   se = round(sd(data)/sqrt(n()),2)) %>% 
  mutate(summary = paste(mean, "\u00b1", se)) %>% 
  dplyr::select(-mean, -se)


#clean

texture_long %>% knitr::kable() # prints a somewhat clean table in the console

#export

write.csv(texture_long, "output/texture_long.csv", row.names = FALSE)



