# 2-23-2022
# FTICR
# Relabund by horizon

#load packages
source("code/FTICR-0-packages.R")

fticr_data_water = read.csv("fticr_data_water.csv") %>% select(plot, formula, slopepos, cover_type, ID) 
fticr_meta_water = read.csv("fticr_meta_water.csv")
metadata = read.csv("processed/Y1_metadata.csv")


horizonation_relabund = 
  fticr_data_water %>% 
  left_join(select(fticr_meta_water, formula, Class), by = "formula") %>% 
  ## create a column for group counts
  mutate(Class = factor(Class, levels = c("aliphatic", "unsaturated/lignin", "aromatic", "condensed aromatic"))) %>% 
  #group_by(slopepos, cover_type, plot, ID, Class) %>%
  group_by(ID, Class) %>% 
  dplyr::summarize(counts = n()) %>% 
  ## create a column for total counts
  group_by(ID) %>%
  dplyr::mutate(totalcounts = sum(counts)) %>% 
  ungroup() %>% 
  mutate(relabund = (counts/totalcounts)*100,
         relabund = round(relabund, 2)) 



metadata2 = 
  metadata %>% 
  separate(ID, sep = " ", into = c("site", "ID")) %>% 
  mutate(ID = as.integer(ID)) %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  rename(plot = rep)

relabund_metadatacombo =
  horizonation_relabund %>% 
  left_join(metadata2, by = c("ID")) %>% 
  select(-slopepos_num, -site, -ID, -mid_cm, -counts, -totalcounts) %>% 
  pivot_wider(names_from = 'Class', values_from = "relabund") %>% 
  mutate(depth = paste(top_cm, "-", bottom_cm)) %>% 
  select(-top_cm, -bottom_cm)

##something incorrect with depth. Backslope rep C Bg horizon is NOT 0-8 cm
##likely a result of having FOUR reps due to sampling issues (rep C was not sampled, rep D was, but it referred to as rep C in analysis)

relabund_metadatacombo %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(relabund_metadatacombo, "output/relabund_horizonation.csv", row.names = FALSE)
