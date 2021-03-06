# 3-11-2021
# FTICR
# Relabund

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

#replaced ID with plot in line 12, moved plot to beginning (it was already at the end of line 12)

fticr_data_water = read.csv("fticr_data_water.csv") %>% select(plot, formula, slopepos, cover_type)
# fticr_data_water = read.csv("fticr_data_water.csv") %>% select(plot, formula, slopepos, cover_type, ID) 
fticr_meta_water = read.csv("fticr_meta_water.csv")
#meta_hcoc_water  = read.csv("fticr_meta_hcoc_water.csv") %>% select(-Mass)
### ^^ the files above have aliph as well as aromatic for the same sample, which can be confusing/misleading
### create an index combining them


# fticr_data_water_summarized = 
#   fticr_data_water %>% 
#   distinct(ID, slopepos, cover_type, plot, formula) %>% mutate(presence = 1)

#2. Calculate Relabund
## aromatic rel_abund

#replaced ID with plot in line 33 (plot was already at the end before Class)
#same for line 37

fticr_water_relabund = 
  fticr_data_water %>% 
  left_join(select(fticr_meta_water, formula, Class), by = "formula") %>% 
  ## create a column for group counts
  #mutate(Class = factor(Class, levels = c("aliphatic", "unsaturated/lignin", "aromatic", "condensed aromatic"))) %>% 
  group_by(slopepos, cover_type, plot, Class) %>%
  # group_by(ID, Class) %>% 
  dplyr::summarize(counts = n()) %>% 
  ## create a column for total counts
  group_by(plot, slopepos, cover_type) %>%
  dplyr::mutate(totalcounts = sum(counts)) %>% 
  ungroup() %>% 
  mutate(relabund = (counts/totalcounts)*100,
         relabund = round(relabund, 2)) %>% 
mutate(slopepos = factor(slopepos, levels = c("backslope", "low backslope", "footslope"))) %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) %>% 
  na.omit()
  



#2b. relabund summary by treatment ---


fticr_water_relabund_summarized = 
  fticr_water_relabund %>% 
  mutate(Class = factor(Class, levels = c("aliphatic", "unsaturated/lignin", "aromatic", "condensed aromatic"))) %>% 
  group_by(slopepos, cover_type, Class) %>% 
  #just removed plot from line 57 # 2 25 2022
  #group_by(ID, Class) %>% 
  dplyr::summarise(relabundance = round(mean(relabund), 2),
                   se = round(sd(relabund)/sqrt(n()),2))




# run anova to get statistical significance
# then use that to create the label file below


# bar graph
relabund_graph = 
  fticr_water_relabund_summarized %>% 
  ggplot(aes(x = cover_type, y = relabundance))+
  geom_bar(aes(fill = Class), stat = "identity")+
  facet_wrap(slopepos ~ .)+
  labs(x = "", 
       y = "Relative Abundance, %")+
  scale_fill_manual(values = (pnw_palette("Sunset",4)))+
  #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
  theme_er()+
  theme(legend.position = 'bottom', panel.border = element_rect(color="white",size=0.5, fill = NA))+
  NULL

#figure relabund ~ slopepos

relabund_slopepos_figure =
  fticr_water_relabund_summarized %>% 
   mutate(slopepos = factor(slopepos, levels = c("footslope", "low backslope", "backslope"))) %>% 
    ggplot(aes(x = slopepos, y = relabundance))+
    geom_bar(aes(fill = Class), stat = "identity")+
    facet_wrap(cover_type ~ .)+
    labs(x = "", 
         y = "Relative Abundance, %")+
    scale_fill_manual(values = (pnw_palette("Sailboat",4)))+
    #geom_text(data = label, aes(x = Trtmt, y = y, label = label), size = 8, color = "white")+
    theme_er()+
    coord_flip()+
    theme(legend.position = 'bottom', panel.border = element_rect(color="white",size=0.5, fill = NA))+
    NULL

ggsave("output/relabund_graph.tiff", plot = relabund_graph, height = 4, width = 8)
ggsave("output/relabund_graph.jpeg", plot = relabund_graph, height = 4, width = 8)

ggsave("output/relabund_slopepos_figure.tiff", plot = relabund_slopepos_figure, height = 4, width = 8)
ggsave("output/relabund_slopepos_figure.jpeg", plot = relabund_slopepos_figure, height = 4, width = 8)




##########################

# Site Position 


# 4. relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

relabund_table_covertype = 
  fticr_water_relabund_summarized %>% 
  #filter(cover_type == "Open") %>% 
  mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  dplyr::select(-relabundance, -se)

## step 2: create ANOVA function for relabund ~ cover_type, for each combination of cover_type-slopepos-Class

fit_aov_open = function(dat){
  
  aov(relabund ~ cover_type, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "cover_type") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    mutate(cover_type = "Open") %>% 
    force()
  
}


# relabund ~ slopepos

fit_hsd = function(dat){
  a = aov(relabund ~ slopepos, data = dat)
  h = HSD.test(a, "slopepos")
  h$groups %>% mutate(slopepos = row.names(.)) %>% 
    rename(label = groups) %>%  
    dplyr::select(slopepos, label)
}


## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

#covertype now working

relabund_aov_covertype = 
  fticr_water_relabund %>% 
  #filter(cover_type == "Open") %>% 
  group_by(slopepos, Class) %>% 
  do(fit_aov_open(.))


#slopepos 

relabund_hsd_slopepos = 
  fticr_water_relabund %>% 
  #filter(cover_type == "Open") %>% 
  group_by(cover_type, Class) %>% 
  do(fit_hsd(.))

###quick aov and tukey for slope position only



slope_lig = aov(relabund ~ slopepos, data = fticr_water_relabund %>% 
             filter(Class == 'unsaturated/lignin'))
summary(slope_lig)
print(slope_lig)
anova(slope_lig)
slope_lig_hsd <- HSD.test(slope_lig, "slopepos")
print(slope_lig_hsd)

slope_aliph = aov(relabund ~ slopepos, data = fticr_water_relabund %>% 
                  filter(Class == 'aliphatic'))
summary(slope_aliph)
print(slope_aliph)
anova(slope_aliph)
slope_aliph_hsd <- HSD.test(slope_aliph, "slopepos")
print(slope_aliph_hsd)


slope_aro = aov(relabund ~ slopepos, data = fticr_water_relabund %>% 
                    filter(Class == 'aromatic'))
summary(slope_aro)
print(slope_aro)
anova(slope_aro)
slope_aro_hsd <- HSD.test(slope_aro, "slopepos")
print(slope_aro_hsd)

slope_caro = aov(relabund ~ slopepos, data = fticr_water_relabund %>% 
                  filter(Class == 'condensed aromatic'))
summary(slope_caro)
print(slope_caro)
anova(slope_caro)
slope_caro_hsd <- HSD.test(slope_caro, "slopepos")
print(slope_caro_hsd)

#################


## step 4: combine the summarized values with the asterisks

#Not working, issue with asterisk as a character, won't paste.
relabund_table_with_aov_covertype = 
  relabund_table_covertype %>% 
  left_join(relabund_aov_covertype) %>%
  # combine the values with the label notation
  mutate(value = paste(summary, asterisk),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
  ) %>% 
  dplyr::select(-summary, -asterisk) %>% 
  pivot_wider(names_from = "slopepos", values_from = "value") %>% 
  force() %>% 
  rename("low_backslope" = "low backslope") %>% 
  mutate(backslope = str_remove(backslope, " NA")) %>% 
  mutate(low_backslope = str_remove(low_backslope, " NA")) %>% 
  mutate(footslope = str_remove(footslope, " NA"))  
  


relabund_table_with_aov_covertype %>% knitr::kable() # prints a somewhat clean table in the console


write.csv(relabund_table_with_aov_covertype, "output/covertype_aovstats.csv", row.names = FALSE)


# working

relabund_table_with_hsd_slopepos = 
  relabund_table_covertype %>% 
  left_join(relabund_hsd_slopepos) %>%
  # combine the values with the label notation
  mutate(value = paste(summary, label),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
         ) %>% 
  dplyr::select(-summary, -label) %>% 
  pivot_wider(names_from = "slopepos", values_from = "value") %>% 
  force()

relabund_table_with_hsd_slopepos %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(relabund_table_with_hsd_slopepos, "output/slopepos_hsdstats.csv", row.names = FALSE)



#################################

