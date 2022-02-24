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
mutate(slopepos = factor(slopepos, levels = c("Backslope", "Low Backslope", "Footslope"))) %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "closed"),
         cover_type = recode(cover_type, "Open" = "open")) 
  



#2b. relabund summary by treatment ---


fticr_water_relabund_summarized = 
  fticr_water_relabund %>% 
  mutate(Class = factor(Class, levels = c("aliphatic", "unsaturated/lignin", "aromatic", "condensed aromatic"))) %>% 
  group_by(slopepos, cover_type, plot, Class) %>% 
  #group_by(ID, Class) %>% 
  dplyr::summarise(relabundance = round(mean(relabund), 2),
                   se = round(sd(relabund)/sqrt(n()),2))


#2c. aromatic relabund --- do this later.


## plot relabund of aromatic

# fticr_water_relabund_arom %>% 
#   filter(aromatic_col %in% "aromatic") %>% 
#   ggplot(aes(x = slopepos, y = relabund, color = slopepos, shape = slopepos))+
#   #geom_boxplot()+
#   geom_point()+
#   facet_grid(cover_type ~ .)+
#   theme_er()

# fticr_water_relabund_arom %>% 
#   #mutate(Material = factor (Material, levels = c("Organic", "Upper Mineral", "Lower Mineral"))) %>% 
#   filter(aromatic_col %in% "aromatic") %>% 
#   ggplot(aes(x = slopepos, y = relabund, color = slopepos, shape = slopepos, size = 4))+
#   #geom_boxplot()+
#   geom_point(position = position_dodge(width = 0.3))+
#   facet_grid(cover_type ~ .)+
#   scale_color_manual(values = rev(PNWColors::pnw_palette("Lake", 3)))+
#   theme_er()
## potential idea: do ANOVA with x = site, and report p-values in the graph
## then do ANOVA with x = trtmt for each site, and report sig. as asterisks

# stats for aromatic peaks





# relative abundance ------------------------------------------------------
# fticr_data_water_summarized = 
#   fticr_data_water %>% 
#   group_by(formula, slopepos, cover_type, plot) %>% 
#   dplyr::summarise(n = n()) %>% 
#   mutate(presence = 1) %>% 
#   dplyr::select(-n)

## NOTE: calculate relative abundance PER SAMPLE and then combine by treatment
# fticr_water_relabund = 
#   fticr_data_water %>% 
#   left_join(select(fticr_meta_water, formula, Class), by = "formula") %>% 
#   ## create a column for group counts
#   group_by(slopepos, cover_type, plot, Class) %>% 
#   dplyr::summarize(counts = n()) %>% 
#   ## create a column for total counts
#   group_by(slopepos, cover_type, plot,) %>%
#   dplyr::mutate(totalcounts = sum(counts)) %>% 
#   ungroup() %>% 
#   mutate(relabund = (counts/totalcounts)*100,
#          relabund = round(relabund, 2)) %>% 
#   mutate(slopepos = factor(slopepos, levels = c("Backslope", "Low Backslope", "Footslope")))

#mutate(Material = factor(Material, levels = c("Organic", "Upper Mineral", "Lower Mineral")))
## use this file (relabund by sample) for PCA and PERMANOVA

## but summarize for bar plots
# fticr_water_relabund_summarized = 
#   fticr_water_relabund %>% 
#   group_by(slopepos, cover_type, plot, Class) %>% 
#   dplyr::summarise(relabund = mean(relabund))



# run anova to get statistical significance
# then use that to create the label file below


# bar graph
#relabund_graph = 
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
    mutate(slopepos = factor(slopepos, levels = c("Footslope", "Low Backslope", "Backslope"))) %>% 
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

ggsave("output/relabund_graph.tiff", plot = relabund_graph, height = 6, width = 10)
ggsave("output/relabund_graph.jpeg", plot = relabund_graph, height = 6, width = 10)

ggsave("output/relabund_slopepos_figure.tiff", plot = relabund_slopepos_figure, height = 6, width = 10)
ggsave("output/relabund_slopepos_figure.jpeg", plot = relabund_slopepos_figure, height = 6, width = 10)


# 4. relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

relabund_table = 
  fticr_water_relabund_summarized %>% 
  mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  dplyr::select(-relabundance, -se)

## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class


fit_aov = function(dat){
  
  aov(relabund ~ 'cover_type', data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "cover_type") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set cover_type = "Open" so the asterisks will be added to the FTC values only
    mutate(cover_type = "Open") %>% 
    NULL
  
}


## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

relabund_asterisk = 
  fticr_water_relabund %>% 
  group_by(slopepos, Class) %>% 
  do(fit_aov(.))

## step 4: combine the summarized values with the asterisks
relabund_table_with_asterisk = 
  relabund_table %>% 
  left_join(relabund_asterisk) %>%
  # combine the values with the asterisk notation
  mutate(value = paste(summary, asterisk),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         value = str_remove(value, " NA")) %>% 
  dplyr::select(-summary, -asterisk) %>% 
  pivot_wider(names_from = "cover_type", values_from = "value")

relabund_table_with_asterisk %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(relabund_table_with_asterisk, "output/canopy_aovstats.csv", row.names = FALSE)

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

#covertype 

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


#################


## step 4: combine the summarized values with the asterisks

#Not working, issue with asterisk as a character, won't paste.
relabund_table_with_aov_covertype = 
  relabund_aov_covertype %>% 
  left_join(relabund_hsd_covertype) %>%
  # combine the values with the label notation
  mutate(value = paste(summary, asterisk),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
  ) %>% 
  dplyr::select(-summary, -asterisk) %>% 
  pivot_wider(names_from = "slopepos", values_from = "value") %>% 
  force()

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

write.csv(relabund_table_with_hsd_covertype, "output/slopepos_hsdstats.csv", row.names = FALSE)



#################################

# Site Position (CANOPY ONLY)


# 4. relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol

# relabund_table_canopy = 
#   fticr_water_relabund_summarized %>% 
#   filter(cover_type == "Canopy") %>% 
#   mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
#   dplyr::select(-relabundance, -se)
# 
# ## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class
# 
# fit_aov_canopy = function(dat){
#   
#   aov(relabund ~ slopepos, data = dat) %>% 
#     broom::tidy() %>% # convert to clean dataframe
#     rename(pvalue = `p.value`) %>% 
#     filter(term == "slopepos") %>% 
#     mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
#     dplyr::select(asterisk) %>% # we need only the asterisk column
#     # two steps below, we need to left-join. 
#     # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
#     mutate(cover_type = "Canopy")   
  
# }


## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

# relabund_asterisk_canopy = 
#   fticr_water_relabund %>% 
#   filter(cover_type == "Canopy") %>% 
#   group_by(Class) %>% 
#   do(fit_aov_canopy(.))
# 
# ## step 4: combine the summarized values with the asterisks
# relabund_table_with_asterisk_canopy = 
#   relabund_table_canopy %>% 
#   left_join(relabund_asterisk_canopy) %>%
#   # combine the values with the asterisk notation
#   mutate(value = paste(summary, asterisk),
#          # this will also add " NA" for the blank cells
#          # use str_remove to remove the string
#          value = str_remove(value, " NA")) %>% 
#   dplyr::select(-summary, -asterisk) %>% 
#   pivot_wider(names_from = "slopepos", values_from = "value")
# 
# relabund_table_with_asterisk_canopy %>% knitr::kable() # prints a somewhat clean table in the console
# 
# write.csv(relabund_table_with_asterisk_canopy, "output/slopeposcanopy_aovstats.csv", row.names = FALSE)
