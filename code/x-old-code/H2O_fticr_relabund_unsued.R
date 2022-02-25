#Relative abundance
#not used
# 2 25 2022

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


#############################



#stats


# 4. relabund summary table ----------------------------------------------------------------
## step 1: prepare the data, combine mean +/- se
## unicode "\u00b1" gives plus-minus symbol


relabund_table = 
  fticr_water_relabund_summarized %>% 
  mutate(summary = paste(relabundance, "\u00b1", se)) %>% 
  dplyr::select(-relabundance, -se)

## step 2: create ANOVA function for relabund ~ Trtmt, for each combination of Site-Material-Class


fit_aov_notworking = function(dat){
  
  aov(relabund ~ cover_type, data = dat) %>% 
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
  do(fit_aov_notworking(.))

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


#####


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

