#xrd data
#Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

xrd_data = read.csv("processed/xrd_data.csv") 

# 2. Process data---------------------------------
# remove n and separate sample IDs into multiple columns
# grepl for canopy and slope columns
# LDC and LDA are typos from xrd analysis input, should be LOA and LOC, fixed with recode

xrd_data_processed =
  xrd_data %>% 
  select(-c(quartz_stdev, albite_stdev, anorthite_stdev, microcline_stdev, chlorite_stdev, 
            mica_stdev, hornblend_stdev, ankerite_stdev, feldspar_decimal, rwp)) %>% 
  # mutate(quartz_stdev = stringi::stri_replace_all_fixed(quartz_stdev, "ñ",""),
  #        albite_stdev = stringi::stri_replace_all_fixed(albite_stdev, "ñ",""),
  #        anorthite_stdev = stringi::stri_replace_all_fixed(anorthite_stdev, "ñ",""),
  #        microcline_stdev = stringi::stri_replace_all_fixed(microcline_stdev, "ñ",""),
  #        chlorite_stdev = stringi::stri_replace_all_fixed(chlorite_stdev, "ñ",""),
  #        mica_stdev = stringi::stri_replace_all_fixed(mica_stdev, "ñ",""),
  #        hornblend_stdev = stringi::stri_replace_all_fixed(hornblend_stdev, "ñ",""),
  #        ankerite_stdev = stringi::stri_replace_all_fixed(ankerite_stdev, "ñ","")) %>% 
  # rename(hornblende_stdev = hornblend_stdev) %>%
  separate(sample, sep = " ", into = c("sample_num", "sample_id")) %>% 
  separate(sample_id, sep = "-", into = c("canopy_slope", "morph")) %>%
  mutate(canopy_slope = recode(canopy_slope, 'LDC' = 'LOC',
                               'LDA' = "LOA")) %>% 
  dplyr::mutate(slopepos = case_when(grepl("F", canopy_slope)~"footslope",
                                 grepl("L", canopy_slope)~"low_backslope",
                                 grepl("B", canopy_slope)~"backslope"),
                covertype = case_when(grepl("O", canopy_slope)~"open",
                                      grepl("o", canopy_slope)~"open",
                                     grepl("CC", canopy_slope)~"canopy",
                                     grepl("C", canopy_slope)~"canopy")) %>% 
  select(-c(canopy_slope))
  
# 3. Analyse data and figures

xrd_data_tableanalysis =
  xrd_data_processed %>% 
  dplyr::mutate(quartz = as.numeric(quartz),
                albite = as.numeric(albite),
                anorthite = as.numeric(anorthite),
                microcline = as.numeric(microcline),
                chlorite = as.numeric(chlorite),
                mica = as.numeric(mica),
                hornblende = as.numeric(hornblende),
                ankerite = as.numeric(ankerite),
                #rwp = as.numeric(rwp),
                #feldspar_decimal = as.numeric(feldspar_decimal),
                ) %>% 
  #mutate(feldspar = (feldspar_decimal * 100)) %>% 
  # select(-c(quartz_stdev, albite_stdev, anorthite_stdev, microcline_stdev,
  #           chlorite_stdev, mica_stdev, hornblende_stdev, ankerite_stdev)) %>% 
  pivot_longer(cols = c(quartz, albite, anorthite, microcline, chlorite, mica, hornblende,
                        ankerite), names_to = "mineral", values_to = "abundance") %>% 
  #group_by(slopepos, covertype, morph) %>% 
  group_by(slopepos, covertype, mineral) %>% 
  dplyr::summarize(mean = round(mean(abundance), 3),
                   se = round(sd(abundance)/sqrt(n()),3)) %>% 
  mutate(summary = paste(mean, "\u00b1", se)) %>% 
  na.omit()
  #dplyr::select(-mean, -se)


xrd_stats = 
  xrd_data_processed %>% 
  dplyr::mutate(quartz = as.numeric(quartz),
                albite = as.numeric(albite),
                anorthite = as.numeric(anorthite),
                microcline = as.numeric(microcline),
                chlorite = as.numeric(chlorite),
                mica = as.numeric(mica),
                hornblende = as.numeric(hornblende),
                ankerite = as.numeric(ankerite),
                # rwp = as.numeric(rwp),
                # feldspar_decimal = as.numeric(feldspar_decimal),
  ) %>% 
  #mutate(feldspar = (feldspar_decimal * 100)) %>% 
  # select(-c(quartz_stdev, albite_stdev, anorthite_stdev, microcline_stdev,
  #           chlorite_stdev, mica_stdev, hornblende_stdev, ankerite_stdev)) %>% 
  pivot_longer(cols = c(quartz, albite, anorthite, microcline, chlorite, mica, hornblende,
                        ankerite), names_to = "mineral", values_to = "abundance") %>% 
  group_by(slopepos, covertype, mineral) %>% 
  dplyr::summarize(mean = round(mean(abundance), 3),
                   se = round(sd(abundance)/sqrt(n()),3)) %>% 
  na.omit() %>% 
  mutate(covertype = recode(covertype, "canopy" = "closed"))



xrd_slope = 
  xrd_stats %>% 
  mutate(slopepos = recode(slopepos, "low_backslope" = 'low backslope')) %>% 
  mutate(slopepos = factor(slopepos, levels = c("backslope", "low backslope", "footslope"))) 

xrd_slope %>% 
  ggplot(aes(x = mineral, y = mean, fill = covertype))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin=(mean-se/2),ymax=(mean+se/2)),width=.2,position=position_dodge(.9))+
  coord_flip() +
  labs(y = "abundance",
       x = "")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
    theme_er()+
    theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA)
    )+
    facet_grid(.~slopepos)+
    NULL


xrd_cover =
  xrd_stats %>% 
  mutate(slopepos = recode(slopepos, "low_backslope" = 'low backslope')) %>% 
  mutate(slopepos = factor(slopepos, levels = c("backslope", "low backslope", "footslope"))) %>%
  ggplot(aes(x = mineral, y = mean, fill = slopepos))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin=(mean-se/2),ymax=(mean+se/2)),width=.2,position=position_dodge(.9))+
  coord_flip() +
  labs(y = "abundance",
       x = "")+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Bay", 3)))+
  theme_er()+
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA)
  )+
  facet_grid(.~covertype)+
  NULL

ggsave("output/xrd_cover.tiff", plot = xrd_cover, height = 6, width = 10)
ggsave("output/xrd_cover.jpeg", plot = xrd_cover, height = 6, width = 10)
#ggsave("output/xrd_slope.tiff", plot = xrd_slope, height = 6, width = 10)
#ggsave("output/xrd_slope.jpeg", plot = xrd_slope, height = 6, width = 10)

# xrd_data_processed %>% 
#   ggplot(aes(y = quartz, x = covertype, fill = covertype))+
#   geom_col(horizontal = TRUE, width = 0.7)+
#   scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 3)))+
#   theme_er()+
#   theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA) 
#   )+
#   coord_flip() +
#   facet_grid(.~slopepos)+
#   NULL


# xrd_data_processed %>% 
#     ggplot(aes(y = quartz, x = covertype, fill = slopepos))+
#     geom_col(width = 0.7)+
#     scale_fill_manual(values = (PNWColors::pnw_palette("Bay", 3)))+
#     theme_er()+
#     theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA)
#     )+
#     NULL
  
# 5. XRD stats ------------------------
#Changed canopy to closed on 2022 1 25, may affect code downstream

library(nlme)


xrd_interaction = lme(abundance ~ slopepos*covertype, random = ~1|mineral, na.action = na.omit, data = xrd_stats)
                      
summary(xrd_interaction)
print(xrd_interaction)
anova(xrd_interaction)

#slope pos comparison

fit_aov = function(dat){
  
  aov(abundance ~ slopepos, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "slopepos") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    #mutate(cover_type = "Open")   
    force()
  
}

fit_hsd = function(dat){
  a = aov(abundance ~ slopepos, data = dat)
  h = HSD.test(a, "slopepos")
  h$groups %>% mutate(slopepos = row.names(.)) %>% 
    rename(label = groups) %>%  
    dplyr::select(slopepos, label)
}

## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

xrd_hsd_all = 
  xrd_stats %>% 
  #filter(cover_type == "Open") %>% 
  group_by(covertype, mineral) %>% 
  do(fit_hsd(.))

xrd_table_with_hsd_covertype = 
  xrd_data_tableanalysis %>% 
  left_join(xrd_hsd_all) %>%
  # combine the values with the label notation
  mutate(value = paste(summary, label),
         # this will also add " NA" for the blank cells
         # use str_remove to remove the string
         #value = str_remove(value, " NA")
  ) %>% 
  dplyr::select(-summary, -label) %>% 
  pivot_wider(names_from = "slopepos", values_from = "value") %>% 
  force()

xrd_table_with_hsd_covertype %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(xrd_table_with_hsd_covertype, "output/xrd_table_with_hsd_covertype.csv", row.names = FALSE)

xrd_hsd_all %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(xrd_hsd_all, "output/xrd_hsd_all.csv", row.names = FALSE)

######################



# cover type comparison

fit_aov = function(dat){
  
  aov(abundance ~ covertype, data = dat) %>% 
    broom::tidy() %>% # convert to clean dataframe
    rename(pvalue = `p.value`) %>% 
    filter(term == "covertype") %>% 
    mutate(asterisk = case_when(pvalue <= 0.05 ~ "*")) %>% 
    dplyr::select(asterisk) %>% # we need only the asterisk column
    # two steps below, we need to left-join. 
    # set Trtmt = "FTC" so the asterisks will be added to the FTC values only
    #mutate(cover_type = "Open")   
    force()
  
}

fit_hsd = function(dat){
  a = aov(abundance ~ covertype, data = dat)
  h = HSD.test(a, "covertype")
  h$groups %>% mutate(covertype = row.names(.)) %>% 
    rename(label = groups) %>%  
    dplyr::select(covertype, label)
}

## step 3: run the fit_anova function 
## do this on the original relabund file, because we need all the reps

xrd_hsd_covertype = 
  xrd_stats %>% 
  #filter(cover_type == "Open") %>% 
  group_by(slopepos, mineral) %>% 
  do(fit_hsd(.))

# xrd_table_with_hsd_covertype = 
#   xrd_data_tableanalysis %>% 
#   left_join(xrd_hsd_all) %>%
#   # combine the values with the label notation
#   mutate(value = paste(summary, label),
#          # this will also add " NA" for the blank cells
#          # use str_remove to remove the string
#          #value = str_remove(value, " NA")
#   ) %>% 
#   dplyr::select(-summary, -label) %>% 
#   pivot_wider(names_from = "slopepos", values_from = "value") %>% 
#   force()

# xrd_table_with_hsd_covertype %>% knitr::kable() # prints a somewhat clean table in the console
# 
# write.csv(xrd_table_with_hsd_covertype, "output/xrd_table_with_hsd_covertype.csv", row.names = FALSE)

xrd_hsd_covertype %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(xrd_hsd_covertype, "output/xrd_hsd_covertype.csv", row.names = FALSE)



################## Sample and sample error table ----------------

xrd_data_sample =
  xrd_data %>% 
  # select(-c(quartz_stdev, albite_stdev, anorthite_stdev, microcline_stdev, chlorite_stdev, 
  #           mica_stdev, hornblend_stdev, ankerite_stdev, feldspar_decimal, rwp)) %>% 
  # mutate(quartz_stdev = stringi::stri_replace_all_fixed(quartz_stdev, "ñ",""),
  #        albite_stdev = stringi::stri_replace_all_fixed(albite_stdev, "ñ",""),
  #        anorthite_stdev = stringi::stri_replace_all_fixed(anorthite_stdev, "ñ",""),
  #        microcline_stdev = stringi::stri_replace_all_fixed(microcline_stdev, "ñ",""),
  #        chlorite_stdev = stringi::stri_replace_all_fixed(chlorite_stdev, "ñ",""),
  #        mica_stdev = stringi::stri_replace_all_fixed(mica_stdev, "ñ",""),
  #        hornblend_stdev = stringi::stri_replace_all_fixed(hornblend_stdev, "ñ",""),
  #        ankerite_stdev = stringi::stri_replace_all_fixed(ankerite_stdev, "ñ","")) %>% 
  rename(hornblende_stdev = hornblend_stdev) %>%
  separate(sample, sep = " ", into = c("sample_num", "sample_id")) %>% 
  separate(sample_id, sep = "-", into = c("canopy_slope", "morph")) %>%
  mutate(canopy_slope = recode(canopy_slope, 'LDC' = 'LOC',
                               'LDA' = "LOA")) %>% 
  dplyr::mutate(slopepos = case_when(grepl("F", canopy_slope)~"footslope",
                                     grepl("L", canopy_slope)~"low_backslope",
                                     grepl("B", canopy_slope)~"backslope"),
                covertype = case_when(grepl("O", canopy_slope)~"open",
                                      grepl("o", canopy_slope)~"open",
                                      grepl("CC", canopy_slope)~"canopy",
                                      grepl("C", canopy_slope)~"canopy")) %>% 
  select(-c(canopy_slope)) %>% 
  mutate(quartz_sample = paste(quartz, "\u00b1", quartz_stdev),
         albite_sample = paste(albite, "\u00b1", albite_stdev),
         anorthite_sample = paste(anorthite, "\u00b1", anorthite_stdev),
         microcline_sample = paste(microcline, "\u00b1", microcline_stdev),
         chlorite_sample = paste(chlorite, "\u00b1", chlorite_stdev),
         mica_sample = paste(mica, "\u00b1", mica_stdev),
         hornblende_sample = paste(hornblende, "\u00b1", hornblende_stdev),
         ankerite_sample = paste(ankerite, "\u00b1", ankerite_stdev
                          )) 

xrd_sample =
  xrd_data_sample %>% 
  select(-c(quartz_stdev, albite_stdev, anorthite_stdev, microcline_stdev, chlorite_stdev, 
                      mica_stdev, hornblende_stdev, ankerite_stdev, quartz, albite, anorthite, 
            microcline, chlorite, mica, hornblende, ankerite, sample_num)) %>% 
  mutate(feldspar = (feldspar_decimal * 100)) %>% 
  rename(quartz = quartz_sample,
         albite = albite_sample,
         anorthite = anorthite_sample,
         microcline = microcline_sample,
         chlorite = chlorite_sample, 
         mica = mica_sample,
         hornblende = hornblende_sample,
         ankerite = ankerite_sample) %>% 
  na.omit()
         

xrd_sample %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(xrd_sample, "output/xrd_sample.csv", row.names = FALSE)

         