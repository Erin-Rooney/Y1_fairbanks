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
  mutate(quartz_stdev = stringi::stri_replace_all_fixed(quartz_stdev, "ñ",""),
         albite_stdev = stringi::stri_replace_all_fixed(albite_stdev, "ñ",""),
         anorthite_stdev = stringi::stri_replace_all_fixed(anorthite_stdev, "ñ",""),
         microcline_stdev = stringi::stri_replace_all_fixed(microcline_stdev, "ñ",""),
         chlorite_stdev = stringi::stri_replace_all_fixed(chlorite_stdev, "ñ",""),
         mica_stdev = stringi::stri_replace_all_fixed(mica_stdev, "ñ",""),
         hornblend_stdev = stringi::stri_replace_all_fixed(hornblend_stdev, "ñ",""),
         ankerite_stdev = stringi::stri_replace_all_fixed(ankerite_stdev, "ñ","")) %>% 
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
                rwp = as.numeric(rwp),
                feldspar_decimal = as.numeric(feldspar_decimal),
                ) %>% 
  mutate(feldspar = (feldspar_decimal * 100)) %>% 
  select(-c(quartz_stdev, albite_stdev, anorthite_stdev, microcline_stdev,
            chlorite_stdev, mica_stdev, hornblende_stdev, ankerite_stdev, feldspar_decimal)) %>% 
  pivot_longer(cols = c(quartz, albite, anorthite, microcline, chlorite, mica, hornblende,
                        ankerite, rwp, feldspar), names_to = "mineral", values_to = "abundance") %>% 
  #group_by(slopepos, covertype, morph) %>% 
  group_by(slopepos, covertype, mineral) %>% 
  dplyr::summarize(mean = round(mean(abundance), 3),
                   se = round(sd(abundance)/sqrt(n()),3)) %>% 
  mutate(summary = paste(mean, "\u00b1", se)) %>% 
  na.omit()
  #dplyr::select(-mean, -se)

xrd_slope = 
  xrd_data_tableanalysis %>% 
  mutate(slopepos = recode(slopepos, "low_backslope" = 'low backslope')) %>% 
  mutate(slopepos = factor(slopepos, levels = c("backslope", "low backslope", "footslope"))) %>%
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
  xrd_data_tableanalysis %>% 
  mutate(slopepos = recode(slopepos, "low_backslope" = 'low backslope')) %>% 
  mutate(slopepos = factor(slopepos, levels = c("backslope", "low backslope", "footslope"))) %>%
  ggplot(aes(x = mineral, y = mean, fill = slopepos))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin=(mean-se/2),ymax=(mean+se/2)),width=.2,position=position_dodge(.9))+
  coord_flip() +
  labs(y = "abundance",
       x = "")+
  scale_fill_manual(values = (PNWColors::pnw_palette("Shuksan", 3)))+
  theme_er()+
  theme(legend.position = "bottom", panel.border = element_rect(color="white",size=0.5, fill = NA)
  )+
  facet_grid(.~covertype)+
  NULL

ggsave("output/xrd_cover.tiff", plot = xrd_cover, height = 6, width = 10)
ggsave("output/xrd_cover.jpeg", plot = xrd_cover, height = 6, width = 10)
ggsave("output/xrd_slope.tiff", plot = xrd_slope, height = 6, width = 10)
ggsave("output/xrd_slope.jpeg", plot = xrd_slope, height = 6, width = 10)

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
  
  
