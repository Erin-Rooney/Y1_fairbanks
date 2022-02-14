#Total CN data
#Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

# 1. Load files-----------------------------------

cn_data = read.csv("processed/Y1_CNperc.csv") 

# 2. Process data---------------------------------
# remove n and separate sample IDs into multiple columns
# grepl for canopy and slope columns
# LDC and LDA are typos from xrd analysis input, should be LOA and LOC, fixed with recode

cn_data_processed =
  cn_data %>% 
  separate(sample, sep = " ", into = c("site", "sample_num", "sample_id")) %>% 
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

cn_data_tableanalysis =
  cn_data_processed %>% 
  group_by(slopepos, covertype) %>% 
  dplyr::summarize(C_mean = round(mean(c_perc), 3),
                   C_se = round(sd(c_perc)/sqrt(n()),3),
                   N_mean = round(mean(n_perc), 3),
                   N_se = round(sd(n_perc)/sqrt(n()),3),
                   ) %>% 
  mutate(C_summary = paste(C_mean, "\u00b1", C_se),
         N_summary = paste(N_mean, "\u00b1", N_se)) %>% 
  na.omit() %>% 
  dplyr::select(-C_mean, -C_se, -N_mean, -N_se)


cn_data_tableanalysis %>% knitr::kable() # prints a somewhat clean table in the console

write.csv(cn_data_tableanalysis, "output/cn_data_tableanalysis.csv", row.names = FALSE)
