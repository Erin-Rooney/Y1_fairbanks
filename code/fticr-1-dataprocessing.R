# March-12-2021
# FTICR
# Alaska Y1 Fairbanks


# Load libraries-------------------------------
library(tidyverse)
library(reshape2)
library(soilpalettes)
library(PNWColors)
theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "right",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=2, fill = NA),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}


###################
###################
# process raw fticr data --------------------------------------------------
# Load data------------------------------------
report_water = read.csv("processed/Y1 H2O Report.csv")

# metadata--------------------
ID = read.csv("processed/y1_metadata.csv")

fticr_key = 
  ID %>% 
  separate(ID, sep = " ", into = c("site", "ID")) %>% 
  filter(morph != "Oi")



# core key for fticr ------------------------------------------------------

fticr_reps = 
  fticr_key %>% 
  # calculate reps per treatment grouping
  # select(-rep) %>% 
  group_by(slopepos, cover_type, rep) %>% 
  dplyr::mutate(reps = n()) %>% 
  # separate ID column into many
  #separate(ID, sep = " ", into = c("FT", "ID")) %>% 
  # keep only the necessary columns
  select(ID, slopepos, cover_type, mid_cm, reps) %>% 
  rename(plot = 'rep')

 fticr_key = 
   fticr_key %>% 
   rename(plot = 'rep') %>% 
   select(plot, ID, slopepos, cover_type, mid_cm) 
   
# Assemble reports WATER------------------------------
fticr_report_water = 
  report_water %>% 
  #rename(Mass=`ï..Mass`) %>% 
  # filter appropriate mass range
  filter(Mass>200 & Mass<900) %>% 
  # remove isotopes
  filter(C13==0) %>% 
  # remove peaks without C assignment
  filter(C>0)


fticr_meta_water = 
  fticr_report_water %>% 
  # select only the relevant columns for the formula assignments
  dplyr::select(Mass:Candidates) %>% 
  # alternatively, use `starts_with()` if all your sample names start with the same prefix
  # dplyr::select(-starts_with("FT")) %>% 
  # select only necessary columns
  dplyr::select(Mass, C, H, O, N, S, P, El_comp) %>% 
  # create columns for indices
  dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                HC = round(H/C,2),
                OC = round(O/C,2)) %>% 
  # create column/s for formula
  # first, create columns for individual elements
  # then, combine
  dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                formula = str_replace_all(formula,"NA","")) %>% 
  dplyr::select(Mass, formula, El_comp, HC, OC, AImod, NOSC, C:P) %>% 
  mutate(Class = case_when(AImod>0.66 ~ "condensed aromatic",
                           AImod<=0.66 & AImod > 0.50 ~ "aromatic",
                           AImod <= 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                           HC >= 1.5 ~ "aliphatic"),
         Class = replace_na(Class, "other"))


# subset of meta for HC/OC only, for Van Krevelen diagrams
meta_hcoc_water = 
  fticr_meta_water %>% 
  dplyr::select(Mass, formula, HC, OC, NOSC)

fticr_data_water = 
  fticr_report_water %>% 
  # select only the relevant columns for the formula assignments
  dplyr::select(-c(C:Candidates)) %>% 
  # alternatively, use `starts_with()` if all your sample names start with the same prefix
  # dplyr::select(Mass,starts_with("FT")) %>% 
  melt(id = c("Mass"), value.name = "presence", variable.name = "ID") %>%
  separate(ID, sep = "_", into = c("site_col", "ID", "W")) %>% 
  # convert intensities to presence==1/absence==0  
  dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
  # keep only peaks present
  filter(presence>0) %>% 
  left_join(dplyr::select(fticr_meta_water, Mass,formula), by = "Mass")  %>% 
  left_join(fticr_key, by = "ID") %>% 
  # rearrange columns
  dplyr::select(-Mass,-formula, -presence, Mass,formula,presence) %>% 
  # separate COREID for easy left_join
  #separate(ID, sep = "_", into = c("site_col", "ID", "W")) %>% 
  # filter only FT
  # filter(FT_col == "FT")
  #filter(FT_col %in% "FT") %>% 
  # left_join(fticr_reps, by = "field_ID") %>% 
  left_join(fticr_reps) %>% 
  rename(max_reps = reps) %>% 
  group_by(slopepos, cover_type, plot, formula) %>% 
  dplyr::mutate(formulareps = n()) %>% 
  # set up replication filter for 2/3 of max_rep
  ungroup() %>% 
  mutate(include = formulareps >= (2/3)*max_reps) %>% 
  
  ## mutate(include = formulareps > 1,
  ##        occurrence = case_when(formulareps == max_reps ~ "3/3",
  ##                               formulareps < max_reps & formulareps >= (2/3)*max_reps ~ "2/3+",
  ##                               formulareps >= (1/3)*max_reps ~ "1/3+",
  ##                               formulareps < (1/3)*max_reps ~ "exclude")) %>% 
  filter(include)




# now we want only peaks that are in 3 of the 5 replicates
# group by the treatment levels  
# group_by(treatment, sat_level,formula) %>% 
# dplyr::mutate(n = n(),
#              presence = mean(presence)) %>% 
# filter(n>2) 

meta_formula_water = 
  fticr_meta_water %>% 
  dplyr::select(Mass, formula) %>% 
  group_by(formula) %>% 
  dplyr::mutate(n = n())




# Write reports csv output----------------------------------

## OUTPUTS
write.csv(fticr_data_water,"fticr_data_water.csv", row.names = FALSE)
write.csv(fticr_meta_water,"fticr_meta_water.csv", row.names = FALSE)
write.csv(meta_hcoc_water,"fticr_meta_hcoc_water.csv", row.names = FALSE)


