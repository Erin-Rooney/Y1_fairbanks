# 3-21-2022
# FTC data
# Fairbanks Y1
# Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(plyr)
library(Rmisc)
library(devtools)
#install_github("BajczA475/FTCQuant/FTCQuant")
library(FTCQuant)
library(data.table)
library(neonUtilities)
library(scales)

# 1. Load files-----------------------------------

#replaced ID with plot in line 12, moved plot to beginning (it was already at the end of line 12)

fairbanks_ftc = read.csv("rawdata/fairbanks_open_vs_tree.csv") 

fairbanks_ftc_open = 
  fairbanks_ftc %>% 
  select(open_date_time, open_tempC_0.05cm, open_tempC_0.75cm) %>% 
  dplyr::mutate(cover_type = 'open') %>% 
  rename(date_time = open_date_time) %>% 
  rename(tempC_0.05 = open_tempC_0.05cm) %>% 
  rename(tempC_0.75 = open_tempC_0.75cm)

fairbanks_ftc_closed = 
  fairbanks_ftc %>% 
  select(closed_date_time, closed_tempC_0.05cm, closed_tempC_0.75cm) %>% 
  dplyr::mutate(cover_type = 'closed') %>% 
  rename(date_time = closed_date_time) %>% 
  rename(tempC_0.05 = closed_tempC_0.05cm) %>% 
  rename(tempC_0.75 = closed_tempC_0.75cm)

fairbanks_ftc_combined = 
  fairbanks_ftc_open %>% 
  bind_rows(fairbanks_ftc_closed) %>% 
  na.omit() %>% 
  separate(date_time, sep = " ", into = c("date", "time")) 

  #fairbanks_ftc_combined$date <- as.Date(fairbanks_ftc_combined$date)

fairbanks_ftc_combined %>% 
  ggplot(aes(x = as.Date(date_time), y = tempC_0.75))+
  geom_line(group = "cover_type")+
  facet_grid(cover_type ~.)


  geom_segment(x = 0.0, y = 0.0, xend = '9/9/2021 18:00', yend = 0.0,
               color = "red", linetype = "longdash")+
  theme(axis.text.x = element_text (vjust = 0.5, hjust=1, angle = 90))



# scale_x_datetime( breaks=("1 month"), 
#                   minor_breaks=("14 days"), 
#                   labels=date_format("%m/%y"), 
#                   timezone = "UTC-9:00") + 





