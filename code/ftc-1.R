# 3-21-2022
# FTC data
# Fairbanks Y1
# Erin C Rooney

#load packages
source("code/FTICR-0-packages.R")

library(lubridate)

#change source file 0.75 cm to 75 cm & 0.05 cm to 5 cm


# 1. Load files-----------------------------------

#replaced ID with plot in line 12, moved plot to beginning (it was already at the end of line 12)

fairbanks_ftc = read.csv("rawdata/fairbanks_open_vs_tree.csv") 

fairbanks_ftc_open = 
  fairbanks_ftc %>% 
  select(starts_with('open')) %>% 
  dplyr::rename(date_time = open_date_time) %>% 
  dplyr::rename(tempC_5 = open_tempC_5cm) %>% 
  dplyr::rename(tempC_75 = open_tempC_75cm) %>% 
  pivot_longer(-c(date_time), names_to = "depth", values_to = "temperature_C") %>% 
  mutate(depth = str_remove(depth, "tempC_")) %>% 
  dplyr::mutate(cover_type = 'open')   
  

fairbanks_ftc_closed = 
  fairbanks_ftc %>% 
  select(starts_with('closed')) %>% 
  rename(date_time = closed_date_time) %>% 
  rename(tempC_5 = closed_tempC_5cm) %>% 
  rename(tempC_75 = closed_tempC_75cm) %>% 
  pivot_longer(-c(date_time), names_to = "depth", values_to = "temperature_C") %>% 
  mutate(depth = str_remove(depth, "tempC_")) %>% 
  dplyr::mutate(cover_type = 'closed') 
  

fairbanks_ftc_combined = 
  fairbanks_ftc_open %>% 
  bind_rows(fairbanks_ftc_closed) %>%
  na.omit() %>% 
  mutate(date_time = mdy_hm(date_time)) %>% 
  pivot_wider(names_from = 'cover_type', values_from = "temperature_C") %>% 
  pivot_longer(-c(date_time, depth), names_to = 'cover_type', values_to = 'temperature_C')

  #fairbanks_ftc_combined$date <- as.Date(fairbanks_ftc_combined$date)

library(scales)


fairbanks_temperature = 
  fairbanks_ftc_combined %>% 
  ggplot(aes(x = date_time, y = temperature_C, color = cover_type))+
  geom_line()+
  labs(x = "", 
       y = "temperature, celsius")+
  geom_hline(yintercept = 0, color = "black", linetype = "longdash")+
  scale_x_datetime( breaks=("3 months"), 
                    labels=date_format("%b-%y"), 
                    timezone = "UTC-9:00") + 
  #scale_color_manual(values = PNWColors::pnw_palette("Bay", 2))+
  scale_color_manual(values = c('#006d77', '#e29578'))+
  facet_grid(depth ~., scales = "free_y", 
             labeller = as_labeller(c('5' = "5 cm", '75' = "75 cm"))) +
  theme_er()+
  theme(axis.text.x = element_text (vjust = 0.5, angle = 45))
  
ggsave("output/fairbanks_temperature.tiff", plot = fairbanks_temperature, height = 6.6, width = 10)







