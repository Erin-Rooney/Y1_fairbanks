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
  pivot_longer(-c(date_time, depth), names_to = 'cover_type', values_to = 'temperature_C') %>% 
  filter(date_time > "2020-06-01 00:00:00") %>% 
  mutate(months_go = date_time) %>% 
  separate(months_go, sep = " ", into = c("date", "time")) %>% 
  separate(date, sep = "-", into = c("year", "month", "day")) %>% 
  mutate(month = recode(month, "01" = "january", "02" = "february", 
                        "03" = "march", "04" = "april", "05" = "may", 
                        "06" = "june", "07" = "july", "08" = "august",
                        "09" = "september", "10" = "october", "11" = "november",
                        "12" = "december")) %>% 
  dplyr::mutate(season_go = case_when(grepl("june", month)~"summer",
                                      grepl("july", month)~"summer",
                                      grepl("august", month)~"summer",
                                      grepl("september", month)~"fall",
                                      grepl("october", month)~"fall",
                                      grepl("november", month)~"fall",
                                      grepl("december", month)~"winter",
                                      grepl("january", month)~"winter",
                                      grepl("february", month)~"winter",
                                      grepl("march", month)~"spring",
                                      grepl("april", month)~"spring",
                                      grepl("may", month)~"spring"))
                                      
                                       
                                       
                                       
summary =
  fairbanks_ftc_combined %>% 
  group_by(depth, cover_type, season_go) %>% 
  na.omit() %>% 
  dplyr::summarise(max = max(temperature_C),
                   min = min(temperature_C),
                   mean = mean(temperature_C)) 

  #fairbanks_ftc_combined$date <- as.Date(fairbanks_ftc_combined$date)

library(scales)


fairbanks_temperature = 
  fairbanks_ftc_combined %>% 
  ggplot(aes(x = date_time, y = temperature_C, color = cover_type))+
  geom_line(size = 0.625)+
  labs(x = "", 
       y = "temperature, celsius")+
  geom_hline(yintercept = 0, color = "black", linetype = "longdash")+
  scale_x_datetime( breaks=("1 month"), 
                    labels=date_format("%b-%y"), 
                    timezone = "UTC-9:00") + 
  #scale_color_manual(values = PNWColors::pnw_palette("Bay", 2))+
  scale_color_manual(values = c('#006d77', '#e29578'))+
  facet_grid(depth ~., scales = "free_y", 
             labeller = as_labeller(c('5' = "5 cm", '75' = "75 cm"))) +
  theme_er()+
  theme(axis.text.x = element_text (angle = 90, size = 10))
  
ggsave("output/fairbanks_temperature.tiff", plot = fairbanks_temperature, height = 6.6, width = 10)



fairbanks_temperature_ftcexample = 
  fairbanks_ftc_combined %>% 
  filter(date_time >= "2021-09-13 6:00:00" & cover_type == "closed" &
           depth == 5) %>% 
  ggplot(aes(x = date_time, y = temperature_C, color = cover_type))+
  geom_line(size = 0.9)+
  labs(x = "", 
       y = "temperature, celsius")+
  geom_hline(yintercept = 0, color = "black", linetype = "longdash")+
  # geom_hline(yintercept = 2, color = "grey", linetype = "shortdash")+
  #geom_hline(yintercept = -2, color = "grey", linetype = "shortdash")+
  scale_x_datetime( breaks=("1 day"), 
                    labels=date_format("%b-%d"), 
                    timezone = "UTC-9:00") + 
  #scale_color_manual(values = PNWColors::pnw_palette("Bay", 2))+
  scale_color_manual(values = c('#9d8189'))+
  # facet_grid(depth ~., scales = "free_y", 
  #            labeller = as_labeller(c('5' = "5 cm", '75' = "75 cm"))) +
  theme_er()+
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.text.x = element_text (angle = 45, size = 9, hjust = 1))

fairbanks_temperature_ftcexample_lineonly = 
  fairbanks_ftc_combined %>% 
  filter(date_time >= "2021-09-13 6:00:00" & cover_type == "closed" &
           depth == 5) %>% 
  ggplot(aes(x = date_time, y = temperature_C, color = cover_type))+
  geom_line(size = 0.9)+
  labs(x = "", 
       y = "")+
  geom_hline(yintercept = 0, color = "#E4A8BC", linetype = "longdash")+
  # geom_hline(yintercept = 2, color = "grey", linetype = "shortdash")+
  #geom_hline(yintercept = -2, color = "grey", linetype = "shortdash")+
  scale_x_datetime( breaks=("1 day"), 
                    labels=date_format("%b-%d"), 
                    timezone = "UTC-9:00") + 
  #scale_color_manual(values = PNWColors::pnw_palette("Bay", 2))+
  scale_color_manual(values = c('black'))+
  # facet_grid(depth ~., scales = "free_y", 
  #            labeller = as_labeller(c('5' = "5 cm", '75' = "75 cm"))) +
  theme_er()+
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
        
  )


ggsave("output/fairbanks_temperature_ftcexample_lineonly.png", plot = fairbanks_temperature_ftcexample_lineonly, height = 3.5, width = 10)



fairbanks_temperature_ftcexample2 = 
  fairbanks_ftc_combined %>% 
  filter(date_time >= "2021-09-14 6:00:00" & date_time <= "2021-09-16 6:00:00" & cover_type == "closed" &
           depth == 5) %>% 
  ggplot(aes(x = date_time, y = temperature_C, color = cover_type))+
  geom_line(size = 0.9)+
  labs(x = "", 
       y = "temperature, celsius")+
  geom_hline(yintercept = 0, color = "black", linetype = "longdash")+
  # geom_hline(yintercept = 2, color = "grey", linetype = "shortdash")+
  #geom_hline(yintercept = -2, color = "grey", linetype = "shortdash")+
  scale_x_datetime( breaks=("6 hours"), 
                    labels=date_format("%r"), 
                    timezone = "UTC-9:00") + 
  #scale_color_manual(values = PNWColors::pnw_palette("Bay", 2))+
  scale_color_manual(values = c('#9d8189'))+
  # facet_grid(depth ~., scales = "free_y", 
  #            labeller = as_labeller(c('5' = "5 cm", '75' = "75 cm"))) +
  theme_er()+
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x = element_text (angle = 45, size = 9, hjust = 1))


ggsave("output/fairbanks_temperature_ftcexample2.png", plot = fairbanks_temperature_ftcexample2, height = 4.5, width = 6)


###FTC time

library(purrr)
library(reshape)
library(reshape2)
library(plyr)
library(Rmisc)
library(devtools)
library(FTCQuant)
library(data.table)
library(scales)


#convert to factors


#open 5

for_ftc_dat_open_5 =
  fairbanks_ftc_combined %>% 
  dplyr::rename("date" = "date_time") %>%
  separate(date, sep = " ", into = c("date1", "time")) %>%
  mutate(date = as.Date(paste(date1,time, sep = "-"))) %>% 
  filter(cover_type == "open" & depth == "5") %>% 
  na.omit() %>% 
  dplyr::select(c(date, temperature_C)) %>% 
  dplyr::rename("temp" = "temperature_C") 

column_names_open5=as.vector(colnames(for_ftc_dat_open_5[,2:ncol(for_ftc_dat_open_5)]))
column_number_open5=as.vector(2:ncol(for_ftc_dat_open_5))
#This function makes a list of elements, extracting row 1 (date) and then sequentially each column (y)
#Each date and data column is a new element in the list, named by the list of column names above (x) 
funopen5 <- function(name,number) {
  data.list_open5 <- list(name = for_ftc_dat_open_5[,c(1,number)])
}
#This applies the function to loop through the list of column names (name) and numbers (number) 
data.list_open5 = mapply(funopen5,column_names_open5,column_number_open5)
FTC_open_5 =freeze.thaw.analysis(data.list_open5, mag.vec=0, dur.vec=1, thres.vec=0)

FTC_dat_open_5 = cbind(FTC_open_5$data, column_names_open5)


#open 75

for_ftc_dat_open_75 =
  fairbanks_ftc_combined %>% 
  dplyr::rename("date" = "date_time") %>%
  separate(date, sep = " ", into = c("date1", "time")) %>%
  mutate(date = as.Date(paste(date1,time, sep = "-"))) %>% 
  filter(cover_type == "open" & depth == "75") %>% 
  na.omit() %>% 
  dplyr::select(c(date, temperature_C)) %>% 
  dplyr::rename("temp" = "temperature_C") 

column_names_open75=as.vector(colnames(for_ftc_dat_open_75[,2:ncol(for_ftc_dat_open_75)]))
column_number_open75=as.vector(2:ncol(for_ftc_dat_open_75))
#This function makes a list of elements, extracting row 1 (date) and then sequentially each column (y)
#Each date and data column is a new element in the list, named by the list of column names above (x) 
funopen75 <- function(name,number) {
  data.list_open75 <- list(name = for_ftc_dat_open_75[,c(1,number)])
}
#This applies the function to loop through the list of column names (name) and numbers (number) 
data.list_open75 = mapply(funopen75,column_names_open75,column_number_open75)
FTC_open75 =freeze.thaw.analysis(data.list_open75, mag.vec=0, dur.vec=1, thres.vec=0)

FTC_dat_open75 = cbind(FTC_open75$data, column_names_open75)



#closed 5


for_ftc_dat_closed_5 =
  fairbanks_ftc_combined %>% 
  dplyr::rename("date" = "date_time") %>%
  separate(date, sep = " ", into = c("date1", "time")) %>%
  mutate(date = as.Date(paste(date1,time, sep = "-"))) %>% 
  filter(cover_type == "closed" & depth == "5") %>% 
  na.omit() %>% 
  dplyr::select(c(date, temperature_C)) %>% 
  dplyr::rename("temp" = "temperature_C") 

column_names_closed5=as.vector(colnames(for_ftc_dat_closed_5[,2:ncol(for_ftc_dat_closed_5)]))
column_number_closed5=as.vector(2:ncol(for_ftc_dat_closed_5))
#This function makes a list of elements, extracting row 1 (date) and then sequentially each column (y)
#Each date and data column is a new element in the list, named by the list of column names above (x) 
funclosed5 <- function(name,number) {
  data.list_closed5 <- list(name = for_ftc_dat_closed_5[,c(1,number)])
}
#This applies the function to loop through the list of column names (name) and numbers (number) 
data.list_closed5 = mapply(funclosed5,column_names_closed5,column_number_closed5)
FTC_closed_5 =freeze.thaw.analysis(data.list_closed5, mag.vec=0, dur.vec=1, thres.vec=0)

FTC_dat_closed_5 = cbind(FTC_closed_5$data, column_names_closed5)


#closed 75


for_ftc_dat_closed_75 =
  fairbanks_ftc_combined %>% 
  dplyr::rename("date" = "date_time") %>%
  separate(date, sep = " ", into = c("date1", "time")) %>%
  mutate(date = as.Date(paste(date1,time, sep = "-"))) %>% 
  filter(cover_type == "closed" & depth == "75") %>% 
  na.omit() %>% 
  dplyr::select(c(date, temperature_C)) %>% 
  dplyr::rename("temp" = "temperature_C") 

column_names_closed75=as.vector(colnames(for_ftc_dat_closed_75[,2:ncol(for_ftc_dat_closed_75)]))
column_number_closed75=as.vector(2:ncol(for_ftc_dat_closed_75))
#This function makes a list of elements, extracting row 1 (date) and then sequentially each column (y)
#Each date and data column is a new element in the list, named by the list of column names above (x) 
funclosed75 <- function(name,number) {
  data.list_closed75 <- list(name = for_ftc_dat_closed_75[,c(1,number)])
}
#This applies the function to loop through the list of column names (name) and numbers (number) 
data.list_closed75 = mapply(funclosed75,column_names_closed75,column_number_closed75)
FTC_closed_75 =freeze.thaw.analysis(data.list_closed75, mag.vec=0, dur.vec=1, thres.vec=0)

FTC_dat_closed_75 = cbind(FTC_closed_75$data, column_names_closed75)





