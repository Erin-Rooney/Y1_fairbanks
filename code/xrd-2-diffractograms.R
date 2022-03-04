#xrd diffractogram patterns
# EC Rooney
# Feb 9 2022

source("code/fticr-0-packages.R")


library(rxylib)
library(magrittr)
library(utils)

# 

# importing XRD data ------------------------------------------------------
# set the path where the files are being stored
# then run the import function. 

y1metadata = read.csv("processed/y1_metadata.csv") %>% 
  mutate(ID = str_remove(ID, "Y1 "))  

XY_PATH = "processed"

import_xy_data = function(XY_PATH){
  # first, pull all the file names with the target pattern (".xy")
  filePaths <- list.files(path = XY_PATH,pattern = "*.xy", full.names = TRUE)
  
  # then, run this function to combine (rbind) all the imported dataframes
  xy_dat <- do.call(rbind, lapply(filePaths, function(path) {

    # read the .xy files, they come in as lists. convert to dataframe
    data <- read_xyData(path)
    data_df <- data.frame(data[["dataset"]][[1]][["data_block"]])
    
    # then add a new column `source` to denote the file name
    data_df[["source"]] <- rep(path, nrow(data_df))
    data_df}))
  
}

xydata = import_xy_data(XY_PATH) %>% 
  mutate(source = str_remove(source, "processed/Y1"),
         source = str_remove(source, ".xy"))  

xydata_cleaned = 
  xydata %>% 
  separate(source, sep = " ", into = c("ID", "sample")) %>% 
  left_join(y1metadata, by = 'ID') %>% 
  mutate(cover_type = recode(cover_type, "Canopy" = "Closed")) %>% 
  mutate(sample_rep = paste(rep, " ", top_cm, "-", bottom_cm)) %>% 
  na.omit() 
  
  
## ^^ clean up the source column using str_remove() and separate()
## and then left_join() the sample key

## make diffractograms
open_backslope = 
  xydata_cleaned %>%
  filter(slopepos == "backslope" & cover_type == "Open") %>% 
  ggplot(aes(x = V1, y = V2, color = sample_rep))+
  geom_line()+
  facet_grid(slopepos ~ cover_type)+
  labs(y = 'intensity (counts)',
       x = "Position, °2Theta",
       color="replicate and depth, cm")+
  theme_er()

closed_backslope = 
  xydata_cleaned %>%
  filter(slopepos == "backslope" & cover_type == "Closed") %>% 
  ggplot(aes(x = V1, y = V2, color = sample_rep))+
  geom_line()+
  facet_grid(slopepos ~ cover_type)+
  labs(y = 'intensity (counts)',
       x = "Position, °2Theta",
       color="replicate and depth, cm")+
  theme_er()


open_low_backslope = 
  xydata_cleaned %>%
  filter(slopepos == "low backslope" & cover_type == "Open") %>% 
  ggplot(aes(x = V1, y = V2, color = sample_rep))+
  geom_line()+
  facet_grid(slopepos ~ cover_type)+
  labs(y = 'intensity (counts)',
       x = "Position, °2Theta",
       color="replicate and depth, cm")+
  theme_er()

closed_low_backslope = 
  xydata_cleaned %>%
  filter(slopepos == "low backslope" & cover_type == "Closed") %>% 
  ggplot(aes(x = V1, y = V2, color = sample_rep))+
  geom_line()+
  facet_grid(slopepos ~ cover_type)+
  labs(y = 'intensity (counts)',
       x = "Position, °2Theta",
       color="replicate and depth, cm")+
  theme_er()


open_footslope = 
  xydata_cleaned %>%
  filter(slopepos == "footslope" & cover_type == "Open") %>% 
  ggplot(aes(x = V1, y = V2, color = sample_rep))+
  geom_line()+
  facet_grid(slopepos ~ cover_type)+
  labs(y = 'intensity (counts)',
       x = "Position, °2Theta",
       color="replicate and depth, cm")+
  theme_er()

closed_footslope = 
  xydata_cleaned %>%
  filter(slopepos == "footslope" & cover_type == "Closed") %>% 
  ggplot(aes(x = V1, y = V2, color = sample_rep))+
  geom_line()+
  facet_grid(slopepos ~ cover_type)+
  labs(y = 'intensity (counts)',
       x = "Position, °2Theta",
       color="replicate and depth, cm")+
  theme_er()


ggsave("output/xrdiffclosedback.tiff", plot = closed_backslope, height = 4.6, width = 7)
ggsave("output/xrdiffclosedlowback.tiff", plot = closed_low_backslope, height = 4.6, width = 7)
ggsave("output/xrdiffclosedfoot.tiff", plot = closed_footslope, height = 4.6, width = 7)
ggsave("output/xrdiffopenback.tiff", plot = open_backslope, height = 4.6, width = 7)
ggsave("output/xrdiffopenlowback.tiff", plot = open_low_backslope, height = 4.6, width = 7)
ggsave("output/xrdiffopenfoot.tiff", plot = open_footslope, height = 4.6, width = 7)




#
# -------------------------------------------------------------------------


##start here in re-runs of this code-------------------------


# library(powdR)
# 
# #healysoils = 
#   plot(dataFiles, wavelength = "Cu",
#        xlim = c(0,100),
#        normalise = FALSE)+
#   geom_text(data = gglabel2, aes(x = x, y = y, label = label), color = "black", size = 3.5, angle = 90)+
#   labs(y = 'intensity (counts)',
#        x = "Position, °2Theta",
#        color="replicate and depth, cm")+
#   # scale_color_manual(values = c("#fd474d","#fdb678","#fcff9f","#91f698","#51fff9",
#   #                               "#4f52ef","#9c4bf8","#db4ffa"
#   # 
#   # ))+
#   theme_er()
