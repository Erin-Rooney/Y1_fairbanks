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

xydata = import_xy_data(XY_PATH)
## ^^ clean up the source column using str_remove() and separate()
## and then left_join() the sample key

## make diffractograms
xydata %>% 
  ggplot(aes(x = V1, y = V2, color = source))+
  geom_line()


#
# -------------------------------------------------------------------------


#trying to bring all of the .xy ASCII files in
dataFiles <- lapply(Sys.glob("processed/*.xy"), read_xyData)

#now bringing them in as not Rxylib files, which are impossible to deal with
dataFiles2 <- lapply(Sys.glob("processed/*.xy"), read.delim)

#now trying to bring them in with names but only six are loading. Why? WHy???
#also, two columns are loading as one. Need to by separated by " "
filenames <- head(list.files("processed/", pattern = "*.xy", recursive = TRUE, full.names = TRUE))
filenames
tools::file_path_sans_ext(basename(filenames))

dataFiles2b <- setNames(lapply(filenames, readLines), 
                    tools::file_path_sans_ext(basename(filenames)))


#now all loaded, not rxylib, no names, two columns lumped into one column
list_data <- Map(as.data.frame, dataFiles2)

#don't know why I did this. Just trying things.
write.csv(list_data, "processed/list_data.csv")

##start here in re-runs of this code-------------------------


library(powdR)

#healysoils = 
  plot(dataFiles, wavelength = "Cu",
       xlim = c(0,100),
       normalise = FALSE)+
  geom_text(data = gglabel2, aes(x = x, y = y, label = label), color = "black", size = 3.5, angle = 90)+
  labs(y = 'intensity (counts)',
       x = "Position, °2Theta",
       color="replicate and depth, cm")+
  # scale_color_manual(values = c("#fd474d","#fdb678","#fcff9f","#91f698","#51fff9",
  #                               "#4f52ef","#9c4bf8","#db4ffa"
  # 
  # ))+
  theme_er()
