#Unused Xrd diffractogram patterns



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

#dataFiles2 <- lapply(Sys.glob("processed/*.xy"), convert_xy2TKA)

# 
# dataFilesTKA <- convert_xy2TKA(dataFiles, file = NULL, overwrite = FALSE)
# 
# 
# dataFilesextracted <- lapply(dataFiles, data.frame(dataFiles[i]))
# 
# 
# 
# 
# dataFilesextracted <- lapply(dataFiles, data.frame(dataFiles[[1]][["dataset"]][[1]]))



# "H1_24_33dataframe" = data.frame(H12433data[["dataset"]][[1]][["data_block"]])

# write.csv(FTC.all, paste0("FTC_1.5_4-",name, ".csv", sep="")) #Change append to name to make parameters in FTC function



# temp = list.files(path = "processed", all.files = TRUE, pattern="*.xy")
# list2env(
#   lapply(setNames(temp, make.names(gsub("*.xy$", "", temp))), 
#          read_xyData), envir = .GlobalEnv)
# 
# 
# tbl <-
#   list.files(path = "processed/", pattern = "*.xy") %>% 
#   map_df(~read_xyData(.))
# 
# 
# do.call_rbind_read_xyData <- function(path, pattern = "*.xy") {
#   files = list.files("processed", "*.xy", full.names = TRUE)
#   do.call(rbind, lapply(files, function(x) read_xyData(x, stringsAsFactors = FALSE)))
# }
# 
# do.call_rbind_read_xyData()

# for (i in 1:length(temp)) assign(temp[i], read_xyData(temp[i]))
# 
# myfiles = lapply(temp, read.delim)
# 
# write.csv(FTC.all, paste0("FTC_1.5_4-",name, ".csv", sep="")) #Change append to name to make parameters in FTC function
# 


