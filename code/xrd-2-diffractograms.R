#xrd diffractogram patterns
# EC Rooney
# Feb 9 2022

source("code/fticr-0-packages.R")

library(rxylib)
library(magrittr)
library(utils)

# 
#undo comment out and run for first-time run (line 15-70)


dataFiles <- lapply(Sys.glob("processed/*.xy"), read_xyData)




dataFilesextracted <- lapply(dataFiles, data.frame(dataFiles[[1]][["dataset"]][[1]]))

dataFilesextracted <- lapply(dataFiles, data.frame(dataFiles[[1]][["dataset"]][[1]]))


write.csv()

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


# H12433data = read_xyData("processed/H1-24-33.xy")
# H13350data = read_xyData("processed/H1-33-50.xy")
# H15060data = read_xyData("processed/H1-50-60.xy")
# H22834data = read_xyData("processed/H2-28-34.xy")
# H23447data = read_xyData("processed/H2-34-47.xy")
# H24768data = read_xyData("processed/H2-47-68.xy")
# H34050data = read_xyData("processed/H3-40-50.xy")
# H33038data = read_xyData("processed/H3-30-38.xy")
# T14060data = read_xyData("processed/T1-40-60.xy")
# T16067data = read_xyData("processed/T1-60-67.xy")
# T22838data = read_xyData("processed/T2-28-38.xy")
# T23844data = read_xyData("processed/T2-38-44.xy")
# T24458data = read_xyData("processed/T2-44-58.xy")
# T33541data = read_xyData("processed/T3-35-41.xy")
# T34150data = read_xyData("processed/T3-41-50.xy")
# T35058data = read_xyData("processed/T3-50-58.xy")
# 
# "H1_24_33dataframe" = data.frame(H12433data[["dataset"]][[1]][["data_block"]])
# "H1_33_50dataframe" = data.frame(H13350data[["dataset"]][[1]][["data_block"]])
# "H1_50_60dataframe" = data.frame(H15060data[["dataset"]][[1]][["data_block"]])
# "H2_28_34dataframe" = data.frame(H22834data[["dataset"]][[1]][["data_block"]])
# "H2_34_47dataframe" = data.frame(H23447data[["dataset"]][[1]][["data_block"]])
# "H2_47_68dataframe" = data.frame(H24768data[["dataset"]][[1]][["data_block"]])
# "H3_40_50dataframe" = data.frame(H34050data[["dataset"]][[1]][["data_block"]])
# "H3_30_38dataframe" = data.frame(H33038data[["dataset"]][[1]][["data_block"]])
# "T1_40_60dataframe" = data.frame(T14060data[["dataset"]][[1]][["data_block"]])
# "T1_60_67dataframe" = data.frame(T16067data[["dataset"]][[1]][["data_block"]])
# "T2_28_38dataframe" = data.frame(T22838data[["dataset"]][[1]][["data_block"]])
# "T2_38_44dataframe" = data.frame(T23844data[["dataset"]][[1]][["data_block"]])
# "T2_44_58dataframe" = data.frame(T24458data[["dataset"]][[1]][["data_block"]])
# "T3_35_41dataframe" = data.frame(T33541data[["dataset"]][[1]][["data_block"]])
# "T3_41_50dataframe" = data.frame(T34150data[["dataset"]][[1]][["data_block"]])
# "T3_50_58dataframe" = data.frame(T35058data[["dataset"]][[1]][["data_block"]])
# 
# 

## OUTPUTS 1
# write.csv(H1_24_33dataframe,"output/H1_24_33dataframe.csv", row.names = FALSE)
# write.csv(H1_33_50dataframe,"output/H1_33_50dataframe.csv", row.names = FALSE)
# write.csv(H1_50_60dataframe,"output/H1_50_60dataframe.csv", row.names = FALSE)
# write.csv(H2_28_34dataframe,"output/H2_28_34dataframe.csv", row.names = FALSE)
# write.csv(H2_34_47dataframe,"output/H2_34_47dataframe.csv", row.names = FALSE)
# write.csv(H2_47_68dataframe,"output/H2_47_68dataframe.csv", row.names = FALSE)
# write.csv(H3_40_50dataframe,"output/H3_40_50dataframe.csv", row.names = FALSE)
# write.csv(H3_30_38dataframe,"output/H3_30_38dataframe.csv", row.names = FALSE)
# write.csv(T1_40_60dataframe,"output/T1_40_60dataframe.csv", row.names = FALSE)
# write.csv(T1_60_67dataframe,"output/T1_60_67dataframe.csv", row.names = FALSE)
# write.csv(T2_28_38dataframe,"output/T2_28_38dataframe.csv", row.names = FALSE)
# write.csv(T2_38_44dataframe,"output/T2_38_44dataframe.csv", row.names = FALSE)
# write.csv(T2_44_58dataframe,"output/T2_44_58dataframe.csv", row.names = FALSE)
# write.csv(T3_35_41dataframe,"output/T3_35_41dataframe.csv", row.names = FALSE)
# write.csv(T3_41_50dataframe,"output/T3_41_50dataframe.csv", row.names = FALSE)
# write.csv(T3_50_58dataframe,"output/T3_50_58dataframe.csv", row.names = FALSE)


##start here in re-runs of this code-------------------------



