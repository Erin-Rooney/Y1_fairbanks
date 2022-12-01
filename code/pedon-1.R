library(aqp)
#library(plyr)
library(RColorBrewer)
library(latticeExtra)
#library(plyr)
library(reshape)
library(tidyverse)
library(sp)
library(scales)
library(sharpshootR)
library(soilDB)

#load csv
#pedon_load <- read.csv("processed/metadata_Y1_munsell.csv", stringsAsFactors=FALSE)
pedon <- read.csv("processed/metadata_Y1_munsell_noO.csv", stringsAsFactors=FALSE)
pedon_load <- read.csv("processed/metadata_Y1_munsell_noO.csv", stringsAsFactors=FALSE)

pedon2 = 
  pedon_load %>%   
  separate(id, sep = "-", into = c("proj", "slope_num",
                                   "cover", "rep")) %>%
  mutate(slopepos = recode(slopepos, "low_backslope" = "low backslope")) %>% 
  mutate(cover_type2 = paste(cover_type, rep),
         cover_type3 = paste(slopepos, cover_type, rep))


#look at first several lines of imported file
head(pedon)

#double-check structure of the data
str(pedon)

#gives the class of the object
class(pedon)

#create new column for hex color and convert munsell color to hex format
pedon$soilcolor <- munsell2rgb(pedon$hue, pedon$value, pedon$chroma)


#double-check new "soil color" column
print(pedon)

#convert to apq object "soil profile collection"
depths(pedon) <- id ~ top + bottom


#check the new class
str(pedon)

class(pedon)
summary(pedon)


#plot
plot(pedon, name = 'horizon', color = 'soilcolor') 


#####

###by elevation/slope position

pedon_footslope = pedon2 %>% filter(slopepos == "footslope")
pedon_backslope = pedon2 %>% filter(slopepos == "backslope")
pedon_lowbackslope = pedon2 %>% filter(slopepos == "low backslope")

#create new column for hex color and convert munsell color to hex format
pedon_footslope$soilcolor <- munsell2rgb(pedon_footslope$hue, pedon_footslope$value, pedon_footslope$chroma)
pedon_backslope$soilcolor <- munsell2rgb(pedon_backslope$hue, pedon_backslope$value, pedon_backslope$chroma)
pedon_lowbackslope$soilcolor <- munsell2rgb(pedon_lowbackslope$hue, pedon_lowbackslope$value, pedon_lowbackslope$chroma)
pedon2$soilcolor <- munsell2rgb(pedon2$hue, pedon2$value, pedon2$chroma)


#convert to apq object "soil profile collection"
depths(pedon_footslope) <- cover_type2 ~ top + bottom
depths(pedon_backslope) <- cover_type2 ~ top + bottom
depths(pedon_lowbackslope) <- cover_type2 ~ top + bottom
depths(pedon2) <- cover_type3 ~ top + bottom



#plot
par(mar=c(0,0,0,0))
    
    plot(pedon_footslope, name = 'horizon', color = 'soilcolor',
         groups = 'cover_type',
         group.name.offset = c(-10, -15),
         break.offset = 0.6,
         name.style = 'center-center',
         width = 0.3
)
    par(mar=c(0,0,0,0))
    
    plot(pedon_backslope, name = 'horizon', color = 'soilcolor',
         groups = 'cover_type',
         group.name.offset = c(-10, -15),
         break.offset = 0.6,
         name.style = 'center-center',
         width = 0.3
    )
  
    par(mar=c(0,0,0,0))
    
    plot(pedon_lowbackslope, name = 'horizon', color = 'soilcolor',
         groups = 'cover_type',
         group.name.offset = c(-10, -15),
         break.offset = 0.6,
         name.style = 'center-center',
         width = 0.3
    )
    

backslope_fig = plot(pedon_backslope, name = 'horizon', color = 'soilcolor') 
lowbackslope_fig = plot(pedon_lowbackslope, name = 'horizon', color = 'soilcolor') 

plotSPC(pedon2, label = "cover_type3", id.style = "side", name = 'horizon', color = 'soilcolor') 

plotSPC(pedon2, plot.order = new.order, print.id = TRUE, relative.pos = pos[new.order])

  
axis(side=1, at = pos[new.order], labels = x$elev[new.order], line = 1)
mtext('Elevation (m) | Not to Scale', side = 1, line = 3.25)


par(mar=c(1,1,1,1))

ggsave("output/footslope_fig.tiff", plot = footslope_fig, height = 4.5, width = 10)
ggsave("output/backslope_fig.tiff", plot = backslope_fig, height = 4.5, width = 10)
ggsave("output/lowbackslope_fig.tiff", plot = lowbackslope_fig, height = 4.5, width = 10)



pos <- rescale(pedon2$elev, to = c(1, length(pedon2)))
pos <- fixOverlap(pos, thresh = 0.65)

new.order <- order(pedon2$elev)

# plot profiles in the new order
par(mar=c(4.5,1,2,3))
plotSPC(pedon2, label = "cover_type3", id.style = "side", name = 'horizon', color = 'soilcolor', plot.order = new.order, print.id = TRUE, relative.pos = pos[new.order])


# the "brackets" will automatically follow the new ordering
addBracket(s, tick.length = 0, lwd=10, col=rgb(red=0, green=0, blue=1, alpha=0.25))
addDiagnosticBracket(x, kind='argillic horizon', col='red')

# add an axis with elevation associated with each profile
axis(side=1, at = pos[new.order], labels = pedon2$elev[new.order], line = 1)
mtext('Elevation (m) | Not to Scale', side = 1, line = 3.25)


#########################



