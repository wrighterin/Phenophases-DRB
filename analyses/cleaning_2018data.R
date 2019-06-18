#### 18 June 2019 - started by Cat
## Aim: to start a script organizing data for Erin's Darin Butz project
## Major Questions: how does floral/fruit morphological traits affect fruit ripening or other phenophases?

## Housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

### Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set Working Directory
setwd("~/Documents/git/Phenophases-DRB/analyses/input")
pheno18 <-read.csv("2018_CG_datasheet.csv", header=TRUE) ## adding in data - 2018 phenology data to get you started
inds <- read.csv("cg_listofids_2019.csv", header=TRUE) ## your list of individuals

names(inds) <- c("id", "species", "provenance") ## changing the column names so easy to code
inds$provenance <- substr(inds$id, 8, 9) ## taking the substring of the ID name to find provenance site code

## Now let's add in some GPS coordinates
inds$lat <- NA
inds$long <- NA

inds$lat <- ifelse(inds$provenance == "HF", 42.531705, inds$lat)
inds$long <- ifelse(inds$provenance == "HF", -72.189920, inds$long)
inds$lat <- ifelse(inds$provenance == "WM", 44.112337, inds$lat)
inds$long <- ifelse(inds$provenance == "WM", -71.230138, inds$long)
inds$lat <- ifelse(inds$provenance == "GR", 44.794942, inds$lat)
inds$long <- ifelse(inds$provenance == "GR", -71.146683, inds$long)
inds$lat <- ifelse(inds$provenance == "SH", 45.932675, inds$lat)
inds$long <- ifelse(inds$provenance == "SH", -74.025070, inds$long)

### Alright, let's clean up the old 2018 data
pheno18<-gather(pheno18, "date","bbch", -Ind, -Plot)
pheno18<-na.omit(pheno18)
pheno18$date<-substr(pheno18$date, 2,8)
pheno18$date<-as.character(as.Date(pheno18$date,"%m.%d.%y"))
pheno18$doy<-yday(pheno18$date)

pheno18$Ind <- ifelse(pheno18$Ind=="[SORAME09_GR]", "SORAME09_GR", pheno18$Ind)
pheno18$Ind <- ifelse(pheno18$Ind=="{SORAME03_SH}", "SORAME03_SH", pheno18$Ind)
pheno18$species<-substr(pheno18$Ind, 0,6)
pheno18<-dplyr::select(pheno18, -date)
pheno18$species<-ifelse(pheno18$species=="betpap", "BETPAP", pheno18$species)
pheno18$bbch<-gsub(",", " ", pheno18$bbch, fixed=TRUE)


pheno18<-pheno18[!(pheno18$bbch==""),]
dx<-separate(pheno18, bbch, into = c("first", "second"), sep = " (?=[^ ]+$)")
dx<-separate(dx, first, into = c("first", "third"), sep = " (?=[^ ]+$)")

dx$first <- substr(dx$first, 0, 2)
dx$second <- substr(dx$second, 0, 2)
dx$third <- substr(dx$third, 0, 2)

dx$bb<-NA
dx$bb<-ifelse(dx$first=="9" | dx$first=="9-" | dx$first=="11" | dx$second=="9" | dx$second=="9-" |
                dx$second=="11" | dx$third=="9" | dx$third=="9-" | dx$third=="11", dx$doy, dx$bb)
dx$lo<-NA
dx$lo<-ifelse(dx$first=="19" | dx$second=="19" | dx$third=="19", dx$doy, dx$lo)

dx$flo<-NA
flophases<-c("60", "61", "62")
dx$flo<-ifelse(dx$first%in%flophases | dx$second%in%flophases | dx$third%in%flophases, dx$doy, dx$flo)
dx$fruit<-NA
fruitphases <- c("69", "70", "71")
dx$fruit<-ifelse(dx$first%in%fruitphases | dx$second%in%fruitphases | dx$third%in%fruitphases, dx$doy, dx$fruit)
dx$ripe<-NA
dx$ripe<-ifelse(dx$first=="79" | dx$second=="79" | dx$third=="79", dx$doy, dx$ripe)

cg18<-dx
cg18$year<-2018
cg18 <- subset(cg18, select=c("Ind", "Plot", "species",  "bb", "lo", "flo", "fruit", "ripe", "year"))

cg18 <- cg18[!duplicated(cg18),]

cg18 <- cg18 %>% 
  group_by(Ind, Plot, species, year) %>% 
  summarise_all(list(~first(na.omit(.))))
cg18$risk <- cg18$lo - cg18$bb
cg18$ripening <- cg18$ripe - cg18$fruit
cg18$flowers <- cg18$fruit - cg18$flo


### Now let's see how much data we have for your individuals
### Missing plot information for each individual... need to collect to make sure it's all lining up correctly
names(cg18) <- c("id", "plot", "spp", "year", "bb", "lo", "flo", "fruit", "ripe", 
                 "risk", "ripening", "flowers")

inds$spp <- substr(inds$id, 0, 6)

howmuchdata <- left_join(inds, cg18)

## Quick (bad) analysis:
#library(lme4)
library(arm)

ripe <- lm(flowers ~ bb + lat + species, data=howmuchdata)
display(ripe)

## Quick plot...
library(RColorBrewer)

cols <- colorRampPalette(brewer.pal(7,"Dark2"))(7)

quartz()
ggplot(howmuchdata, aes(x=bb, y=ripening, col=species)) + geom_point() +
  xlab("Day of budburst") + ylab("Ripening time (days)") +
  scale_color_manual(name="Species", values=cols) +
  theme(legend.text = element_text(face="italic"))




