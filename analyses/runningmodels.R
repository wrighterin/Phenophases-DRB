### 23 July 2019 - Cat #
## Working on code for Erin

## Housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

### Load libraries
library(dplyr)
library(tidyr)

### First start with 2018 data...
# Set Working Directory
setwd("~/Documents/git/Phenophases-DRB")

hts <- read.csv("analyses/input/clean_phenandtraits_growthform.csv", header=TRUE) ## from Cat's repo CGtraits

flodata <- read.csv("data/erindata2019.csv", header=TRUE)
flodata$Individual <- as.character(flodata$Individual)

inds <- unique(flodata$Individual)

hts <- hts[(hts$Individual%in%inds),]
hts <- hts[(hts$year==2019),]

hts <- subset(hts, select=c("Individual", "Plot", "ht.diff", "leafout"))

foo <- full_join(hts, flodata)

write.csv(foo, file="data/erindata2019_heights.csv", row.names=FALSE)
