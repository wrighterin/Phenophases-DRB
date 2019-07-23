### 23 July 2019 - Cat #
## Working on code for Erin

## Housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

### Load libraries
library(dplyr)
library(tidyr)
library(lme4)
library(arm)
library(RColorBrewer)
library(ggplot2)

### First start with 2018 data...
# Set Working Directory
setwd("~/Documents/git/Phenophases-DRB")

hts <- read.csv("analyses/input/clean_phenandtraits_growthform.csv", header=TRUE) ## from Cat's repo CGtraits

flodata <- read.csv("data/erindata2019.csv", header=TRUE)
flodata$Individual <- as.character(flodata$Individual)
flodata$year <- 2019

cg18 <- read.csv("data/2018data.csv", header=TRUE)
cg18 <- cg18[!is.na(cg18$year),]
cg18$X<-NULL
cg18$Individual <- cg18$id
cg18$Plot <- cg18$plot
cg18$id <- NULL
cg18$plot <- NULL
cg18$fru.time.start <- cg18$fruit
cg18$leafout <- cg18$lo
cg18$BB.time <- cg18$bb
cg18$fruit <- cg18$lo <- cg18$bb <- NULL
cg18$prov.lat <- cg18$lat
cg18$Species <- cg18$species
cg18$lat <- cg18$species <-NULL

inds <- unique(flodata$Individual)

hts <- hts[(hts$Individual%in%inds),]
hts <- hts[(hts$year==2019),]

hts <- subset(hts, select=c("Individual", "Plot", "ht.diff", "leafout"))

foo <- full_join(hts, flodata)
goo <- full_join(foo, cg18)
goo <- subset(goo, select=c("Individual", "Plot", "year", "leafout", "BB.time", "fru.time.start", "prov.lat",
                            "Species", "ht.diff"))

write.csv(goo, file="data/erindata_heights_twoyears.csv", row.names=FALSE)

goo <- goo[!is.na(goo$fru.time.start),]
###
mod.fruit <- lmer(fru.time.start ~ leafout + prov.lat + (1|Species), data=goo) ### This is interesting!!
display(mod.fruit)

foo <- foo[!is.na(foo$Species),]

cols <- colorRampPalette(brewer.pal(7,"Accent"))(4)

plot.name <- ggplot(goo, aes(x=Species, y=fru.time.start, col=as.factor(prov.lat), fill=as.factor(prov.lat))) + geom_boxplot(aes(col=as.factor(prov.lat))) +
  scale_fill_manual(name="Provenance", values=cols,
                    labels=c("HF"="Harvard Forest, MA",
                             "WM" = "White Mountains, NH",
                             "GR" = "Second College Grant, NH",
                             "SH" = "Saint-Hippolyte, Quebec")) +
  scale_color_manual(name="Provenance", values=cols,
                     labels=c("HF"="Harvard Forest, MA",
                              "WM" = "White Mountains, NH",
                              "GR" = "Second College Grant, NH",
                              "SH" = "Saint-Hippolyte, Quebec")) + 
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), #legend.position = "none",
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), 
        axis.title.x = element_blank()) + # top, right, bottom, left
  #scale_y_continuous(expand = c(0, 0)) +
  ylab("Day of year (fruit time)") + guides(col=FALSE)


quartz()
plot.name

goo <- goo[!is.na(goo$Species),]

colz <- colorRampPalette(brewer.pal(11,"Set3"))(8)
ggplot(goo, aes(x=leafout, y=fru.time.start, col=Species)) + geom_smooth(aes(col=Species), method="lm", se=FALSE) + 
  scale_color_manual(name="Species", values=colz,
                     labels=unique(sort(goo$Species))) + theme_classic() + xlab("Day of year (leafout)") + ylab("Day of year (first fruit)")
#ggplot(goo, aes(x=leafout, y=fru.time.start, col=Species)) + geom_point(aes(col=Species)) + 
 # scale_color_manual(name="Species", values=colz,
  #                   labels=unique(sort(goo$Species))) + theme_classic() 


ggplot(goo, aes(x=prov.lat, y=fru.time.start, col=Species)) + geom_smooth(aes(col=Species), method="lm", se=FALSE) + 
  scale_color_manual(name="Species", values=colz,
                     labels=unique(sort(goo$Species))) + theme_classic() + xlab("Day of year (leafout)") + ylab("Day of year (first fruit)")
#ggplot(goo, aes(x=prov.lat, y=fru.time.start, col=Species)) + geom_point(aes(col=Species)) + 
 # scale_color_manual(name="Species", values=colz,
  #                   labels=unique(sort(goo$Species))) + theme_classic() 

goo <- goo[!duplicated(goo),]
goo$ind_plot <- paste(goo$Individual, goo$Plot)
goo$fruit.avg <- ave(goo$fru.time.start, goo$ind_plot)
plotdf <- subset(goo, select=c("fruit.avg", "Species", "prov.lat"))
plotdf <- plotdf[!duplicated(plotdf),]
plotdf <- group_by(plotdf, Species, prov.lat)
ggplot(plotdf, aes(x=Species, y=fruit.avg, fill=Species, alpha=prov.lat)) + ### for some reason this isn't working... maybe try Excel?
  geom_bar(stat="identity", position="dodge2") + 
  scale_fill_manual(name="Species", values=colz,
                     labels=unique(sort(goo$Species))) + 
  theme_classic() + xlab("") + ylab("Day of year (first fruit)") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        axis.text.x = element_text(face = "italic", angle=45, hjust=1))



#### Let's try looking at growth rate now instead
mod.growth<-lmer(ht.diff ~ leafout + prov.lat + (1|Species), data=goo) ## this isn't interesting!
display(mod.growth)
