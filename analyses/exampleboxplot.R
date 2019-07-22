### 22 July 2019 - Cat
# Example code for boxplots for Erin

library(ggplot2)
library(RColorBrewer)

# df = dataframe name
# x = column name for species
# y = column name for y variable
# col = column name for provenance.lat - can be name (i.e, HF instead of 42)

cols <- colorRampPalette(brewer.pal(7,"Accent"))(4)

plot.name <- ggplot(mat, aes(x=species, y=y.variable, col=provenance.lat, fill=provenance.lat)) + geom_boxplot(aes(col=as.factor(provenance.lat))) +
  scale_fill_manual(name="Provenance", values=cols,
                    labels=c("HF"="Harvard Forest, MA",
                             "WM" = "White Mountains, NH"
                             "GR" = "Second College Grant, NH"
                             "SH" = "Saint-Hippolyte, Quebec")) +
  scale_color_manual(name="Provenance", values=cols,
                    labels=c("HF"="Harvard Forest, MA",
                             "WM" = "White Mountains, NH"
                             "GR" = "Second College Grant, NH"
                             "SH" = "Saint-Hippolyte, Quebec")) + 
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0, axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white"), #legend.position = "none",
        plot.margin = unit(c(1.5,1.5,1.0,1.5), "lines"), 
        axis.title.x = element_blank()) + # top, right, bottom, left
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels=c("BETPEN" = "Betula pendula", "AESHIP" = "Aesculus \nhippocastanum", 
                            ### example of species list - R automatically puts them alphabetically, 
                            #if you want to make them in order of budburst we can chat!
                            "ALNGLU" = "Alnus glutinosa", "FAGSYL"="Fagus sylvatica",
                            "QUEROB"="Quercus robur", "FRAEXC"="Fraxinus \nexcelsior")) +
  ylab("Name of y variable") + guides(col=FALSE)
