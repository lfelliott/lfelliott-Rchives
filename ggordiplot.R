
library(ggplot2)
library(plyr)
windowsFonts(Times=windowsFont("TT Times New Roman"))
windowsFonts(Book=windowsFont("TT Book Antiqua"))
# use windowsFonts() to see what font mappings are currently in effect

xydf <- data.frame(nmds.scores)
clusters <- clusters$clustering
xydf[,"cluster"] <- clusters
myColors <- c("blue", "black", "forestgreen", "firebrick", "blue", "darkgoldenrod", "black")
names(myColors) <- levels(factor(xydf$cluster))
colScale <- scale_colour_manual(name="cluster", values=myColors)

# Set some linetypes
lt = c("solid", "solid", "solid", "solid", "dashed", "solid", "dashed")
# also available longdash dotted and others
names(lt) <- levels(factor(xydf$cluster))


find_hull <- function(df) df[chull(df$Dim1, df$Dim2),]
hulls <- ddply(xydf, "cluster", find_hull)
hulls$Dim3 <- NULL
myTheme <- theme(legend.position = "none", panel.background = element_blank(), panel.grid.minor=element_blank(), axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),panel.border = element_rect(colour="black", fill = NA))
# if you put label in initial ggplot call, this fails. You must put the label in geom_text, following the geom_polygon
p <- ggplot(xydf, aes(Dim1, Dim2, colour=factor(cluster)))
ghull <- geom_polygon(data=hulls, fill=NA, linetype=lt[hulld$cluster])
gtext <- geom_text(data=xydf, aes(x=Dim1, y = Dim2, label=rownames(xydf)), family="Times", size=6)
p + ghull + gtext + colScale + myTheme + scale_x_continuous(name="nmds1") + scale_y_continuous(name="nmds2")


library(ggdendro)
ggdendrogram(pipe.flx, theme_dendro=F)
# remove most formating with
ggdendrogram(pipe.flx)

#dhc <- as.dendrogram(pipe.flx)
hcdata <- dendro_data(as.dendrogram(flexresult), type="rectangle")
theme_dendro2 <- function () 
{
    element_blank <- ggplot2::element_blank
    ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.title.x = element_text(colour = NA), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), axis.line = element_line(), axis.line.x = element_blank(),
        axis.ticks = element_blank())
}
ggplot(segment(hcdata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + theme_dendro2() + geom_text(aes(x=x, y=y, label=label, angle=90, hjust=1.1), data = hcdata$label) + theme(axis.text.y=element_text(size = 12, family="sans", face="bold"))
#ggplot(segment(hcdata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + theme_dendro2() + geom_text(aes(x=x, y=y, label=label, angle=90, hjust=1.1, colour=labs$group), data=hcdata$label) + scale_colour_manual("blue", "black", "forestgreen", "firebrick", "blue", "darkgoldenrod", "black") + theme(axis.text.y=element_text(size = 12, family="sans", face="bold")) + theme(legend.position="none")

labs <- label(hcdata)
labs$group <- myColors[sort(order(labs$x)[clusters])]
ggplot(segment(hcdata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + theme_dendro2() + geom_text(aes(x=x, y=y, label=label, angle=90, hjust=1.1, colour=labs$group), data=hcdata$label)+ theme(axis.text.y=element_text(size = 12, family="sans", face="bold"), legend.position="none") + scale_colour_manual(values = c("black", "blue", "darkgoldenrod", "forestgreen", "firebrick", "blue", "black"))
# required futzing with the labs$group values to make it match the ordination colors

# if you put label in initial ggplot call, this fails. You must put the label in geom_text, following the geom_polygon


