processveg <- function(veg){
logveg <- log(veg + 1)
dis.log.bc <- dsvdis(logveg, 'bray')
flex <- as.hclust(agnes(dis.log.bc, method='flex', par.method=0.625))
veg.decor <- decorana(veg)
veg.nmds <- nmds(dis.log.bc, k=3)
assign("veg.decor", veg.decor, envir = .GlobalEnv)
assign("veg.nmds", veg.nmds, envir=.GlobalEnv)
assign("flex", flex, envir=.GlobalEnv)
assign("logveg", logveg, envir=.GlobalEnv)
assign("dis.log.bc", dis.log.bc, envir=.GlobalEnv)
}
plotflex <- function(){plot(flex, hang=-1)}
plotnmds <- function(){ordiplot(veg.nmds, display='sites', type='t')}

importcsv <- function(csvfile){
veg <- data.frame(reshape(read.csv(csvfile), direction='wide', idvar='plot', timevar='species'))
row.names(veg) <- veg$plot
veg$plot <- NULL
names(veg) <- sub("cover.","",names(veg))
veg[is.na(veg)] <- 0
return(veg)
}

striprare <- function(veg){
require(labdsv)
veg1 <- veg[,colSums(veg) > 0.5]
veg2 <- vegtab(veg1, min=1)
return(veg2)
}

plotordflex <- function(cluster_number){
fcolors <- c('firebrick', 'blue', 'forestgreen', 'gold', 'gray0', 'darkorange')
plotnmds()
chullord(veg.nmds, cutree(flex, cluster_number), cols=fcolors)
}

cutflex <- function(cluster_number){
require(dendroextras)
fcolors <- c('firebrick', 'blue', 'forestgreen', 'gold', 'gray0', 'darkorange')
flex_result <- slice(flex, cluster_number)
frdf <- data.frame(flex_result)
frdf$plot <- rownames(frdf)
colnames(frdf) <- c("cluster", "plot")
fr <- array(frdf)
write.csv(frdf, "flex_result.csv", row.names=FALSE)
plotordflex(cluster_number)
colorplot <- colour_clusters(flex, cluster_number, col=fcolors[1:cluster_number])
plot(colorplot)
return(flex_result)
}

loadlibraries <- function(){
library(vegan)
library(labdsv)
library(rgl)
library(optpart)
library(BiodiversityR)
library(dendroextras)
}

removeplots <- function(veg, plots){
veg <- veg[!(rownames(veg) %in% plots),]
veg <- striprare(veg)
return(veg)
}