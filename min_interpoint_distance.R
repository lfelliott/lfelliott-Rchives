library(sp)
library(maptools)
shape <- readShapePoints("point_locations_utm.shp")
dist <- spDists(shape, shape, longlat = F)
dist[dist ==0] <- 1000
x <- NULL
for (i in 1:dim(dist)[1]) x <- c(x, min(dist[i,]))