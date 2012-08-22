library(raster)
library(rgdal)
r <- raster("E:/workspace/Landfire/esp_48r_fix2") 
variety <- function (x) {length(table(x))}
nbrhd <- focal(r, w=33, fun=variety)