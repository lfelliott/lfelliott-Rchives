library(raster)
rsize <- 11 * 100
r <- raster(ncol=rsize, nrow=rsize)
#values(r) <- as.integer(runif(rsize*rsize)*10)
values(r) <- c(rep(1, 1100*1100/2), rep(2, 1100*1100/2)
#r <- raster("E:/workspace/Landfire/esp_48r_fix2") 
variety <- function (x) {length(table(x))}
nbrhd <- focal(r, w=11, fun=variety)
writeRaster(nbhrd, filename="nbrhd.img", format='HFA', datatype='INT2U')
