library(rgdal)
library(raster)
library(aspace)
aspect <- raster("hosp_aspect")
r <- aspect
r2 <- setValues(r, getValues(sin_d(r + 45) + 1))
r3 <- setValues(r, getValues(cos_d(45 - r) + 1))
r2[aspect < 0] <- -1
writeRaster(r3, "hosp_beers_cos.img", format='HFA')
