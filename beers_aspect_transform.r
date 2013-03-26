library(rgdal)
library(raster)
library(aspace)
aspect <- raster("hosp_aspect")
r <- aspect
r2 <- setValues(r, getValues(sin_d(r + 45) + 1))
r2[aspect < 0] <- -1
