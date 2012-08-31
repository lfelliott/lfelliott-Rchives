require(randomForest)
require(raster)
library(maptools)
setwd("d:/r/randomforest")
# read csv of classes with stack data
sample_df <- readShapePoints("elk_pts.shp")

# get data stack
datastack <- stack("stack_nc_i.img")
# convert to brick
databrick <- brick(datastack)
# replace 0 with NA for pixels with 0 values in all layers; this step not really necessary
elkbrick <- calc(databrick, fun = function(x){x[rowSums(x) == 0,] <- NA; x})
# rename layers in stack to match variables for running prediction
pts_extract <- data.frame(extract(elkbrick, sample_df))
pts_extract <- cbind(sample_df$id, pts_extract)

#layerNames(elkbrick) <- colnames[seq(2,38)]
# predict out to file
#predict(elkbrick, elkrf, filename="elkout.img", type = 'class', format="HFA", datatype = 'INT2U', overwrite=T)
