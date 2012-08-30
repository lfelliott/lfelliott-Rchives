require(randomForest)
require(raster)
setwd("d:/r/randomforest")
# read csv of classes with stack data
indata <- read.csv("draft_082412_nc_shrub.data", header = FALSE)
indatadf <- data.frame(indata)
# strip duplicate class and x,y, and input categorical variables to factors
indatadf2 <- data.frame(cbind(indatadf[,1], indatadf[,seq(5,34)], as.factor(indatadf[,35]), as.factor(indatadf[,36]), indatadf[,seq(37,41)]))
# rename variables to avoid more confusion
colnames(indatadf2)[1] <- "types"
colnames(indatadf2)[32] <- "V35"
colnames(indatadf2)[33] <- "V36"

# run random forest
elkrf <- randomForest(as.factor(types)~., data = indatadf2, importance=T, ntrees=10000, proximity=T, type = "classification")

# get data stack
datastack <- stack("stack_nc_i.img")
# convert to brick
databrick <- brick(datastack)
# replace 0 with NA for pixels with 0 values in all layers; this step not really necessary
elkbrick <- calc(databrick, fun = function(x){x[rowSums(x) == 0,] <- NA; x})
# rename layers in stack to match variables for running prediction
layerNames(elkbrick) <- colnames[seq(2,38)]
# predict out to file
predict(elkbrick, elkrf, filename="elkout.img", type = 'class', format="HFA", datatype = 'INT2U', overwrite=T)
