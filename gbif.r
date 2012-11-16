library(dismo)
library(XML)
# get GBIF data with function: 
myrger <- gbif("Myricaria", "germanica", geo = T)   
# check: 
str(myrger)   
# plot occurrences: 
library(maptools) 
data(wrld_simpl) 
plot(wrld_simpl, col = "light yellow", axes = T) 
points(myrger$lon, myrger$lat, col = "red", cex = 0.5) 
text(-140, -50, "MYRICARIA\nGERMANICA")