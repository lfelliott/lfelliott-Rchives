library(rgdal)
setwd("d:/r/spatial")
tracks <- readOGR("peckranch.gpx", layer='tracks')
writeOGR(tracks, getwd(), "peckranch", driver="ESRI Shapefile")

#proj4string(PointsAsFrame) <- CRS("+proj=longlat +datum=WGS84")

#The variants are as described in the readOGR help file, that is layer=
#"waypoints", "tracks", "routes", "track_points", or "route_points". 