# From http://holliekitson.com/projects/blog_posts/r_raster/raster_blog.html

library(rgdal)
library(raster)

# read in the raster
nlcd <- raster('nlcd.img')

# read in regions.shp
# I only tried this with individual regions, so I did a dissolve on the field of interest with multifeature checked
reg <- readOGR('.', 'regions')

#making the names match with what's in the blog post
r1 <- nlcd
f1sm <- reg

# Extract the values of the landcover raster for each zone. This outputs a list of landuse categories for each region and 'tables' the results. 
#ext<-raster::extract(r1, f1sm, method='simple')
ext<-extract(r1, f1sm, method='simple', fun=function(x) table(x))



# I'm sure there's a better way to do this but create table from lists
tab1<-do.call(rbind, lapply(lapply(ext, unlist), "[", unique(unlist(c(sapply(ext,names))))))

# Assign region names
rownames(tab1)<-f1sm$NAME_2

# We're interested in area so multiply the counts by the grid cell dimensions 
cellsize <- 30
tab1<-data.frame(apply(tab1, 2, function(x) cellsize * cellsize * x))
tab1[is.na(tab1)] <- 0