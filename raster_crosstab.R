library(raster)
r1 <- raster("lc_segap")
r2 <- raster("lc_segap")
r_xtab <- crosstab(r1, r2)
write.csv(r_xtab, file='r_xtab.csv')
# produce a long format comparison of crosstab
r_xtabdf <- as.data.frame(r_xtab)
colnames(r_xtabdf) <- c("r1", "r2", "freq")

