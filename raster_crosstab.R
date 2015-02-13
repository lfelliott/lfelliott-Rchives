library(raster)
r1 <- raster("anderson.img", format = "HFA")
r2 <- raster("evt.img", format = "HFA")
r_xtab <- crosstab(r1, r2)
# produce a long format comparison of crosstab
r_xtabdf <- as.data.frame(r_xtab)
colnames(r_xtabdf) <- c("r1", "r2", "freq")
write.csv(r_xtabdf, file='anderson_x_evt.csv')

r_xtablng <- reshape(r_xtabdf, direction="long", v.names="freq", idvar="r1", timevar="r2)
