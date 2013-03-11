library(raster)
r1 <- raster(ncol=10, nrow=10)
r2 <- r1
values(r1) <- sample(1:5, ncell(r1), replace=T)
values(r2) <- sample(2:6, ncell(r2), replace = T)
s <- stack(r1, r2)
df <- as.data.frame(s)
table(df)
freq(s, merge=T)