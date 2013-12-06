r <- raster(nrow=50, ncol=50)
r[] <- sample(seq(1:5), ncell(r), replace=T)
for (i in seq(1:5)){
	rtmp <- r
	rtmp[rtmp != i] <- 0
	rc <- clump(rtmp)
	freqrc.df <- data.frame(freq(rc))
	cat(table(freqrc.df$count))
	cat("\n")
	}
