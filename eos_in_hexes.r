eos <- readOGR("eos.shp", layer="eos")
hexes <- readOGR("hexes.shp", layer="hexes")
counts <- data.frame(matrix(NA, nrow = 100, ncol = 2)
colnames(counts) <- c("specCnt", "occCnt")
hexids <- hexes$OBJECTID
for (i in seq(1, 100))
	{
	hexsamples <- sample(hexids, 1446, replace=F)
	selectedHexes <- hexes[hexes$OBJECTID %in% hexsamples,]
	j <- over(eos, selectedHexes)
	foundEos <- j[!is.na(j$OBJECTID),]
	foundEos <- strtoi(rownames(foundEos))
	selectedEos <- eos[eos$OBJECTID %in% foundEos,]
	species <- selectedEos$SCIENTIFIC
	speciesCount <- length(unique(species))
	occCount <- length(selectedEos)
	counts$specCnt[i] <- speciesCount
	counts$occCnt[i] <- occCount
	}
colMeans(counts)