# Modified function ordicoeno from Biodiversity R to list species and the colors of the curves for each species. Also, modified
# maximum y to better reflect maximum possible in species response curves.
#
# to get a smaller subset of species, make a list of species 
# to remove species
# drop.species = c("ANGE", "SCSC")
# x.subset <- x[,-which(names(x) %in% drop.species)]
# or, 
# x.subset <- subset(x, select=-c(ANGE, SCSC))
# to keep the species
# keep.species = c("BRIN2", "ANGE", "POPR", "HESP11", "SCSC", "SPHE", "SYER", "PEAR6", "SOCA6", "HEMA2", "ROAR3", "PAVI2", "BODA2", "KOMA")
# x.subset <- x[, keep.species]


ordicoeno3 <- function (x, ordiplot, axis = 1, ...) 
{
    if (!require(mgcv)) {
        stop("Requires package mgcv")
    }
	species <- NULL
	linecolor <- NULL
	linetype <- NULL
    ordiscore <- scores(ordiplot, display = "sites")[, axis]
    original <- cbind(x, ordiscore)
    sorted <- original
    seq <- order(ordiscore)
    sorted[1:nrow(original), ] <- original[seq, ]
    edfs <- array(NA, dim = c(ncol(x)))
    names(edfs) <- colnames(x)
#   palette(rainbow(ncol(x)))
	colorsloaded <- rep(palette(), 20)
    newdata <- data.frame(seq(min(sorted$ordiscore), max(sorted$ordiscore), 
        length = 1000))
    colnames(newdata) <- "ordiscore"


	lmax <- NULL
	lmaxscore <- NULL
	gamresults <- NULL
	gamresults2 <- NULL
	for (i2 in 1:ncol(x)){
		lgamresult <- gam(sorted[, i2] ~ s(ordiscore), data = sorted)
		gamresults[[i2]] <- lgamresult
		lgamresult2 <- predict(lgamresult, newdata)
		gamresults2[[i2]] <- lgamresult2
		lmax <- c(lmax, max(lgamresult2))
		lmaxscore <- c(lmaxscore,((which(lgamresult2 == max(lgamresult2), arr.ind=T)[1]/1000)*(abs(min(sorted$ordiscore))+max(sorted$ordiscore))) + min(sorted$ordiscore))
		}
	curvemax = max(lmax)

	cat("Species\t\tColor\t\t\tMaximum Value\tScore of Max\n")
	for (i in 1:ncol(x)) {
        gamresult <- gamresults[[i]]
        gamresult2 <- gamresults2[[i]]
        edfs[i] <- summary(gamresult)$edf
		if (i == 1) plot(newdata$ordiscore, gamresult2, type = "l", ylim = c(0,curvemax), col = 1, pch = 1, xlab = "site score on ordination axis", ylab = "species values", ...)
		else points(newdata$ordiscore, gamresult2, type = "l", pch = 19, col = i, ...)
		if (nchar(colorsloaded[i]) > 5) cat(colnames(original)[i], "\t\t", colorsloaded[i],"\t\t", max(gamresult2), "\t", lmaxscore[i], "\n") else cat(colnames(original)[i], "\t\t", colorsloaded[i],"\t\t\t", max(gamresult2), "\t", lmaxscore[i],"\n")
		species <- c(species, colnames(original)[i])
		linecolor <- c(linecolor, i)
		linetype <- c(linetype, 1)
    }
	legend(-0.2, curvemax, species, lty=linetype, col=linecolor)
	palette("default")
    cat("\nedfs from GAM models for each species...\n")
    return(edfs)
}