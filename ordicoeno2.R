# Modified function ordicoeno from Biodiversity R to list species and the colors of the curves for each species. Also, modified
# maximum y to better reflect maximum possible in species response curves.
#
# to get a smaller subset of species, make a list of species 
# to remove species
# drop.species = c("ANGE", "SCSC")
# x.subset <- x[,-which(names(x) %in% drop.species)]
# or
# x.subset <- subset(x, select=-c(ANGE, SCSC))
# to keep the species
# keep.species = c("BRIN2", "ANGE", "POPR", "HESP11", "SCSC", "SPHE", "SYER", "PEAR6", "SOCA6", "HEMA2", "ROAR3", "HEMA2", "PAVI2", "BODA2", "KOMA")
# x.subset <- x[, keep.species]


ordicoeno2 <- function (x, ordiplot, axis = 1, ...) 
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
	gamresult <- gam(sorted[, 1] ~ s(ordiscore), data = sorted)
    edfs[1] <- summary(gamresult)$edf
    newdata <- data.frame(seq(min(sorted$ordiscore), max(sorted$ordiscore), 
        length = 1000))
    colnames(newdata) <- "ordiscore"
	lmax <- NULL
	lmaxscore <- NULL
# run through all the models to get the overall max value for plotting and the axis score where the max occurred stored in lmaxscore[]
	for (i2 in 1:ncol(x)){
		lgamresult <- gam(sorted[, i2] ~ s(ordiscore), data = sorted)
		lgamresult2 <- predict(lgamresult, newdata)
		lmax <- c(lmax, max(lgamresult2))
# calculate axis score where max occurred
		lmaxscore <- c(lmaxscore,((which(lgamresult2 == max(lgamresult2), arr.ind=T)[1]/1000)*(abs(min(sorted$ordiscore))+max(sorted$ordiscore))) + min(sorted$ordiscore))
		}
	curvemax = max(lmax)

    gamresult2 <- predict(gamresult, newdata)
    plot(newdata$ordiscore, gamresult2, type = "l", ylim = c(0, 
        curvemax), col = 1, pch = 1, xlab = "site score on ordination axis", 
        ylab = "species values", ...)
	cat("Species\t\tColor\t\t\tMaximum Value\tScore of Max\n")
	if (nchar(colorsloaded[1]) > 5) cat(colnames(original)[1], "\t\t", colorsloaded[1],"\t\t", max(gamresult2), "\t", lmaxscore[1], "\n") else cat(colnames(original)[1], "\t\t", colorsloaded[1],"\t\t\t", max(gamresult2), "\t", lmaxscore[1],"\n")
	species <- c(species, colnames(original)[1])
	linecolor <- c(linecolor, 1)
	linetype <- c(linetype, 1)
	
	for (i in 2:ncol(x)) {
        gamresult <- gam(sorted[, i] ~ s(ordiscore), data = sorted)
        gamresult2 <- predict(gamresult, newdata)
        edfs[i] <- summary(gamresult)$edf
        points(newdata$ordiscore, gamresult2, type = "l", pch = 19, 
            col = i, ...)
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