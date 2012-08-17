# Modified function ordicoeno from Biodiversity R to list species and the colors of the curves for each species. Also, modified
# maximum y to better reflect maximum possible in species response curves.

ordicoeno2 <- function (x, ordiplot, axis = 1, ...) 
{
    if (!require(mgcv)) {
        stop("Requires package mgcv")
    }
	colorsloaded <- rep(palette(), 20)
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
    palette(rainbow(ncol(x)))
	gamresult <- gam(sorted[, 1] ~ s(ordiscore), data = sorted)
    edfs[1] <- summary(gamresult)$edf
    newdata <- data.frame(seq(min(sorted$ordiscore), max(sorted$ordiscore), 
        length = 1000))
    colnames(newdata) <- "ordiscore"
	lmax <- NULL
	for (i2 in 1:ncol(x)){
		lgamresult <- gam(sorted[, i2] ~ s(ordiscore), data = sorted)
		lgamresult2 <- predict(lgamresult, newdata)
		lmax <- c(lmax, max(lgamresult2))
		}
	curvemax = max(lmax)

    gamresult2 <- predict(gamresult, newdata)
    plot(newdata$ordiscore, gamresult2, type = "l", ylim = c(0, 
        curvemax), col = 1, pch = 1, xlab = "site score on ordination axis", 
        ylab = "species values", ...)
	cat("Species\t\t\tColor\t\t\tMaximum Value\n")
	cat(colnames(original)[1], "\t\t\t", colorsloaded[1], "\t\t\t", max(gamresult2), "\n")
	species <- c(species, colnames(original)[1])
	linecolor <- c(linecolor, 1)
	linetype <- c(linetype, 1)
	for (i in 2:ncol(x)) {
        gamresult <- gam(sorted[, i] ~ s(ordiscore), data = sorted)
        gamresult2 <- predict(gamresult, newdata)
        edfs[i] <- summary(gamresult)$edf
        points(newdata$ordiscore, gamresult2, type = "l", pch = 19, 
            col = i, ...)
		cat(colnames(original)[i], "\t", colorsloaded[i],"\t", max(gamresult2), "\n")
		species <- c(species, colnames(original)[i])
		linecolor <- c(linecolor, i)
		linetype <- c(linetype, 1)
    }
	legend(-0.2, curvemax, species, lty=linetype, col=linecolor)
	palette("default")
    cat("\nedfs from GAM models for each species...\n")
    return(edfs)
}