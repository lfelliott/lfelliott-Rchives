read.mat.excelclp <- function(header=TRUE,...) {
    # Returns a dataframe from clipboard and replaces row names with elements from first column
    temp <- read.table("clipboard",sep="\t",header=header,...)
    tempdf <- data.frame(temp)
    rownames(tempdf) <- tempdf[,1]
    tempdf[,1] <- NULL
    tempdf[is.na(tempdf)] <- 0
	rownames(tempdf) <- seq(1:dim(tempdf)[1])
	names(tempdf) <- seq(1:dim(tempdf)[1])
	tempmat <- as.matrix(tempdf)
    return(tempmat)
	}