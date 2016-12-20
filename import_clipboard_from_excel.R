read.excelclp <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

read.df.excelclp <- function(header=TRUE,...) {
	# Returns a dataframe from clipboard and replaces row names with elements from first column
	temp <- read.table("clipboard",sep="\t",header=header,...)
	tempdf <- data.frame(temp)
	rownames(tempdf) <- tempdf[,1]
	tempdf[,1] <- NULL
	tempdf[is.na(tidaldf)] <- 0
	return(tempdf)
}
 
dat=read.excel()
datdf <- data.frame(dat)
rownames(datdf) <- dat[,1]
datm <- as.matrix(datadf)


write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
 
write.excel(dat)