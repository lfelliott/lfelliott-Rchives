read.excelclp <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

read.df.excelclp <- function(header=TRUE,...) {
	# Returns a dataframe from clipboard and replaces row names with elements from first column
	temp <- read.table("clipboard",sep="\t",header=header,...)
	tempdf <- data.frame(temp)
	rownames(tempdf) <- tempdf[,1]
	tempdf[,1] <- NULL
	tempdf[is.na(tempdf)] <- 0
	return(tempdf)
}

read.df.accessclp <- function(header=TRUE,...) {
	# Returns a dataframe from clipboard and replaces row names with elements from first column
	temp <- paste(readClipboard(), "\n", sep="")
	write.table(temp,"clipboard",sep="\t",row.names=F,col.names=,...)
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


write.excel <- function(x,row.names=FALSE,col.names=FALSE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
 
write.excel(dat)
dat <- c()
for (i in 1:12){
	for (j in 1:12){
	dat <- c(dat, rep(c(i, j), fixbothm[i,j]))
	}
	}
sampdat <- c()
for (i in 1:12){
pop <- datdf[datdf$map == i,]
samp <-pop[sample(1:nrow(pop), origrowsums[i],replace=F),]
samp
sampdat <- rbind(sampdat, samp)
}

mat <- matrix(nrow=12, ncol=12)
for (i in 1:12){
	for (j in 1:12){
	mat[i,j] <- sum(sampdat[,1] == i & sampdat[,2] == j)
	}
}

build_Bootstrap <- function(longCM, targetrowsums,ndraws){
results <- list(users = NULL, producers = NULL, kappas = NULL, accuracies = NULL)
for (x in 1:ndraws){
# draw from each mapped type and produce a long format results matrix
	sampdat <- c()
	for (i in 1:12){
		pop <- longCM[longCM[,1] == i,]
		samp <- pop[sample(1:nrow(pop), targetrowsums[i], replace=T),]
		sampdat <- rbind(sampdat, samp)
	}
# produce a symmetric matrix (wide format) from above long format matrix
	mat <- matrix(nrow=12, ncol=12)
	for (i in 1:12){
		for (j in 1:12){
		mat[i,j] <- sum(sampdat[,1] == i & sampdat[,2] == j)
		}
	}
	usersacc <- diag(mat)/rowSums(mat)
	prodacc <- diag(mat)/colSums(mat)
	n <- sum(mat)
	d <- diag(mat)
	dsum <- sum(d)
	k <- dsum/n
	expacc <- (sum(rowSums(mat) * colSums(mat))/sum(mat))/sum(mat)
	acc <- sum(diag(mat))/sum(mat)
	k <- (acc - expacc)/(1 - expacc)
	results$users <- cbind(results$users, usersacc)
	results$producers <- cbind(results$producers, prodacc)
	results$kappas <- c(results$kappas, k)
	results$accuracies <- c(results$accuracies, acc)
	}
	return(results)
}


bj <- function(longCM){
results <- list(users = NULL, producers = NULL, kappas = NULL, accuracies = NULL)
for (x in 1:30){
	sampdat <- c()
	for (i in 1:12){
		pop <- longCM[longCM[,1] == i,]
		samp <- pop[sample(1:nrow(pop), origrowsums[i], replace=F),]
		sampdat <- rbind(sampdat, samp)
	}
	mat <- matrix(nrow=12, ncol=12)
	for (i in 1:12){
		for (j in 1:12){
		mat[i,j] <- sum(sampdat[,1] == i & sampdat[,2] == j)
		}
	}
	usersacc <- data.frame(diag(mat)/rowSums(mat))
	prodacc <- data.frame(diag(mat)/colSums(mat))
	print(prodacc)
	n <- sum(mat)
	d <- diag(mat)
	dsum <- sum(d)
	k <- dsum/n
	acc <- sum(diag(mat))/sum(mat)
}
}