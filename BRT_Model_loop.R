library(gbm)
library(dismo)
#install.packages(xlsx)
library(xlsx)

# species list this should be the filename (without extension) for the species you have csv files for and you want to run.
specieslist = c("blcktlshnr")
# tree complexities you want to run
tcs = c(1,2)
# learing rates you want to run
lrs = c(0.1, 0.05)
df <- NULL
modelindex <- 0
modellist <- list()
for (i in 1:length(specieslist))
{
# build file name  
  fname <- paste("d:/R/BRT/Lee_test/", specieslist[i], ".csv", sep="")
#  fname <- paste("d:/AWR/", specieslist[i], ".csv", sep="")
#  uncomment next line to actually read in data  
  moddata <- read.csv(fname)
  for (j in 1:length(lrs))
  {
    for (k in 1:length(tcs))
    {
	modres <- NULL
	# uncomment next line to actually run the model
	modres <- try(gbm.step(moddata, gbm.x=4:61, gbm.y=3, family="bernoulli", tree.complexity=tcs[k], learning.rate=lrs[j], bag.fraction=0.5))
	# replace the rnorm statements with variables from the model, with following lines variables are populated with random numbers
	if (class(modres) == "gbm")
		{
		modelindex <- modelindex + 1
		#roc <- rnorm(1)
		#rocse <- rnorm(1)
		#trees <- rnorm(1)
		#elapsed <- rnorm(1)
       roc <- mean(modres$cv.roc.matrix)
       rocse <- sd(modres$cv.roc.matrix)/sqrt(length(modres$cv.roc.matrix))
       trees <- modres$n.trees
       elapsed <- modres$gbm.call$elapsed.time.minutes
		df <- rbind(df, c(i, lrs[j], tcs[k], roc, rocse, trees, elapsed, modelindex))
		modellist[[modelindex]] <- modres
		}
    }
  }
}
save(modellist, file="modellist.RData")
resdf <- data.frame(df)
resdf <- cbind(specieslist[resdf$X1], resdf)
colnames(resdf) <- c("species", "speciesndx", "lr", "tc", "roc", "rocse", "trees", "elapsed", "modelindex")
# Remove species index
resdf$speciesndx <- NULL
write.xlsx(resdf, "resdf.xlsx",sheetName="Sheet1", row.names=FALSE)
maxrocs <- which(resdf$roc[resdf$species==specieslist[i]] == max(resdf$roc[resdf$species==specieslist[i]]))
	