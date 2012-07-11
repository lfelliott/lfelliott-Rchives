#library(gdm)
#library(dismo)
#install.packages(xlsx)
library(xlsx)

# species list this should be the filename (without extension) for the species you have csv files for and you want to run.
specieslist = c("sauger", "redshiner", "catfish")
# tree complexities you want to run
tcs = c(1,2,3)
# learing rates you want to run
lrs = c(0.1, 0.05, 0.01)
df <- NULL
for (i in 1:length(specieslist))
{
# build file name  
  fname <- paste("d:/AWR/", specieslist[i], ".csv", sep="")
#  uncomment next line to actually read in data  
#  moddata <- read.csv(fname)Inital
  for (j in 1:length(lrs))
  {
    for (k in 1:length(tcs))
    {
# uncomment next line to actually run the model
# modres <- gmb.step(moddata, gbm.x=4:61, gbm.y=3, family="bernoulli", tree.complexity=tcs[k], learning.rate=lrs[j], bag.fraction=0.5)
# replace the rnorm statements with variables from the model, with following lines variables are populated with random numbers
      roc <- rnorm(1)
      rocse <- rnorm(1)
      trees <- rnorm(1)
      elapsed <- rnorm(1)
      # roc <- mean(modres$cv.roc.matrix)
      # rocse <- std(modres$cv.roc.matrix)/sqrt(length(modres$cv.roc.matrix))
      # trees <- modres$n.trees
      # elapsed <- modres$gbm.call$elapsed.time.minutes
      df <- rbind(df, c(i, lrs[j], tcs[k], roc, rocse, trees, elapsed))
    }
  }
}
resdf <- data.frame(df)
resdf <- cbind(specieslist[resdf$X1], resdf)
colnames(resdf) <- c("species", "speciesndx", "lr", "tc", "roc", "rocse", "trees", "elapsed")
# Remove species index
resdf$speciesndx <- NULL
write.xlsx(resdf, "resdf.xlsx",sheetName="Sheet1", row.names=FALSE)