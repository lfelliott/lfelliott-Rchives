specieslist = c("sauger", "redshiner", "catfish")
tcs = c(1,2,3)
lrs = c(0.1, 0.05, 0.01)
df <- NULL
for (i in 1:length(specieslist))
{
  fname <- paste("d:/AWR/", specieslist[i], ".csv", sep="")
  print(fname)
#  moddata <- read.csv(fname)Inital
  for (j in 1:length(lrs))
  {
    for (k in 1:length(tcs))
    {
# put code to run model here
# modres <- gmb.step(moddata, gbm.x=4:61, gbm.y=3, family="bernoulli", tree.complexity=tcs[k], learning.rate=lrs[j], bag.fraction=0.5)
# roc <- mean(modres$cv.roc.matrix)
# rocse <- std(modres$cv.roc.matrix)/sqrt(length(modres$cv.roc.matrix))
# trees <- modres$n.trees
# elapsed <- modres$gbm.call$elapsed.time.minutes
# replace the rnorm statements with variables from the model
      roc <- rnorm(1)
      rocse <- rnorm(1)
      trees <- rnorm(1)
      elapsed <- rnorm(1)
      df <- rbind(df, c(i, lrs[j], tcs[k], roc, rocse, trees, elapsed))
    }
  }
}
resdf <- data.frame(df)
resdf <- cbind(resdf, specieslist[resdf$X1])
colnames(resdf) <- c("speciesndx", "lr", "tc", "roc", "rocse", "trees", "elapsed", "species")
write.csv(resdf, "resdf.csv", row.names=FALSE)