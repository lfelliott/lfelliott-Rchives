# Script to run boosted regression tree on fish data and apply predictions from model to full stream layer in chunks to reduce memory consumption
# Lee Elliott 20120706

library(gbm)
library(dismo)

redshiner<- read.csv("D:/AWR/redshiner.csv")

# fit the model
redshiner.tc1.lr1 <- gbm.step(data=redshiner, gbm.x= 4:61, gbm.y=3, family = "bernoulli", tree.complexity = 1, learning.rate = 0.1, bag.fraction =0.5)

streams<-read.delim("D:/AWR/strms4.txt", header=TRUE, sep="\t")

# get predictions for chunks of streams file

chunks <- 10
numrows <- dim(streams)[1]
jump <- round(numrows/chunks) + 1
for (i in seq(from=1, to=numrows, by=jump))
	{
	start <- i
	end <- start + jump - 1
	if (end > numrows) end <- numrows
	stream_subset <- streams[start:end,]
	preds <- predict.gbm(redshiner.tc1.lr1, stream_subset, n.trees = redshiner.tc1.lr1$gbm.call$best.trees, type = "response")
	if (start == 1)
		{
		redshiner.df <- data.frame(stream_subset$HUC4_COMID, preds)
		}
	else
		{
		temp.df <- data.frame(stream_subset$HUC4_COMID, preds)
		redshiner.df <- rbind(redshiner.df, data.frame(stream_subset$HUC4_COMID, preds))
		}
	}

head(redshiner.df)  # look at sauger.df
colnames(redshiner.df)[1]<- "HUC4_COMID"  # change name of Seg_ID
library(foreign)
write.dbf(redshiner.df, file=" D:/AWR/Out/redshiner.tc1.lr1.dbf")  # write results of predictive model 
# get residuals to see the fit, over-fit and under-fit for each modeled stream segment
redshiner.resid <- residuals(redshiner.tc1.lr1)  

# get residuals from model
redshiner.df <- data.frame(redsiner$HUC4_COMID, redshiner.resid)    

# dataframe -- use Seg_ID from input and residuals from object
head(redshiner.df)
colnames(RS.df) <- c("HUC4_COMID", "resid")
library(foreign)
write.dbf(redshiner.df, file = " D:/AWR/Out/redshiner_residuals.dbf")

# To change both column names at once . . .
# colnames(PD.df) <- c("Seg_ID", "resid")

# Residuals . . .
#names(Pdace.tc5.lr0005)
#residuals(Pdace.tc5.lr0005)

