# Script to apply predictions from boosted regression tree on fish data to models held in folder
# Originally ran into problems with the field names being lower case in the data used to build the model and upper case in the streams set. used ?toupper to correct
# Lee Elliott 20120810

library(gbm)
library(dismo)
library(foreign)

working_dir <- "D:/AWR/directory_of_models"
streams<-read.delim("D:/AWR/strms4.txt", header=TRUE, sep="\t")
setwd(working_dir)

chunks <- 10
numrows <- dim(streams)[1]
jump <- round(numrows/chunks) + 1

fish_list <- list.files(pattern = "*.RData")
for (fish_data in fish_list)
	{
	load(fish_data)
	fish_name <- strsplit(fish_data, ".", fixed = TRUE)[[1]][1]
	# get predictions for chunks of streams file
	for (i in seq(from=1, to=numrows, by=jump))
		{
		start <- i
		end <- start + jump - 1
		if (end > numrows) end <- numrows
		stream_subset <- streams[start:end,]
		preds <- predict.gbm(modres, stream_subset, n.trees = modres$gbm.call$best.trees, type = "response")
		if (start == 1)
			{
			fishresult.df <- data.frame(stream_subset$HUC4_COMID, preds)
			}
		else
			{
			temp.df <- data.frame(stream_subset$HUC4_COMID, preds)
			fishresult.df <- rbind(fishresult.df, data.frame(stream_subset$HUC4_COMID, preds))
			}
		}
	colnames(fishresult.df)[1] <- "HUC4_COMID"
	dbfname <- paste(working_dir, "/", fish_name, ".dbf", sep = "")
	write.dbf(fishresult.df, file = dbfname)
	rm(modres)
	}
	
