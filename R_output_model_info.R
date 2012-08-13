# Script to output parameters from model into excel spreadsheet
# Change and add parameters and column names (second to last line)
# Lee Elliott 20120810


library(xlsx)

working_dir <- "D:/AWR/directory_of_models"
setwd(working_dir)
results <- NULL
species <- NULL
fish_list <- list.files(pattern = "*.RData")
for (fish_data in fish_list)
	{
	load(fish_data)
	fish_name <- strsplit(fish_data, ".", fixed = TRUE)[[1]][1]
	# If the model loaded in the load() above is not called modres then you need to move the model into modres. For instance, if it's called the fishes name (as in the string fish_name)
	# load it into modres using 
	# modres <- get(fish_name)
	if (class(modres) == "gbm")
		{
		# Get parameters you want to save
		roc <- mean(modres$cv.roc.matrix)
		rocse <- sd(modres$cv.roc.matrix)/sqrt(length(modres$cv.roc.matrix))
		trees <- modres$n.trees
		elapsed <- modres$gbm.call$elapsed.time.minutes
		# Build data. These are not dataframes so you can't mix strings and numbers, if you have some strings from the model put them another data set and you can join them in the cbind step later
		results <- rbind(results, c(roc, rocse, trees, elapsed))
		species <- rbind(species, fish_name)
		}
	rm(modres)
	# To delete all the gbm models in the workspace before the next iteration, next 7 lines of code
	modelsinws <- NULL
	objsinws <- ls()
	for (obj in objsinws)
		{
		if (class(get(obj)) == "gbm") modelsinws <- c(modelsinws, obj)
		}
	rm(list = modelsinws)
	cat(".")
	}
resdf <- data.frame(results)
resdf <- cbind(species, resdf)
colnames(resdf) <- c("species", "roc", "rocse", "trees", "elapsed")
write.xlsx(resdf, "resdf.xlsx",sheetName="Sheet1", row.names=FALSE)	
