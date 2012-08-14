# Script to output residuals from models
# Lee Elliott 20120814

install.packages("svMisc")
library(svMisc)
library(foreign)

working_dir <- "D:/AWR/directory_of_models_and_sample_csv"
output_residuals_dir <- "D:/AWR/directory_of_models_and_sample_csv/ouput/"
setwd(working_dir)

fish_list <- list.files(pattern = "*.RData")
list_length <- length(fish_list)
i = 1

for (fish_data in fish_list)
	{
	load(fish_data)
	fish_name <- strsplit(fish_data, ".", fixed = TRUE)[[1]][1]
	species_stream_file <- paste(fish_name, ".csv", sep = "")
	species_data <- read.csv(species_stream_file)
	dbfname <- paste(output_residuals_dir, fish_name, ".dbf", sep = "")
	modres <- get(fish_name)
	if (class(modres) == "gbm")
		{
		species_residuals <- residuals(modres)
		residualsdf <- data.frame(species_data$HUC4_COMID, species_residuals)
		colnames(residualsdf) <- c("HUC4_COMID", "resids")
		write.dbf(residualsdf, file = dbfname)
		}
	# To delete all the gbm models in the workspace before the next iteration, next 7 lines of code
	modelsinws <- NULL
	objsinws <- ls()
	for (obj in objsinws)
		{
		if (class(get(obj)) == "gbm") modelsinws <- c(modelsinws, obj)
		}
	rm(list = modelsinws)
	progress(i, list_length)
	i = i + 1
	}
