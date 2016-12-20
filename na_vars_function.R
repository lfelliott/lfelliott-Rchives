na_vars <- function(x){
	for (Var in names(x)){
		missing <- sum(is.na(x[,Var]))
		if (missing > 0){
			print(c(Var,missing))
		}
	}
}

# Could also use sapply(in_df, function(x) sum(is.na(x)))
# or apply(is.na(in_df),2,sum)
# or colSums(is.na(in_df))