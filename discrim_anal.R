# Discriminant analysis to look at how well cover from Amie's points predicts landcover from classification

totcv <- NULL
y <- 0
while (y<100){
	y <- y + 1
	train <- sample(1:1192, 1192-119)
	z <- lda(LC ~ Woody + BLEG + CEG + Tree + Shrub + Herb, ldadf, subset=train)
	predclass <- predict(z, ldadf[-train, ])$class
	ct <- table(ldadf[-train, ]$LC, predclass)
	totcv <- c(totcv, sum(diag(prop.table(ct)))) }
mean(totcv)
