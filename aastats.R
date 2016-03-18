aastats <- function(mat){
# Produce table using mat <- table(pred, actual) so actual are the columns and predicted are rows
# calculate user's accuracy
ua <- diag(mat)/rowSums(mat)
# Errors of Commision
eoc <- 1 - ua
# Producer's Accuracy
pa <- diag(mat)/colSums(mat)
# Errors of Omission
eoo <- 1 - pa
# Observed Accuracy
acc <- sum(diag(mat))/sum(mat)
# Expected Accuracy
# = ((Marginal frequency of actual * marginal frequency of predicted)/total observations) summed over all classes and divdied by total # of observations
rowmarg <- rowSums(mat)
colmarg <- colSums(mat)
expacc <-  sum(rowmarg * colmarg)/sum(mat)
expacc <- expacc/sum(mat)
# Kappa
kappa <- (acc - expacc)/(1-expacc)
print(kappa)
}
