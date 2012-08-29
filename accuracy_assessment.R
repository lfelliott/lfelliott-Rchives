# Functions from Rossiter, D. G. 2004. Technical Note: Statistical methods for accuracy assesment of classified thematic maps

kappa <- function(CM) {
#convert both data frames and vectors to matrices
cmx<-as.matrix(CM)
#try to convert a vector to a square matrix
if (ncol(cmx) == 1)
cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
nr<-nrow(cmx); nc<-ncol(cmx)
if (nr != nc)
{ print("Error: matrix is not square"); break }
n<-sum(cmx)
d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
th1v<-((th1*(1-th1))/n)
csum<-apply(cmx,2,sum); rsum<-apply(cmx,1,sum)
ua<-d/rsum; pa<-d/csum
th2 <- sum(rsum*csum) / n^2; kh <- (th1-th2)/(1-th2)
th3 <- sum( (csum + rsum) * d ) / n^2;
th4 <- 0; for (i in 1:nr) for (j in 1:nc)
th4 <- th4 + (cmx[i,j] * ((csum[i] + rsum[j])^2));
th4 <- th4 / n^3;
th1c <- 1 - th1; th2c <- 1 - th2;
khv <- 1/n *
( ( ( th1 * th1c ) / th2c^2 )
+ ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 )
+ ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 )
)
#per-class kappa, user’s accuracy...
p <- cmx/n; uap <- apply(p,1,sum); pap <- apply(p,2,sum); dp<-diag(p);
kpu <- (dp/uap - pap)/(1 - pap);
#...and its variance
t1 <- uap-dp; t2 <- (pap*uap)-dp; t3 <- dp*(1 - uap - pap + dp);
kpuv <- ( (t1/(uap^3 * (1-pap)^3)) * ((t1*t2) + t3) )/n;
#per-class kappa, producer’s reliability...
kpp <- (dp/pap - uap)/(1 - uap);
#...and its variance
t1 <- (pap-dp);
kppv <- ( (t1/(pap^3 * (1-uap)^3)) * ((t1*t2) + t3) )/n;
#return all statistics as a list
list(sum.n=n, sum.naive=th1, sum.var=th1v, sum.kappa=kh, sum.kvar=khv,
user.naive=ua, prod.naive=pa,
user.kappa=kpu, user.kvar=kpuv, prod.kappa=kpp, prod.kvar=kppv)
}


summary.kappa <- function(kappa, alpha=0.05) {
ciw<-function(var, n) {
qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
}
print(paste("Number of observations:", kappa$sum.n), quote=F)
print("Summary of naive statistics", quote=F)
print(paste(
"Overall accuracy, stdev, CV%:",
round(kappa$sum.naive, 4), ",",
round(sqrt(kappa$sum.var), 4), ",",
round((sqrt(kappa$sum.var)/kappa$sum.naive)*1000,0)/10),
quote=F)
w<-ciw(kappa$sum.var, kappa$sum.n)
print(paste(
round((1-alpha)*100,0),"% confidence limits for accuracy:",
round((kappa$sum.naive-w),4),"...",
round((kappa$sum.naive+w),4)), quote=F, sep="")
print("User’s accuracy", quote=F); print(round(kappa$user.naive,4));
print("Producer’s reliability:", quote=F); print(round(kappa$prod.naive,4));
print("Summary of kappa statistics", quote=F)
print(paste("Overall kappa, stdev, & CV%:",
round(kappa$sum.kappa,4), ",",
round(sqrt(kappa$sum.kvar),4), ",",
round((sqrt(kappa$sum.kvar)/kappa$sum.kappa)*1000,0)/10), quote=F)
w<-ciw(kappa$sum.kvar, kappa$sum.n)
print(paste(
round((1-alpha)*100,0),"% confidence limits for kappa:",
round((kappa$sum.kappa-w),4),"...",
round((kappa$sum.kappa+w),4)), quote=F, sep="")
print("Per-class kappa, stdev, & CV%, for user’s accuracy:", quote=F)
print(round(kappa$user.kappa,4), quote=F);
print(round(sqrt(kappa$user.kvar),4), quote=F);
print(round((sqrt(kappa$user.kvar)/kappa$user.kappa)*1000,0)/10, quote=F);
print("Per-class kappa, stdev, & CV%, for producer’s reliability:", quote=F)
print(round(kappa$prod.kappa,4), quote=F);
print(round(sqrt(kappa$prod.kvar),4), quote=F);
print(round((sqrt(kappa$prod.kvar)/kappa$prod.kappa)*1000,0)/10, quote=F);
}

# Compute tau. CM is the confusion matrix and P is a vector with the prior probability of each class, if none given then equal priors are given.

tau <- function(CM, P) {
#convert both data frames and vectors to matrices
cmx<-as.matrix(CM)
#try to convert a vector to a square matrix
if (ncol(cmx) == 1)
cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
nr<-nrow(cmx); nc<-ncol(cmx)
if (nr != nc)
{ print("Error: matrix is not square"); break }
#check P and create if necessary
if (missing(P))
P<-rep(1/nr, nr)
if (length(P) != nc)
{ print("Error: prior probabilities vector has wrong length"); break }
if (abs(1-sum(P)) > 0.0001)
{ print("Error: prior probabilities must sum to 1"); break }
n<-sum(cmx)
d<-diag(cmx); dsum<-sum(d); th1<-dsum/n
csum<-apply(cmx,2,sum); th2<-(csum%*%P)/n
tau<-(th1-th2)/(1-th2);
th3<-sum( (csum + (P*n)) * diag(cmx) ) / n^2;
rsum<-apply(cmx,1,sum)
ua<-d/rsum; pa<-d/csum
th4 <- 0; for (i in 1:nr) for (j in 1:nc)
th4 <- th4 + (cmx[i,j] * ((csum[i] + P[j]*n)^2));
th4 <- th4 / n^3;
th1c <- 1 - th1; th2c <- 1 - th2;
tv <- 1/n *
( ( ( th1 * th1c ) / th2c^2 )
+ ( ( 2 * th1c * ((2*th1*th2) - th3) ) / th2c^3 )
+ ( ( th1c^2 * ( th4 - (4 * th2^2 ) ) ) / th2c^4 )
)
list(prior=P, obs=rsum, ref=csum, n=n, tau=tau, tvar=tv, coeff=c(th1, th2, th3, th4))
}

# Summarize tau
summary.tau <- function(tau, alpha=0.05) {
ciw<-function(var, n) {
qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
}
print(paste("Number of observations:", tau$n), quote=F)
print("Prior class probabilities:", quote=F)
print(tau$prior, quote=F)
print("Observed class proportions:", quote=F)
print(round(tau$obs/tau$n,4), quote=F)
print("Reference class proportions:", quote=F)
print(round(tau$ref/tau$n,4), quote=F)
print(paste("Tau, stdev, & CV%:",
round(tau$tau,4), ",",
round(sqrt(tau$tvar),4), ",",
round((sqrt(tau$tvar)/tau$tau)*1000,0)/10), quote=F)
w<-ciw(tau$tvar, tau$n)
print(paste(round((1-alpha)*100,0),"% confidence limits for tau:",
round((tau$tau-w),4), "...", round((tau$tau+w),4), sep=""), quote=F)
}