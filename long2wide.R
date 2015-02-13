long2wide <- function(x, colvar, rowid){
return(reshape(data.frame(x), timevar = colvar, idvar=rowid, direction='wide'))
}