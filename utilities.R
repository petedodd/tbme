## utilities file
library(data.table)

## utilities
xfun <- function(X,Y,X.sd,Y.sd) X*Y*sqrt((X.sd/X)^2+(Y.sd/Y)^2) #unc for X x Y
see <- function(x,ns=3)formatC(signif(x,ns),big.mark = ",",format='fg') #for reading big numbers
seer <- function(x,ns=3) see(round(x),ns=ns)
ssum <- function(x) sqrt(sum(x^2,na.rm=TRUE)) #NA from 0 denom xfun

## for making tables from mid point & SD
## nc is the number of category cols at start
tablemaker <- function(DT,DT.sd,nc=1){
    cnmz <- names(DT)[1:nc]
    rgs <- DT[,..cnmz] #category names
    nmz <- names(DT)[(nc+1):ncol(DT)]
    nmz.sd <- names(DT.sd)[(nc+1):ncol(DT.sd)]
    M <- as.matrix(DT[,..nmz]); M.sd <- as.matrix(DT.sd[,..nmz.sd]);
    H <- M + 1.96*M.sd; L <- M - 1.96*M.sd;
    L[L<0] <- 0 #safety
    M <- seer(M); L <- seer(L); H <- seer(H)
    A <- matrix(paste0(M," (",L," - ",H,")"),nrow=nrow(M),ncol=ncol(M))
    colnames(A) <- nmz
    A <- as.data.frame(A)
    A <- cbind(rgs,A)
    return(A)
}
