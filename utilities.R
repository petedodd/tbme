## utilities file
library(data.table)

## utilities
xfun <- function(X,Y,X.sd,Y.sd) X*Y*sqrt((X.sd/X)^2+(Y.sd/Y)^2) #unc for X x Y
see <- function(x,ns=3)formatC(signif(x,ns),big.mark = ",",format='fg') #for reading big numbers
seer <- function(x,ns=3) see(round(x),ns=ns)
ssum <- function(x) sqrt(sum(x^2))
