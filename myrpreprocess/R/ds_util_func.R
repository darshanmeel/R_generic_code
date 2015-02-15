# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)

# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.

# This will runks test but only on numeric column and will return the columns which have value larger than given pval
# You might get warnings when you run this as the ks.test might return warnings.
kstest <- function(X,pval = 0.05)
{
  colclasses <- findclassofcolumns(X)

  nc <- ncol(X)
  dfcolnames <- colnames(X)
  #numcols <- ifelse(colclasses=='numeric',TRUE,(ifelse(colclasses=='integer',TRUE,FALSE)))
  numcols <- findallnumcols(X)
  numcols <- dfcolnames[numcols]
  print(numcols)
  clses <- levels(X[,nc])
  nclses <- length(clses)
  for (i  in 1:(nclses-1)){
    a <- X[X[,nc]==clses[i],numcols]
    print (clses[i])
    print(nrow(a))
    ab <- lapply(numcols,function(x) ks.test(a[,x],pnorm))
    pvalues <- sapply(seq(1:length(numcols)),function(x) ab[[x]]$p.value)
    print(pvalues)
    print (dfcolnames[numcols[pvalues>pval]])
    for (j in (i+1):nclses){
      print (clses[j])
      b <- X[X[,nc]==clses[j],numcols]
      print(nrow(b))
      ab <- lapply(numcols,function(x) ks.test(b[,x],pnorm))
      pvalues <- sapply(seq(1:length(numcols)),function(x) ab[[x]]$p.value)
      print(pvalues)

      ab <- lapply(numcols,function(x) ks.test(a[,x],b[,x]))
      pvalues <- sapply(seq(1:length(numcols)),function(x) ab[[x]]$p.value)
      print(pvalues)
      print (dfcolnames[numcols[pvalues>pval]])

    }
  }
}

