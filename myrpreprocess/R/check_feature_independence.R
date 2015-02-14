# This file will find all the columns which are correlated to each other.
# Numeric columns will be shown based on the correaltaion whereas factor columns will be shown based on the chi square.
# Numeric columns can be converted to factor columns before checking independence of fact columns but these might need to discretize first.

check_whether_features_are_related <- function(X,cor_cutoff = 0.5,alpha = 0.05)
{
  #do not check the correlation or independece against class column which will be the last column.
  X <- X[,-ncol(X)]

  #find correlation.Convert all int columns to numeric
  X <- convertinttonum(X)
  # Find all num cols
  numcols <- findallnumcols(X)

  # Find correlation between columns
  cor_cols <- findcorrelation(X)

  # Consider columns with a certain length as correlated
  ab<- as.matrix(cor_cols>cor_cutoff)
  print(paste("numeric columns with a correlation more than",cor_cutoff))
  print(ab)
  print ("ignore diagonal columns as these values will be 1 always")

  #Now run chi square
  #convert numcols to factors
  #convert the numcols to say 20 factor cols. This is just to get the idea
  X[,numcols] <- EqualFreqDiscretization(X[,numcols],numofbins=20)
  X <- convertnumtofact(X)
  #convert char cols to fact cols
  X <- convertchartofact(X)

  factcols <- findallfactorcols(X)
  #now run the loop. It can be done using sapply and lapply as well but that will make it more complicated although might be faster
  # Here the code is for analysis so it is more about understanding and that is why you will see some verbose output as well. In future, I might test the sapply and l apply as well.
  X <- X[,factcols]
  pvals <- as.data.frame(t(c('NULL','NULL',0.0)))
  colnames(pvals) <- c('col1','col2','pvalue')
  pvals$col1 <- as.character(pvals$col1)
  pvals$col2 <- as.character(pvals$col2)
  pvals$pvalue <- as.numeric(pvals$pvalue)
  cols <- colnames(X)
  nc <- ncol(X) -1
  for (i in 1:nc){
    k <- i+1
    for (j in k:(nc+1)){
        ak <- chisq.test(table(X[,i],X[,j]))
        pdtl <- c(cols[i],cols[j],ak$p.value)
        pvals <- rbind(pvals,pdtl)
      }

    }
  #removefirst val
  pvals <- pvals[-1,]
  #now print
  print("print the p values for column independence.These are order by p values descending to find the columsn which are independent")

  pvals <- pvals[order(pvals[,3],decreasing=FALSE,na.last=TRUE),]
  print(pvals)
}


