#'findclasstypeofeach columns
#'colclasses <- findclassofcolumns(X)
findclassofcolumns <- function(X)
{
  cls <- sapply(X,class)
  cls
}
#'findallfactorcols columns
#'factorcols <- findallfactorcols(X)
findallfactorcols <- function(X)
{
  allcolclasses <- findclassofcolumns(X)
  #print (allcolclasses)
  # This will return position. You can get the names if you want from the colnames
  factorcols <- which(allcolclasses %in% "factor")
  factorcols
}

#'findallnumcols columns
#'numcols <- findallnumcols(X)
findallnumcols <- function(X)
{
  allcolclasses <- findclassofcolumns(X)
  #print (allcolclasses)
  # This will return position. You can get the names if you want from the colnames
  numcols <- which(allcolclasses %in% "numeric")
  numcols
}

#'findintcols columns
#'intcols <- findintcols(X)
findintcols <- function(X)
{
  allcolclasses <- findclassofcolumns(X)
  #print (allcolclasses)
  # This will return position. You can get the names if you want from the colnames
  intcols <- which(allcolclasses %in% "integer")
  intcols
}

#'findcharcols columns
#'charcols <- findcharcols(X)
findcharcols <- function(X)
{
  allcolclasses <- findclassofcolumns(X)
  #print (allcolclasses)
  # This will return position. You can get the names if you want from the colnames
  charcols <- which(allcolclasses %in% "character")
  charcols
}
#'Relevel all the factor columns. This is done when your remove some fo factors completely.
#'X <- relevelfactorcols(X)
relevelfactorcols <- function(X)
{
  #find all factor columns
  factorcols <- findallfactorcols(X)
  print (factorcols)
  #Relevel the columns
  X[,factorcols] <- lapply(X[,factorcols], function(cl) as.factor(as.character(cl)))
  X
}

#'It will basically find for factor columns a factor which is very infrequent in data
#'It doesnt return anything but will print some information and thenyou can decide to remove those factors if needed.
#'findunusualdata(X)
findunusualdata <- function(X,cutoff= 0.0001,clscol='Class')
{
  cols <- colnames(X)

  factorcols <- findallfactorcols(X)
  for (cl in factorcols)
  {

    tbl <- table(X[,cl])
    tbl_ratio <- tbl*1.0/sum(tbl)
    unusualcases <- tbl_ratio[tbl_ratio <= cutoff]
    if (length(unusualcases) > 0)
    {
      print (paste("Unusual data for column",cols[cl],'column number  is ',cl,'starts'))
      print(unusualcases)
      print ("table output")
      print (tbl)
      if (length(clscol) > 0)
      {
        print ("table output with classinfo")
        print (table(X[,cl],X[,clscol]))
      }
      print (paste("Unusual data for column",cols[cl],'ends'))
      print("")
    }
  }

}


findmissingdata <- function(X)
{
  #find missing data in table and then for each column
  completedata <- X[complete.cases(X),]
  missingdata <- X[!complete.cases(X),]
  rowswithmissingdata <- nrow(missingdata)
  print (paste('Rows with missing data ',rowswithmissingdata))
  #find missing rows for each column
  missingcoldata <- as.data.frame(sort(sapply(X, function(x) sum(is.na(x))),decreasing=TRUE))
  colnames(missingcoldata) <-c('numofmissingrows')
  print (missingcoldata)
  list(completedata=completedata,missingdata=missingdata)
}

#below methods are working on data frames and have not been enabled to work with vector as of now.But you can as,character and as,numeric directly on vectors
convertinttonum <- function(X){

  intcols <- findintcols(X)
  print (intcols)
  X[,intcols] <- lapply(X[,intcols],function(x) as.numeric(x))
  X
}
convertchartofact <- function(X){
  charcols <- findcharcols(X)
  X[,charcols] <- lapply(X[,charcols],function(x) as.factor(x))
  X
}
convertfacttochar <- function(X){
  factcols <- findallfactorcols(X)
  X[,factcols] <- lapply(X[,factcols],function(x) as.character(x))
  X
}
convertchartonum <- function(X){
  charcols <- findcharcols(X)
  X[,charcols] <- lapply(X[,charcols],function(x) as.numeric(x))
  X
}

convertnumtochar <- function(X){
  numcols <- findallnumcols(X)
  X[,numcols] <- lapply(X[,numcols],function(x) as.character(x))
  X
}
convertfacttonum <- function(X){
  X <- convertfacttochar(X)
  X <- convertchartonum(X)
  X
}
convertnumtofact <- function(X){
  X <- convertnumtochar(X)
  X <- convertchartofact(X)
  X
}

findcorrelation <- function(X){
  #convert int cols to numeric
  ncols <- ncol(X)
  X <- convertinttonum(X)
  #find all the numeric columns and use them for the correlation matrix
  numcols <- findallnumcols(X)
  X <- X[,numcols]
  #run correlation on X
  X<- cor(X)
  X
}
