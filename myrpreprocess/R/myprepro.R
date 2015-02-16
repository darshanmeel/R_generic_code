#THIS HAS LOTS OF GENERIC METHODS FOR SMALL TASKS INANALYSIS OF DATA WHICH I FIND USEFUL ON DAILY BASIS

# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)
#MOST OF METHOD NAMES WILL BE ENGH TO UNDERSTAND WHT THE FUNCTION IS DOING

#move class/response col in the end. I find it quite useful as it makes some data processing much better if your class/response column is in the end.
moveclasscolintheend <- function(X,clscolpos)
{
  cols <- colnames(X)
  clscolname <- cols[clscolpos]
  cls <- X[,clscolpos]
  X[,clscolpos] <- as.data.frame(c(NULL))
  cols <- colnames(X)
  cols <- append(cols,clscolname)
  X <- cbind(X,cls)
  colnames(X) <- cols
  clscolpos <- ncol(X)
  X[,clscolpos] <- as.factor(X[,clscolpos] )
  X
}

#'findclasstypeofeach columns
#'colclasses <- findclassofcolumns(X)
findclassofcolumns <- function(X)
{
  cls <- lapply(X,class)
  cls
}
find_ord_factor_cols <- function(X)
{

  factorcols <- which(as.data.frame(lapply(X,is.ordered))==TRUE)
  factorcols
}
#This method is only to return factor columns but only unordered facto columns
find_unord_factor_cols <- function(X)
{
  ordfactorcols <- find_ord_factor_cols(X)
  factorcols <- findallfactorcols(X)
  factorcols <- setdiff(factorcols,ordfactorcols)
  factorcols
}
#'findallfactorcols columns
#'factorcols <- findallfactorcols(X)
findallfactorcols <- function(X)
{
  factorcols <- which(as.data.frame(lapply(X,is.factor))==TRUE)
  factorcols


}

#'findallnumcols columns
#'numcols <- findallnumcols(X)
findallnumcols <- function(X)
{

  numcols <- which(as.data.frame(lapply(X,is.numeric))==TRUE)
  numcols
}

#'findintcols columns
#'intcols <- findintcols(X)
findintcols <- function(X)
{

  intcols <- which(as.data.frame(lapply(X,is.integer))==TRUE)
  intcols
}

#'findcharcols columns
#'charcols <- findcharcols(X)
findcharcols <- function(X)
{

  charcols <- which(as.data.frame(lapply(X,is.character))==TRUE)
  charcols
}

#'Relevel all the factor columns. This is done when your remove some of factors completely.I use it when i have removed
#'some of the data that has na columns and it could cause some of levels to be gone fully for some columns.
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
#'
findunusualdata <- function(X,cutoff= 0.0001,clscol='Class')
{
  cols <- colnames(X)
  clscolpos <- match(clscol,cols)
  factorcols <- findallfactorcols(X)
  # Below can be done as a vectorizzed operation as well. But I am not looking from perf point of view here.
  # Some day i will use below as function and will use sapply or lapply
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
        print (table(X[,cl],X[,clscolpos]))
      }
      print (paste("Unusual data for column",cols[cl],'ends'))
      print("")
    }
  }

}

# This will give you details of all misisng data in the table and will also give
# you the details of all the missing data columnwise and you can make a decision liek what to do with the data
# it aalso returns the data which is complete and data which has just all records which has oen or more missing values.
# Complete data could have zero rows in case one column has all misisng data.
# It doesnt handle any data value which u conside r missing e.g. in some cases 0 means missing.
# to do that you will need to make the 0 as NA and then it will handle that.
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

# This will run the cor and be careful that if you havelarge data then it might be quite large operation as it is o(N power 2) operation.
# Even if you pass the character columns. It will work on just columsn which are numeric and will return tehir results.

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

# return all the columns with more than certain level of factors
# This is useful when you are plotting and good for preprocessing.
# here remcols have factors higher than maxlvl andkeepcols has factor levels less than maxlvl
factcolwithhighnumoflevels <- function(X,maxlvl=60){

  factcols <- findallfactorcols(X)
  cols <- lapply(factcols,function(clpos) ifelse(length(levels(X[,clpos])) > maxlvl,TRUE,FALSE))
  cols <- as.vector(unlist(cols))
  remcols <- factcols[cols]
  keepcols <- factcols[!cols]
  list(remcols=remcols,keepcols=keepcols)
}

#find possible candidate of int and numeric cols which can be converted to factors. e.g columns which has just few
#This is quite useful method especially when you want to visualize then scatter plot of the int/num cols with very few values
# doesnt look that great unless you specify the jitter. I find it much better to get same infor by a boxplot.
findnumcolswhicharecandidateforfactcols <- function(X,maxdistinctvalues = 100)
{
  cols <- colnames(X)
  X <- convertinttonum(X)
  numcols <- findallnumcols(X)
  X <- convertnumtofact(X)
  ab <- factcolwithhighnumoflevels(X,maxlvl=maxdistinctvalues)
  possiblecandidates <- intersect(numcols,ab$keepcols)
  cols[possiblecandidates]
}
