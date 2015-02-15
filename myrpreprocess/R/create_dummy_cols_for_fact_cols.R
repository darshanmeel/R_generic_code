#Now make all the factor columns their own columns
create_dummy_cols_for_fact_cols <- function(X,clscolpos)
{
fcl <- findallfactorcols(X)
#remove class column as we do not want to binarize that columns
fcl <- fcl[-clscolpos]

for (cl in fcl){

  cols <- colnames(X)
  lvls <- levels(X[,cl])
  i <- 0
  for (lvl in lvls){
    cl_suf <- as.character(lvl)
    clname <- paste0(cols[cl],'_',as.character(i))
    X$cl1 <- as.factor(c(ifelse(X[,cl]==lvl,1,0)))
    test_data$cl1 <- as.factor(c(ifelse(test_data[,cl]==lvl,1,0)))
    valid_data$cl1 <- as.factor(c(ifelse(valid_data[,cl]==lvl,1,0)))
    cols <- append(cols,clname)
    colnames(X) <- cols
    colnames(test_data) <- cols
    colnames(valid_data) <- cols
    i <- i + 1
  }

}
#remove the original columns

X[,fcl] <- as.data.frame(NULL)


}



train_data <- cbind(X,cls)
test_data <- cbind(test_data,tst_cls)
valid_data <- cbind(valid_data,valid_cls)
cols <- append(cols,clscol)
colnames(train_data ) <- cols
colnames(test_data ) <- cols
colnames(valid_data ) <- cols
clspos <- ncol(train_data)
