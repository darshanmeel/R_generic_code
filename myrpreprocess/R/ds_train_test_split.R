generte_train_test_validset <- function(X,test_ratio= 0.3,valid_ratio= NULL)
{
  # split the data in test and train
  splitdata <- stratifiedshufflesplit(X,test_ratio)
  test_index <- splitdata$test_index
  test_data <- X[test_index,]
  train_data <- X[splitdata$train_index,]
  if (length(valid_ratio) > 0)
  {
    print ("we have valid test")
    splitdata <- stratifiedshufflesplit(train_data,valid_ratio)
    train_index <- splitdata$train_index
    valid_index <- splitdata$test_index
    valid_dt <- train_data[valid_index,]
    train_data <- train_data[train_index,]
  }
  else{
    valid_dt=NULL
  }
  print ('here')
  list(train_data=train_data,valid_data=valid_dt,test_data=test_data)
}

shufflesplit <- function(X,test_ratio=0.3)
{
  nrows <- nrow(X)
  print(nrows)
  tstrows <- nrows*test_ratio
  #print(tstrows)
  allrows <- as.vector(seq(1,nrows))
  #print(allrows)
  #print (rownames(X))
  test_index_sample <- sample(allrows,tstrows)
  #print (test_index_sample)
  test_index <- allrows[test_index_sample]
  print(test_index)
  train_index <- allrows[-test_index_sample]
  #print (train_index)
  list(train_index=train_index,test_index=test_index)
}


stratifiedshufflesplit <-  function (X,test_ratio=0.3)
{
  nrows <- nrow(X)
  classcol <- ncol(X)
  train_index <- as.vector(0)
  test_index <- as.vector(0)
  clslvls <- levels(X[,classcol])
  X <- X[,classcol,drop=FALSE]

  allrows <- as.vector(seq(1,nrows))
  X <- cbind(X,allrows)
  print (clslvls)
  for (cls in clslvls){

    df <- X[X[,1]==cls,]
    print ("i m here 1")
    sdata <- shufflesplit(df,test_ratio)
    print ("i m here 2")
    train_index <- append(train_index,df[sdata$train_index,c('allrows')])
    test_index <- append(test_index,df[sdata$test_index,c('allrows')])
  }
  list(train_index=train_index,test_index=test_index)
}
