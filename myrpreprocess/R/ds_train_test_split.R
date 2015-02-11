shufflesplit <- function (X,test_ratio=0.3)
{  
  nrows <- nrow(X)
  #print(nrows)
  tstrows <- nrows*test_ratio
  #print(tstrows)
  allrows <- as.vector(seq(1,nrows))
  #print(allrows)
  #print (rownames(X))
  test_index_sample <- sample(allrows,tstrows)
  #print (test_index_sample)
  test_index <- allrows[test_index_sample]
  #print(test_index)
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
  
  for (cls in clslvls){
    
    df <- X[X[,1]==cls,]
    
    splitdata <- shufflesplit(df,test_ratio)
    
    train_index <- append(train_index,df[splitdata$train_index,c('allrows')])
    test_index <- append(test_index,df[splitdata$test_index,c('allrows')])
  } 
  a <- intersect(rownames(X[splitdata$test_index,]),rownames(X[splitdata$train_index,]))
  b <- setequal(union(rownames(X[splitdata$test_index,]),rownames(X[splitdata$train_index,])),rownames(X[,]))
  #if (!b)
  #{
  #  print ("there is some issue with the splitting")
  #  c <- setdiff(union(rownames(X[splitdata$test_index,]),rownames(X[splitdata$train_index,])),rownames(X[,]))
  #  print (c)
  #  print (a)
  #}
  list(train_index=train_index,test_index=test_index)  
}
