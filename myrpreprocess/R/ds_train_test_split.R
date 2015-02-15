# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)

# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.

# This is quite useful in the machine learning especially when you are doing analysis on your data like feature selection
# or want to see how a model fit on data without using CV.
# If valid_ratio is provided it returns the validation set which you should always have to claibrate your model. Once you are happy
# test it on test_data and then you can have k fold validation as well.

#valid ratio 0.3 means it will be 30 % of data remaning after test_ratio has been splitted. so if test_ratio is 0.3 and valid_ratio is 0.3  valid data will be (1-0.3)*0.3 i.e. 0.21 of total data passed
generte_train_test_validset <- function(X,test_ratio= 0.3,valid_ratio= NULL)
{
  # split the data in test and train
  splitdata <- stratifiedshufflesplit(X,test_ratio)
  test_index <- splitdata$test_index
  test_data <- X[test_index,]
  train_data <- X[splitdata$train_index,]
  valid_dt=test_data
  if (nrow(train_data) > 0)
  {
    if (length(valid_ratio) > 0 )
    {
      print ("we have valid test")

      splitdata <- stratifiedshufflesplit(train_data,valid_ratio)
      train_index <- splitdata$train_index
      valid_index <- splitdata$test_index
      valid_dt <- train_data[valid_index,]
      train_data <- train_data[train_index,]
      print ("valid set is generated")
    }
    else{
      print ("there is no valid set  so test set and valid set are same")
    }
  }
  list(train_data=train_data,valid_data=valid_dt,test_data=test_data)
}

# This split the data randomly and return the test_ratio as test_index and other one as train_index
shufflesplit <- function(X,test_ratio=0.3)
{
  nrows <- nrow(X)

  tstrows <- nrows*test_ratio
  #print(tstrows)
  allrows <- as.vector(seq(1,nrows))
  #print(allrows)
  #print (rownames(X))
  test_index_sample <- sample(allrows,tstrows)
  #print (test_index_sample)
  test_index <- allrows[test_index_sample]

  train_index <- allrows[-test_index_sample]
  #print (train_index)
  list(train_index=train_index,test_index=test_index)
}


# This is stratified sampling and will be based on the class column and it consider last column as class column.
stratifiedshufflesplit <-  function (X,test_ratio=0.3)
{
  nrows <- nrow(X)
  classcol <- ncol(X)
  train_index <- as.vector(0)
  test_index <- as.vector(0)

  clslvls <- levels(X[,classcol])
  if (length(clslvls) == 0){
    print ("either the last column is not a class column or if it is a clas column it is not a factor column. Startified sampling need class")
  }
  else{
    X <- X[,classcol,drop=FALSE]

    allrows <- as.vector(seq(1,nrows))
    X <- cbind(X,allrows)

    for (cls in clslvls){

      df <- X[X[,1]==cls,]

      sdata <- shufflesplit(df,test_ratio)

      train_index <- append(train_index,df[sdata$train_index,c('allrows')])
      test_index <- append(test_index,df[sdata$test_index,c('allrows')])
    }
  }
  list(train_index=train_index,test_index=test_index)
}
