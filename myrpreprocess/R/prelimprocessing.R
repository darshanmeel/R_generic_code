# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)

# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.

# This is a quite generic method. Use this to have a peak at your data and how it looks like. It does a lots of stuff.

# 1. It finds all the missing data in DF as well as in each column
# 2. Return the str,summary and head outputs.
# 3. It suggests numeric oclumns which could be converted to factor columns.
# 4. Find correlation between numeric data
# 5. Find unusual data e.g. in a factor column one or more values are negligible as compared to others.
# 6. It then plots the graphs for individual columns and then for a all possible pair of columns.
# 7. Before generating the graphs it samples the data so that max rows are 10000 as plotting too much data will be time consuming
# thus the graphs will not be based on full data but a sample of the data but these will give you an idea how your data looks like.
# 8. quick chi square is not a feature selection method but it will provide you whether the data is quite imbalanced across classes
# for fatcor columns.
# Once you are done with this then you can fix some data and run the feature selection as part of featureselections.
# Fianlly you do your modelling and see how your data looks like.


prelimprocessing <- function(X,resrows=2,inttonum=TRUE,clscol='Class',timetovieweachinput=5,generategraphs=TRUE,remmissingdatabeforeprocessing=FALSE,maxlvl=60,cutoff=0.001)
{
  #remove any misisng columns
  com_mis <- findmissingdata(X)
  recordswithmisisngvalues <- com_mis[[2]]
  complete_rec <- com_mis[[1]]
  if (remmissingdatabeforeprocessing)
  {
    X <- complete_rec
  }
  Sys.sleep(timetovieweachinput)
  # get a peak at metadata
  print (str(X))
  Sys.sleep(timetovieweachinput)
  #get a peak into summary of data
  print(summary(X))
  Sys.sleep(timetovieweachinput)
  # Read first resrows lines
  print(head(X,resrows))
  Sys.sleep(timetovieweachinput)
  #if there is a class col put it at the end of the data frame
  cols <- colnames(X)
  clscolpos <- match(clscol,cols)
  if (length(clscolpos) !=  0){
    X <- moveclasscolintheend(X,clscolpos)
    clscolpos <- ncol(X)
    print ('start prining table of class data')
    print(table(X[,clscolpos]))
    print ('end prining table of class data')
    Sys.sleep(timetovieweachinput)
  }
  # There are some int columns change them to numeric. This to make few things easier down the line
  # integer calcualtion can be faster and if you do not need it then dont change it.
  if (inttonum)
  {
    intcols <- findintcols(X)
    X <- convertinttonum(X)
  }
  print ('start converting all char to fact')
  # make all char cols to fact cols
  X <- convertchartofact(X)
  print ('done converting all char to fact')
  #Now remove all those columns from the data which has more than 60 factors.
  factcols <- findallfactorcols(X)
  numoffactcols <- length(factcols)
  if (numoffactcols >0)
  {
    print ('start removing factor cols which has more than 60 level of factors')
    rmcls <- factcolwithhighnumoflevels(X,maxlvl=maxlvl)
    print("removing columns with very high levels list of columns is given below")
    print (rmcls$remcols)

    if (length(rmcls$remcols) > 0){
      X[,remcols] <- as.data.frame(c(NULL))
    }
    print ('end removing factor cols which has more than 60 level of factors')
  }
  print('start find int columns which can be converted to factcol')
  findnumcolswhicharecandidateforfactcols(X,maxdistinctvalues = maxlvl)

  Sys.sleep(timetovieweachinput)
  print('end find int columns which can be converted to factcol')


  #let us see if the data for classes comes from same distribution. You might get the warnings thus it is not going to
  # be exact but will give us enough info
  print ('start findcorrelation ')
  cor1 <- findcorrelation(X)

  #just printing top values if needed you can print more data
  print (head(cor1,resrows))
  print ('done findcorrelation ')
  Sys.sleep(timetovieweachinput)

  #Now find which features are related to each otehr. This is an unsupervised method
  print ('start check_whether_features_are_related ')

  check_whether_features_are_related(X,alpha=0.5,cor_cutoff=0.3)
  print ('done check_whether_features_are_related ')
  Sys.sleep(timetovieweachinput)
  #remove all the factcols which has
  #For all the factor columns check if one of level is too much underrepresented in the data. Cutoff is how much
  # it should be of total data.
  #If you want you can remove the data. This data can be quite large.
  print ('start findunusualdata')
  Sys.sleep(timetovieweachinput)
  findunusualdata(X,cutoff=cutoff,clscol=clscol)
  print('done findunusualdata')
  Sys.sleep(timetovieweachinput)

  if (length(clscolpos) !=  0){
    # call a chi square kidn of stuff to see how the data is in fatcor columns across classes.
    print ('start quick darshan singh style chi square')
    quickchisquare(X)
    print ('done quick darshan singh style chi square')
    Sys.sleep(timetovieweachinput)
  }
  #Use startified splitting. Use just 5000 rows for graphs as too much data will cause issues
  test_ratio <- 10000.0/nrow(X)
  test_ratio <- ifelse(test_ratio>0.9,0.9,test_ratio)
  print (test_ratio)
  splitdata <- stratifiedshufflesplit(X,test_ratio=test_ratio)
  test_index <- splitdata$test_index
  sample_data <- X[test_index,]

  nc <- ncol(X)
  reqcols <- nc
  if (nc > 20){
    print ("There are too many columns andtoo many graphs would be generated. Thus restricting to 30 cols")
    reqcols <- 20
  }
  graphdata <- sample_data[,1:reqcols]
  if (length(clscolpos) !=  0){
    cls <- colnames(graphdata)
    cls <- append(cls,clscol)
    graphdata <- cbind(graphdata,sample_data[,nc])
    colnames(graphdata) <- cls
  }
  #Below makes more sense when you are using R markdown and storing it as html or word
  if (generategraphs)
  {
    # generate the plots to see how data looks.
    plotvariousgraphs(X,individualplots=TRUE,crossplot=TRUE,byfactcols=FALSE,plotall=FALSE,clscol=clscol,timetovieweachinput=timetovieweachinput)

  }
}
