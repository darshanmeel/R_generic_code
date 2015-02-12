prelimprocessing <- function(X,resrows=10,inttonum=TRUE,clscol='Class',timetovieweachinput=5,generategraphs=TRUE,remmissingdatabeforeprocessing=FALSE,maxlvl=60)
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
  if (clscolpos >  0){
    X <- moveclasscolintheend(X,clscolpos)
    print(table(X[,clscolpos]))
    Sys.sleep(timetovieweachinput)
  }
  # make all char cols to fact cols
  X <- convertchartofact(X)

  #Now remove all those columns from the data which has more than 60 factors.
  rmcls <- removefactcolwithhighnumoflevels(X,maxlvl=maxlvl)
  print("removing columns with very high levels list of columns is given below")
  print (rmcls$remcols)

  X[,remcols] <- as.data.frame(c(NULL))

  print(paste("columns which could be converted to fctors columns",findnumcolswhicharecandidateforfactcols(X,maxdistinctvalues = maxlvl)))
  Sys.sleep(timetovieweachinput)
  # There are some int columns change them to numeric. This to make few things easier down the line
  # integer calcualtion can be faster and if you do not need it then dont change it.
  if (inttonum)
  {
    intcols <- findintcols(X)
    X <- convertinttonum(X)
  }

  #let us see if the data for classes comes from same distribution. You might get the warnings thus it is not going to
  # be exact but will give us enough info
  cor1 <- findcorrelation(X)
  #just printing top values if needed you can print more data
  print (head(cor1,resrows))
  Sys.sleep(timetovieweachinput)

  #remove all the factcols which has
  #For all the factor columns check if one of level is too much underrepresented in the data. Cutoff is how much
  # it should be of total data.
  #If you want you can remove the data. This data can be quite large.

  findunusualdata(X,cutoff=0.0005,clscol=clscol)

  Sys.sleep(timetovieweachinput)

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
  if (clscolpos >  0){
    cls <- colnames(graphdata)
    cls <- append(cls,clscol)
    graphdata <- cbind(graphdata,sample_data[,nc])
    colnames(graphdata) <- cls
  }
  #Below makes more sense when you are using R markdown and storing it as html or word
  if (generategraphs)
  {


    # generate the plots for each column to see how data looks.
    print ("now generating the first set of plots")
    myindividualplot(graphdata,clscol=clscol,timetovieweachinput=timetovieweachinput)
    Sys.sleep(timetovieweachinput)
    # Generate scatter plot of all the continous columns against each other and if class is give use that but now have one graph each for a level
    #run below if you have small number of columns otherwise it will run for too long.
    print ("now generating the second set of plots")
    mymultplot(graphdata,clscol=clscol,timetovieweachinput=timetovieweachinput)
    Sys.sleep(timetovieweachinput)
  }
}
