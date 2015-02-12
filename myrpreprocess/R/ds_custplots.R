myindividualplot <- function(df,col=NULL){
  print ('plotting individual plots')
  colclasses <- findclassofcolumns(df)
  dfcolnames <- colnames(df)
  nc <- ncol(df)
  # This loop will run for all the columns
  col <- ifelse(length(col)==0,c(1),col)
  ed <- ifelse(col==1,nc,nc-1)
  for (i in 1:ed)
  {
    x <- dfcolnames[i]
    isxfactor <- ifelse(colclasses[i]=='factor',TRUE,FALSE)
    g <- IndividualPlots(df=df,x=x,col=col,isxfactor=isxfactor)
    print(g)
  }

}


mymultplot <- function(df,col=NULL){
  print ('plotting mult plots')
  colclasses <- findclassofcolumns(df)
  dfcolnames <- colnames(df)
  nc <- ncol(df)
  # This loop will run for all the columns
  col <- ifelse(length(col)==0,c(1),col)
  ed <- ifelse(col==1,nc-1,nc-2)

  # This loop will run for all the columns
  for (i in 1:ed)
  {
    x = dfcolnames[i]
    isxfactor = ifelse(colclasses[i]=='factor',TRUE,FALSE)
    k = i+1
    for (j in k:(ed+1))
    {
      y = dfcolnames[j]
      isyfactor = ifelse(colclasses[j]=='factor',TRUE,FALSE)
      g <- mult_plots_ggplot_scatter(df,x,y,col,isxfactor,isyfactor)
      print(g)
    }
  }
}


mymultplotbyfactcol <- function(df,col = NULL){

  print ('plotting mult fact plots')
  print ('1')
  print(head(df))
  print ('2')
  #relevel to make sure that we have data for all levels
  df <- relevelfactorcols(df)
  col <- ifelse(length(col)==0,c(1),col)
  factcols <- findallfactorcols(df)
  numcols <- findallnumcols(df)
  nc <- length(numcols) + 1
  dfcolnames <- colnames(df)
  if (col==1){
    ed <- nc-1
    slcols <- numcols
  }
  else
  {
    ed <- nc-2
    slcols <- numcols
    classcolpos <- which(col %in% colnames(X))
    slcols <- append(slcols,classcolpos)
  }
  # This loop will run for all the columns

  ed <- ifelse(col==1,nc-1,nc-2)

  for (col in factcols)
  {
    cf <- dfcolnames[col]
    lvls <- levels(df[,cf])
    for (lvl in  lvls){

      mydf <- df[df[,cf]==lvl,slcols]
      # This loop will run for all the columns
      for (i in 1:ed)
      {
        x = numcols[i]
        isxfactor=FALSE
        k = i+1
        for (j in k:(ed+1))
        {
          y = numcols[j]
          isyfactor=FALSE
          g <- mult_plots_ggplot_scatter(mydf,x,y,colorcol,isxfactor,isyfactor)
          print(g)
        }
      }
    }

  }
}

#generate all of above 3 kind or one of those graphs

plotvariousgraphs <- function(X,individualplots=FALSE,crossplot=FALSE,byfactcols=FALSE,plotall=FALSE,classcol='Class')
{
  cols <- colnames(X)
  classcolpos <- (classcol %in% colnames(X))

  if (classcolpos){
    cls <- X[,classcol,drop=FALSE]
    X[,classcol] <- NULL
    X <- cbind(X,cls)
    col <- classcol
    print(findclassofcolumns(X))
  }
  else
  {
    col <- c(1)
  }
  if (plotall){
    individualplots=TRUE
    crossplot=TRUE
    byfactcols=TRUE
  }
  if (individualplots){
    myindividualplot(X,col)
  }


  # Generate scatter plot of all the continous columns against each other and if class is give use that but now have one graph each for a level
  #run below if you have small number of columns otherwise it will run for too long.
  if (crossplot){
    mymultplot(X,col)
  }

  # Here we will generate graph for each numric columns with each other.But the data will be not full data frame but rather
  # data for a given level for a given factor column. We will do same for all the columns. Thus, make sure that you have small data and
  #small number of columns otherwise it will run for lng and will givelots of graphs.
  if (byfactcols){
    mymultplotbyfactcol(df,col)
  }

}

#plotvariousgraphs(ab,plotall=TRUE,classcol='Class')
