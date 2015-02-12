myindividualplot <- function(df,clscol = NULL,ttl='Graph',timetovieweachinput=2){

  factcols <- findallfactorcols(df)
  nc <- ncol(df)
  colclasses <- c(rep(FALSE,nc))
  colclasses[factcols] <- c(TRUE)
  dfcolnames <- colnames(df)
  # This loop will run for all the columns
  for (i in 1:nc)
  {
    x <- dfcolnames[i]
    print (paste("generating graph for the column",x))
    isxfactor <- colclasses[i]
    print (x)
    g <- IndividualPlots(df=df,x=x,col=clscol,isxfactor=isxfactor)

    print(g)
    Sys.sleep(timetovieweachinput)
  }

}


mymultplot <- function(df,clscol = 'Class',ttl='Graph',timetovieweachinput=2){
  colclasses <- findclassofcolumns(df)
  nc <- ncol(df)
  dfcolnames <- colnames(df)
  # This loop will run for all the columns
  for (i in 1:(nc-2))
  {
    x = dfcolnames[i]
    isxfactor = ifelse(colclasses[i]=='factor',TRUE,FALSE)
    k = i+1
    if (k==nc) break
    for (j in k:(nc))
    {
      y = dfcolnames[j]
      isyfactor = ifelse(colclasses[j]=='factor',TRUE,FALSE)
      g <- mult_plots_ggplot_scatter(df,x,y,clscol,isxfactor,isyfactor,ttl,cf=NULL)
      print(g)
      Sys.sleep(timetovieweachinput)
    }
  }
}


mymultplotbyfactcol_1 <- function(df,colorcol = 'Class',ttl='Graph'){

  factcols <- findallfactorcols(df)
  nc <- ncol(df)

  colclasses <- c(rep(FALSE,nc))
  colclasses[factcols] <- c(TRUE)
  print (factcols)
  print (colclasses)
  dfcolnames <- colnames(df)
  numcols <- dfcolnames[-factcols]
  print (numcols)
  nc <- length(numcols)
  for (col in factcols)
  {
    cf <- dfcolnames[col]
    # This loop will run for all the columns
    for (i in 1:(nc-2))
    {
      x = numcols[i]
      isxfactor=FALSE
      k = i+1
      if (i==nc) break
      for (j in k:(nc))
      {
        y = numcols[j]
        isyfactor=FALSE
        g <- mult_plots_ggplot_scatter(df,x,y,colorcol,isxfactor,isyfactor,ttl,cf=cf)
        print(g)
      }
    }
  }
}




mymultplotbyfactcol <- function(df,colorcol = 'Class',ttl='Graph'){

  factcols <- findallfactorcols(df)
  nc <- ncol(df)
  colclasses <- findclassofcolumns(df)
  dfcolnames <- colnames(df)
  for (col in factcols)
  {
    cf <- dfcolnames[col]
    lvls <- levels(df[,cf])
    for (lvl in lvls){
      mydf <- df[df[,cf]==lvl,]
      mydf[,cf] <- NULL
      print (paste("plots of columns",cf,' and its value ',lvl,'starts'))
      myindividualplot(lv_c,col='Class')

      # Generate scatter plot of all the continous columns against each other and if class is give use that but now have one graph each for a level
      #run below if you have small number of columns otherwise it will run for too long.
      mymultplot(lv_c)
      print (paste("plots of columns",cf,' and its value ',lvl,'ends'))

    }

  }
}
