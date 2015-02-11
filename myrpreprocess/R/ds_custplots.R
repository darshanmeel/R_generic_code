myindividualplot <- function(df,col = NULL,ttl='Graph'){
  
  factcols <- findallfactorcols(df)
  nc <- ncol(df)
  colclasses <- c(rep(FALSE,nc))
  colclasses[factcols] <- c(TRUE)
  print (factcols)
  print (colclasses)
  dfcolnames <- colnames(df)
  # This loop will run for all the columns 
  for (i in 1:nc)
  {
    x <- dfcolnames[i]    
    isxfactor <- colclasses[i]   
    print (x)
    g <- IndividualPlots(df=df,x=x,col=col,isxfactor=isxfactor)
    
    print(g)
  }
  
}


mymultplot <- function(df,colorcol = 'Class',ttl='Graph'){
  colclasses <- findclassofcolumns(df)
  nc <- ncol(df)
  dfcolnames <- colnames(df)
  # This loop will run for all the columns 
  for (i in 1:(nc-1))
  {
    x = dfcolnames[i]
    isxfactor = ifelse(colclasses[i]=='factor',TRUE,FALSE)
    k = i+1
    if (i==nc) break
    for (j in k:(nc-1))
    {
      y = dfcolnames[j]
      isyfactor = ifelse(colclasses[j]=='factor',TRUE,FALSE)
      g <- mult_plots_ggplot_scatter(df,x,y,colorcol,isxfactor,isyfactor,ttl,cf=NULL)
      print(g)
    }
  }
}


mymultplotbyfactcol <- function(df,colorcol = 'Class',ttl='Graph'){
  
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
    for (i in 1:nc)
    {
      x = numcols[i]
      isxfactor=FALSE
      k = i+1
      if (i==nc) break
      for (j in k:nc)
      {
        y = numcols[j]
        isyfactor=FALSE  
        g <- mult_plots_ggplot_scatter(df,x,y,colorcol,isxfactor,isyfactor,ttl,cf=cf)
        print(g)
      }
    }
  }
}
