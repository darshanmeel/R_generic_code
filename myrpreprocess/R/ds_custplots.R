# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)
#You can always make better graphs and these are just to give you proper graphs for analysis. Once you are happy with a graph work on more asthetics if needed.
# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.

# Here it will take a Data frame and will plot the density diagram(for continous) or bar chart (for factor columns.)
myindividualplot <- function(df,clscol = 'Class',timetovieweachinput=0){
  print ("now generating the first set of plots i.e. individual plots for each column")
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
    #This genearte the density graph for continous column ,densities for classes will be shown in diff colors but on same graph.
    #For factor cols it will show the bar charts.
    g <- IndividualPlots(df=df,x=x,col=clscol,isxfactor=isxfactor)
    print(g)
    Sys.sleep(timetovieweachinput)
  }
  print ("finished generating the first set of plots i.e. individual plots for each column")
}

#Here we will generate the graphs between 2 columns of same data frame to see how data is related. It is a longer and customized
#version of plot function in R.
mymultplot <- function(df,clscol = 'Class',timetovieweachinput=2){
  print ("now generating the second set of plots i.e.  plots for a pair of columns")
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
      #1. Generate Scatter graph between 2 continous variables. If class is provided use color of class column
      #2. Generate a box plot between a continous and factor column and use  color if class is provided.
      #3. If bothe columns are factor then use facet wrap for y column and use  color if class is provided
      g <- mult_plots_ggplot_scatter(df,x,y,clscol,isxfactor,isyfactor)
      print(g)
      Sys.sleep(timetovieweachinput)
    }
  }
  print ("finished generating the second set of plots i.e.  plots for a pair of columns")
}


#DO NOT RUN BELOW WHEN YOU HAVE LARGE AMOUNT OF DATA OR LOTS OF COLUMNS OR LOTS OF FACTORS IN SAME COLUMNS.
#SOME GRAPHS WONT BE GOOD IF THERE IS NOT ENGH DATA IN A FACTOR LVL

#Below will generate huge amount of graphs.
#1. Get all factor columns and loop over these columns.
#2. For a givenfactor column find all the levels and loop over these.
#3. Select the data for given col and factor based on the above 2 line.
#4. This is our new data frame and plot individual as well as cross plots and that is why we will have too any graphs.

mymultplotbyfactcol <- function(df,clscol = 'Class',timetovieweachinput=2){
  print ("start generating the third set of plots i.e.  plots for a pair of columns but for a subste of data based on the value of factor columns")
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
      #Generate the individual plots
      myindividualplot(lv_c,col='Class')
      #Generate other graphs
      mymultplot(lv_c)
      print (paste("plots of columns",cf,' and its value ',lvl,'ends'))

    }

  }
  print ("finished generating the third set of plots i.e.  plots for a pair of columns but for a subste of data based on the value of factor columns")
}

#generate all of above 3 kind or one of those graphs

plotvariousgraphs <- function(X,individualplots=FALSE,crossplot=FALSE,byfactcols=FALSE,plotall=FALSE,clscol='Class',timetovieweachinput=2)
{
  cols <- colnames(X)
  classcolpos <- match(clscol,colnames(X))
  if (length(classcolpos) <=0) {
    clscol <- c(1)
  }
  if (plotall){
    individualplots=TRUE
    crossplot=TRUE
    byfactcols=TRUE
  }
  if (individualplots){

    myindividualplot(X,clscol=clscol,timetovieweachinput=timetovieweachinput)

  }


  #
  #run below if you have small number of columns otherwise it will run for too long.
  if (crossplot){

    mymultplot(X,clscol=clscol,timetovieweachinput=timetovieweachinput)

  }

  # Here we will generate graph for each numric columns with each other.But the data will be not full data frame but rather
  # data for a given level for a given factor column. We will do same for all the columns. Thus, make sure that you have small data and
  #small number of columns otherwise it will run for lng and will givelots of graphs.
  if (byfactcols){
    mymultplotbyfactcol(df,clscol=clscol,timetovieweachinput=timetovieweachinput)
  }

}

#plotvariousgraphs(ab,plotall=TRUE,classcol='Class')

