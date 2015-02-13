# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)
#You can always make better graphs and these are just to give you proper graphs for analysis. Once you are happy with a graph work on more asthetics if needed.
# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.
#This genearte the density graph for continous column ,densities for classes will be shown in diff colors but on same graph.
#For factor cols it will show the bar charts.
IndividualPlots <- function(df,x,col=NULL,isxfactor=FALSE){
  if (!isxfactor )
  {
      #create density as well but use the class information
      g <- ggplot(df, aes_string(x=x, fill=col)) + geom_density(alpha=.3)
      ttl <- paste('Density graph for column\n',x)
      g <- g + ggtitle(ttl)
  }
  else
  {
    g <- ggplot(df,aes_string(x=x,fill=x)) +  geom_bar(stat="bin",binwidth=1,position='fill')
    ttl <- paste('Bar plot for column \n',x)
  }
  g <- g + ggtitle(ttl) +  theme(plot.title = element_text(size=8,lineheight=.8,vjust=2))
  g

}



#see if columns are continous or factor
#1. Generate Scatter graph between 2 continous variables. If class is provided use color of class column
#2. Generate a box plot between a continous and factor column and use  color if class is provided.
#3. If bothe columns are factor then use facet wrap for y column and use  color if class is provided
mult_plots_ggplot_scatter <-  function(df, x, y, z,isxfactor=FALSE,isyfactor=FALSE){
    if (!isxfactor )
    {
      if (!isyfactor )
      {
        g <- ggplot(df,aes_string(x, y,fill=z,colour=z)) + geom_point(shape=1) + geom_smooth(method='lm',se=FALSE) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
        ttl <- paste(x, ' and ',y)

      }
      else
      {
        g <- ggplot(df,aes_string(x=y, y=x,fill=z,color=z)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
        ttl <- paste(y, ' and ',x)
      }
    }
    else
    {
      if (!isyfactor )
      {
        g <- ggplot(df,aes_string(x, y,fill=z,color=z)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
        ttl <- paste(x, ' and ',y)
      }
      else
      {
        g <- ggplot(df,aes_string(x=x,fill=z,color=z)) +  geom_bar(stat="bin",binwidth=1,position='fill') + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12)) + facet_wrap(reformulate(y))
        ttl <- paste( x, ' by ',y)

      }
    }
    g <- g + ggtitle(ttl) +  theme(plot.title = element_text(size=8,lineheight=.8,vjust=2))
    g
  }








