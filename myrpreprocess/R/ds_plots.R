
IndividualPlots <- function(df,x,col=NULL,isxfactor=FALSE){
  if (!isxfactor )
  {
    if (length(col) > 0)
    {

      #create density as well but use the class information
      g <- ggplot(df, aes_string(x=x, fill=col)) + geom_density(alpha=.3)
      ttl <- paste('Density graph for column ',x)
      g <- g + ggtitle(ttl)
    }
    else
    {

      #create histogram
      g <- ggplot(df,aes_string(x=x)) + geom_histogram(binwidth=1, colour="black", fill="white")
      ttl <- paste('Historgram  for column ',x)
      g <- g + ggtitle(ttl)
    }
  }
  else
  {
      g <- ggplot(df,aes_string(x=x,fill=x)) +  geom_bar(stat="bin",binwidth=1,position='fill')
      ttl <- paste('Bar plot  for column \n',x)
  }
  g <- g + ggtitle(ttl)
  g

}


# Generate scatter plot of all the continous columns against each other and if class is given use that for coloring and for the

mult_plots_ggplot_scatter <-
  function(df, x, y, z,isxfactor=FALSE,isyfactor=FALSE){
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
          ttl <- paste(x, ' and ',y)


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
    g <- g + ggtitle(ttl)
    g
  }



