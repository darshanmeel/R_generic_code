IndividualPlots <- function(df,x,col=NULL,isxfactor=TRUE){

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

    if (length(col) > 0)
    {
      g <- ggplot(df,aes_string(x=x,fill=col)) +  geom_bar(stat="bin",binwidth=1,position='fill')
      ttl <- paste('Bar plot  for column based on classes ',x)
      g <- g + ggtitle(ttl)
    }
    else
    {
      g <- ggplot(df,aes_string(x=x,fill=x)) +  geom_bar(stat="bin",binwidth=1,position='fill')
      ttl <- paste('Bar plot  for column ',x)
      g <- g + ggtitle(ttl)
    }
  }
  g

}


# Generate scatter plot of all the continous columns against each other and if class is given use that for coloring and for the

mult_plots_ggplot_scatter <-
  function(df, x, y, z,isxfactor=FALSE,isyfactor=FALSE,ttl='Graph',cf=NULL){
    if (!isxfactor )
    {
      if (!isyfactor )
      {
        if (length(cf) > 0)
        {
          #print('none are cat')
          g <- ggplot(df,aes_string(x, y,fill=z,colour=z)) + geom_point(shape=1) + geom_smooth(method='lm',se=TRUE)  + facet_grid(reformulate(cf)) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
          ttl <- paste('scatterplot for ', x, ' and ',y,' facets are based on column ',cf)
          g <- g + ggtitle(ttl)
        }
        else
        {
          #print('none are cat')
          g <- ggplot(df,aes_string(x, y,fill=z,colour=z)) + geom_point(shape=1) + geom_smooth(method='lm',se=TRUE) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
          ttl <- paste('scatterplot for ', x, ' and ',y)
          g <- g + ggtitle(ttl)
        }
      }
      else
      {
        if (length(cf) > 0)
        {
          print('y is cat')
          g <- ggplot(df,aes_string(x=y, y=x,fill=z,color=z)) + geom_boxplot() + facet_grid(reformulate(y~z)) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
          ttl <- paste('boxplot for ', x, ' and ',y)
          g <- g + ggtitle(ttl)
        }
        else
        {
          print('y is cat')
          g <- ggplot(df,aes_string(x=y, y=x,fill=z,color=z)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
          ttl <- paste('boxplot for ', x, ' and ',y)
          g <- g + ggtitle(ttl)
        }
      }
    }
    else
    {
      if (!isyfactor )
      {
        print('x is cat')
        g <- ggplot(df,aes_string(x, y,fill=z,color=z)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
        ttl <- paste('boxplot for ', x, ' and ',y)
        g <- g + ggtitle(ttl)
      }
      else
      {
        print('both are cat')
        print(y)
        print(x)
        g <- ggplot(df,aes_string(x=x,fill=z,color=z)) +  geom_bar(stat="bin",binwidth=1,position='fill') + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12)) + facet_wrap(reformulate(y))
        ttl <- paste('barchart for ', x, ' by ',y)
        g <- g + ggtitle(ttl)

      }
    }

    g
  }



