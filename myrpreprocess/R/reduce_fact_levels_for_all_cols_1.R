#Here we use chimerge to reduce the levels of unorderd factor columns. It can merge any level to any other
#level as long as these are independent
reduce_unord_fact_levels <- function(X,cls,clname)
{

  lvls <- levels(X)
  nl <- length(lvls) -1
  print(nl)
  pvals <- as.data.frame(t(c('NULL','NULL','NULL',0.0)))
  colnames(pvals) <- c('col','lvl1','lvl2','pvalue')
  pvals$col <- as.character(pvals$col)
  pvals$lvl1 <- as.character(pvals$lvl1)
  pvals$lvl2 <- as.character(pvals$lvl2)
  pvals$pvalue <- as.numeric(as.character(pvals$pvalue))
  cols <- colnames(X)
  for (i in 1:nl){
    k = i + 1
    for (j in k:(nl+1)){
      rowsneeded <- (X==lvls[i] | X==lvls[j])
      df <- X[rowsneeded]
      df <- as.character(df)
      df <- as.factor(df)
      cls1 <- cls[rowsneeded]
      tb <- table(df,cls1)
      if (length(tb) < 4){
        pdtl <- c(clname,lvls[i],lvls[j],1)
      }
      else{

        ak <- chisq.test(tb)

        pdtl <- c(clname,lvls[i],lvls[j],ak$p.value)
      }
      pvals <- rbind(pvals,pdtl)
    }
  }
  pvals <- pvals[-1,]
  #now print
  print(paste("print the p values for factor levels which are independent.These are order by p values. It is for column",clname))
  #remove any NaN and NA's the p value column is str still
  pvals<- subset(pvals,pvalue !='NaN' & pvalue != 'NA')
  pvals$pvalue <- as.numeric(as.character(pvals$pvalue))
  pvals <- pvals[order(pvals[,4],decreasing=FALSE,na.last=TRUE),]
  pvals
}
# This method is used for orderd column level reductions. Here you can combine 2 levels if these are
#continous
reduce_ord_fact_levels <- function(X,cls,clname)
{

  lvls <- levels(X)
  nl <- length(lvls) -1
  pvals <- as.data.frame(t(c('NULL','NULL','NULL',0.0)))
  colnames(pvals) <- c('col','lvl1','lvl2','pvalue')
  pvals$col <- as.character(pvals$col)
  pvals$lvl1 <- as.character(pvals$lvl1)
  pvals$lvl2 <- as.character(pvals$lvl2)
  pvals$pvalue <- as.numeric(as.character(pvals$pvalue))
  cols <- colnames(X)
  for (i in 1:nl){
    j = i + 1
    rowsneeded <- (X==lvls[i] | X==lvls[j])
    df <- X[rowsneeded]
    df <- as.character(df)
    df <- as.factor(df)
    cls1 <- cls[rowsneeded]
    tb <- table(df,cls1)
    if (length(tb) < 4){
      pdtl <- c(clname,lvls[i],lvls[j],1)

    }
    else{
      ak <- chisq.test(tb)
      pdtl <- c(clname,lvls[i],lvls[j],ak$p.value)
    }


    pvals <- rbind(pvals,pdtl)
  }
  pvals <- pvals[-1,]
  #now print
  print(paste("print the p values for factor levels which are independent.These are order by p values. It is for column",clname))
  pvals<- subset(pvals,pvalue !='NaN' & pvalue != 'NA')
  pvals$pvalue <- as.numeric(as.character(pvals$pvalue))
  pvals <- pvals[order(pvals[,4],decreasing=FALSE,na.last=TRUE),]

  pvals
}

# This might take lots of time.So be carefull. It is similar to chimerge for numeric columns but we merge levels of a facto if those are independent.
#It might be quite slow for levels more than say 30-40
merge_lvls <- function(trn,tst,vld,alpha= 0.01)
{
  X <- trn
  cls <- X[,ncol(X)]
  print(head(cls))
  factcols <- findallfactorcols(X)

  print(factcols)
  factcols <- factcols[-1]
  cols <- colnames(X)
  ord=FALSE
  for (cl in factcols)
  {
    ordlvls <- levels(X[,cl])
    numoflvls <- length(ordlvls)
    print (str(X))
    ord=is.ordered(X[,cl])
    while (numoflvls > 2)
    {
      print (cols[cl])
      print(ordlvls)
      print(cl)
      print (is.ordered(X[,cl]))
      if (ord)
      {
        print ('ordered')

        ab <- reduce_ord_fact_levels(X[,cl],cls,cols[cl])
      }
      else
      {
        print ('usual')
        ab <- reduce_unord_fact_levels(X[,cl],cls,cols[cl])
      }
      ab  <- ab[order(ab[,4],decreasing=TRUE,na.last=TRUE),]
      max_lvls <- ab[1,]
      print(max_lvls)
      if (max_lvls[,4] < alpha){
        print ('thr')
        break
      }
      else{
        print ('here')
        lv2 <- as.character(max_lvls$lvl2)
        lv1 <- as.character(max_lvls$lvl1)
        print(lv2)

        xcl <- as.character(X[,cl])
        xcl[xcl==lv2] <- lv1


        # test data set update
        tcl <- as.character(tst[,cl])
        tcl[tcl==lv2] <- lv1

        # test data set update
        vcl <- as.character(vld[,cl])
        vcl[vcl==lv2] <- lv1


        postoremove <- which(as.character(ordlvls)==lv2)
        ordlvls <- ordlvls[-postoremove]

        X[,cl] <- factor(xcl,levels = ordlvls)
        tst[,cl] <- factor(tcl,levels = ordlvls)
        vld[,cl] <- factor(vcl,levels = ordlvls)

        numoflvls <- length(ordlvls)
      }
    }
    if (ord)
    {
      X[,cl] <- ordered(X[,cl])
      tst[,cl] <- ordered(tst[,cl])
      vld[,cl] <- ordered(vld[,cl])
    }

  }
  list(X,tst,vld)
}


