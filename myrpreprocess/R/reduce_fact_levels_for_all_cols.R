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
  pvals$pvalue <- as.numeric(pvals$pvalue)
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

  pvals <- pvals[order(pvals[,4],decreasing=FALSE,na.last=TRUE),]
  pvals
}

reduce_ord_fact_levels <- function(X,cls,clname)
{

  lvls <- levels(X)
  nl <- length(lvls) -1
  pvals <- as.data.frame(t(c('NULL','NULL','NULL',0.0)))
  colnames(pvals) <- c('col','lvl1','lvl2','pvalue')
  pvals$col <- as.character(pvals$col)
  pvals$lvl1 <- as.character(pvals$lvl1)
  pvals$lvl2 <- as.character(pvals$lvl2)
  pvals$pvalue <- as.numeric(pvals$pvalue)
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
  pvals <- pvals[order(pvals[,4],decreasing=FALSE,na.last=TRUE),]
  pvals
}
reduce_levels_for_unord_fact_cols <- function(X)
{
  #find all fact cols
  cls <- X[,ncol(X)]
  X <- X [,-ncol(X)]
  factcols <- find_unord_factor_cols(X)
  X <- X[,factcols,drop=FALSE]
  cols <- colnames(X)
  nc <- ncol(X)
  pvals <- as.data.frame(t(c('NULL','NULL','NULL',0.0)))
  colnames(pvals) <- c('col','lvl1','lvl2','pvalue')
  pvals$col <- as.character(pvals$col)
  pvals$lvl1 <- as.character(pvals$lvl1)
  pvals$lvl2 <- as.character(pvals$lvl2)
  pvals$pvalue <- as.numeric(pvals$pvalue)
  for (i in 1:nc){
    pvls <- reduce_unord_fact_levels(X[,i],cls,cols[i])
    pvals <- rbind(pvals,pvls)
  }
  pvals
}

reduce_levels_for_ordered_fact_cols <- function(X)
{
  #find all fact cols
  cls <- X[,ncol(X)]
  X <- X [,-ncol(X)]
  factcols <- find_ord_factor_cols(X)
  X <- X[,factcols,drop=FALSE]
  cols <- colnames(X)
  nc <- ncol(X)
  pvals <- as.data.frame(t(c('NULL','NULL','NULL',0.0)))
  colnames(pvals) <- c('col','lvl1','lvl2','pvalue')
  pvals$col <- as.character(pvals$col)
  pvals$lvl1 <- as.character(pvals$lvl1)
  pvals$lvl2 <- as.character(pvals$lvl2)
  pvals$pvalue <- as.numeric(pvals$pvalue)
  for (i in 1:nc){
    pvls <- reduce_ord_fact_levels(X[,i],cls,cols[i])
    pvals <- rbind(pvals,pvls)
  }
  pvals
}

# This might take lots of time.So be carefull. It is similar to chimerge for numeric columns but we merge levels of a facto if those are independent.
#It might be quite slow for levels more than say 30-40
merge_unord_lvls <- function(trn,tst,vld,alpha= 0.01)
{
  X <- trn
  cls <- X[,ncol(X)]
  print(head(cls))
  factcols <- find_unord_factor_cols(X)
  ordcols <- find_ord_factor_cols(X)
  factcols <- setdiff(factcols,ordcols)
  print(factcols)
  factcols <- factcols[-1]
  cols <- colnames(X)
  for (cl in factcols)
  {
    ordlvls <- levels(X[,cl])
    numoflvls <- length(ordlvls)
    while (numoflvls > 2)
    {
      print (cols[cl])
      print(ordlvls)
      print(cl)
      ab <- reduce_unord_fact_levels(X[,cl],cls,cols[cl])

      cd <- ab[,4]

      print(max(cd))
      ab_max <- which.max(ab[,4])
      print(ab_max)
      max_lvls <- ab[ab_max,]
      #print(max_lvls)
      if (max_lvls[,4] < alpha){
        break
      }
      else{
        lv2 <- as.numeric(max_lvls$lvl2)
        lv1 <- as.numeric(max_lvls$lvl1)
        print(lv2)

        xcl <- as.numeric(as.character(X[,cl]))
        xcl[xcl==lv2] <- lv1


        # test data set update
        tcl <- as.numeric(as.character(tst[,cl]))
        tcl[tcl==lv2] <- lv1


        #valid
        # test data set update
        vcl <- as.numeric(as.character(vld[,cl]))
        vcl[vcl==lv2] <- lv1


        postoremove <- which(as.numeric(ordlvls)==lv2)
        ordlvls <- ordlvls[-postoremove]

        X[,cl] <- factor(xcl,levels = ordlvls)
        tst[,cl] <- factor(tcl,levels = ordlvls)
        vld[,cl] <- factor(vcl,levels = ordlvls)

        numoflvls <- length(ordlvls)
      }
    }

  }
  list(X,tst,vld)
}


#

merge_ord_lvls <- function(trn,tst,vld,alpha= 0.01)
{
  X <- trn
  cls <- X[,ncol(X)]
  print(head(cls))

  factcols <- find_ord_factor_cols(X)

  print(factcols)
  factcols <- factcols
  cols <- colnames(X)
  for (cl in factcols)
  {
    ordlvls <- levels(X[,cl])
    numoflvls <- length(ordlvls)
    while (numoflvls > 2)
    {
      print (cols[cl])
      print(ordlvls)
      print(cl)
      ab <- reduce_ord_fact_levels(X[,cl],cls,cols[cl])

      cd <- ab[,4]

      print(max(cd))
      ab_max <- which.max(ab[,4])
      print(ab_max)
      max_lvls <- ab[ab_max,]
      #print(max_lvls)
      if (max_lvls[,4] < alpha){
        break
      }
      else{
        lv2 <- as.numeric(max_lvls$lvl2)
        lv1 <- as.numeric(max_lvls$lvl1)
        print(lv2)

        xcl <- as.numeric(as.character(X[,cl]))
        xcl[xcl==lv2] <- lv1


        # test data set update
        tcl <- as.numeric(as.character(tst[,cl]))
        tcl[tcl==lv2] <- lv1


        #valid
        # test data set update
        vcl <- as.numeric(as.character(vld[,cl]))
        vcl[vcl==lv2] <- lv1


        postoremove <- which(as.numeric(ordlvls)==lv2)
        ordlvls <- ordlvls[-postoremove]

        X[,cl] <- factor(xcl,levels = ordlvls)
        tst[,cl] <- factor(tcl,levels = ordlvls)
        vld[,cl] <- factor(vcl,levels = ordlvls)

        numoflvls <- length(ordlvls)
      }
    }

  }
  list(X,tst,vld)
}

