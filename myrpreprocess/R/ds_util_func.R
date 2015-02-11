
kstest <- function(X,pval = 0.1)
{
  colclasses <- findclassofcolumns(X)

  nc <- ncol(X)
  dfcolnames <- colnames(X)
  #numcols <- ifelse(colclasses=='numeric',TRUE,(ifelse(colclasses=='integer',TRUE,FALSE)))
  numcols <- findallnumcols(X)
  numcols <- dfcolnames[numcols]
  print(numcols)
  clses <- levels(X[,nc])
  nclses <- length(clses)
  for (i  in 1:(nclses-1)){
    a <- X[X[,nc]==clses[i],numcols]
    print (clses[i])
    print(nrow(a))
    ab <- lapply(numcols,function(x) ks.test(a[,x],pnorm))
    pvalues <- sapply(seq(1:length(numcols)),function(x) ab[[x]]$p.value)
    print(pvalues)
    print (dfcolnames[numcols[pvalues>pval]])
    for (j in (i+1):nclses){
      print (clses[j])
      b <- X[X[,nc]==clses[j],numcols]
      print(nrow(b))
      ab <- lapply(numcols,function(x) ks.test(b[,x],pnorm))
      pvalues <- sapply(seq(1:length(numcols)),function(x) ab[[x]]$p.value)
      print(pvalues)

      ab <- lapply(numcols,function(x) ks.test(a[,x],b[,x]))
      pvalues <- sapply(seq(1:length(numcols)),function(x) ab[[x]]$p.value)
      print(pvalues)
      print (dfcolnames[numcols[pvalues>pval]])

    }
  }
}



#'Run below with small sample of data and especially with small number of columns
#'as it could be very costly and it will run cube(n) LR. Thus be careful
lr_feature_selection <- function(X)
{
  #loop through all columns
  cols <- colnames(X)
  #print(cols)
  ncol <- length(cols)
  mainfrml <- paste(cols[ncol],'~')
  #now loop through all columns
  colrange <- ncol-1
  #print(colrange)
  #print(ncol)
  #define two dataframes oen for single column
  #another for column and itreaction with other column
  singlecols <- as.data.frame(t(c(as.character('NULL'),as.numeric(0.0))))
  colnames(singlecols) <- c('col1','col1_pvalue')
  singlecols$col1 <- as.character(singlecols$col1)
  singlecols$col1_pvalue <- as.numeric(singlecols$col1_pvalue)

  interactioncols <- as.data.frame(t(c(as.character('NULL'),as.character('NULL'),as.numeric(0.0),as.numeric(0.0),as.numeric(0.0))))
  colnames(interactioncols) <- c('col1','col2','col1_pvalue','col2_pvalue','interaction_pvalue')
  #provide proper data types
  interactioncols$col1 <- as.character(interactioncols$col1)
  interactioncols$col2 <- as.character(interactioncols$col2)
  interactioncols$col1_pvalue <- as.numeric(interactioncols$col1_pvalue)
  interactioncols$col2_pvalue <- as.numeric(interactioncols$col2_pvalue)
  interactioncols$interaction_pvalue <- as.numeric(interactioncols$interaction_pvalue)

  for (i in 1:colrange)
  {
    #print (cols[i])
    #generate the model for a single column only.See the p values
    frml <- paste(mainfrml,cols[i])
    #print (frml)
    lm1 <- glm(as.formula(frml),data=X[,c(i,ncol)],family=binomial)
    #print ('i am here')
    #print(summary(lm1))
    ab <- summary(lm1)$coefficients
    pvalvector <- c(cols[i])
    pvalvector <- append(pvalvector,as.vector(ab[c(2),4]))
    #print (pvalvector)
    #print(length((pvalvector)))
    singlecols <-rbind(singlecols,pvalvector)
    #loop through one more time as well but not the same column but different columns
    jstart <- i+1
    if (jstart > colrange){
      break
    }
    for (j in jstart:colrange){
      #print
      #print (cols[j])
      #now run for the two columns and their interaction model and savetheir p values
      frml <- paste(mainfrml,cols[i],'+',cols[j],'+',paste0(cols[i],':',cols[j]))
      #print(frml)
      lm1 <- glm(as.formula(frml),data=X[,c(i,j,ncol)],family=binomial)
      #print(summary(lm1))
      ab <- summary(lm1)$coefficients
      pvalvector <- c(cols[i],cols[j])
      pvalvector <- append(pvalvector,as.vector(ab[c(2:4),4]))
      #print (pvalvector)
      #print(length((pvalvector)))
      interactioncols <-rbind(interactioncols,pvalvector)
    }
  }
  list(interactioncols=interactioncols[-1,],singlecols=singlecols[-1,])
}
#ab <- lr_feature_selection(lv_cd)

#random forest for finding features. You can pass any random forest params as well
#frml <- as.formula('Class ~ .' )
# inranking pass eithr class values or accuracy or g_rg for gini/reg
# return top k columns. if k is more than number of cols it will return all columns
# it will return the feature rank as well as the fulll importance data frame which you can use if you want
rf_feature_selection <- function(X,frml,k=10,ranking='accuracy',...)
{
  cols <- colnames(X)
  frml <- as.formula(frml)
  lm1 <- randomForest(frml,X,importance=TRUE,...)
  imp <- as.data.frame(importance(lm1))
  #print (ranking)
  ordercol <- -1
  if (ranking=='accuracy'){
    print('here')
    ordercol= ncol(imp)-1
  }
  else if (ranking=='g_rg'){
    ordercol= ncol(imp)
  }
  else
  {
    clvls <- levels(X[,ncol(X)])
    print(clvls)
    ordercol <- which(clvls %in% ranking)
  }
  imp_ordered <- imp[order(imp[,ordercol],decreasing=TRUE),]
  nr <- nrow(imp_ordered)
  if (k>nr){
    k <- nr
  }
  list(feature_rank = rownames(imp_ordered[1:k,]),imp_ordered=imp_ordered)

}

#see the table and check if both class has same representation
#below method is in no way a chi square method and you shouldnt use it as below method is not using any statistics significane testing
# but it will provide u with nice details. whether a factor column has simialr representation in all classes
# for 2 class you will see that values are same in both classes it is fine as it is expected
# make sure that all the values are around 0 especially for categories where row numbers are quite high
quickchisquare <- function(X)
{
  #find all the factor cols
  factcols <- findallfactorcols(X)

  cls <- X[,ncol(X)]
  X <- X[,factcols]
  cols <- colnames(X)
  tbl <- table(cls)
  overall_ratio <- as.vector(tbl/sum(tbl))
  print ('overall ratio')
  print(overall_ratio)

  for (i in 1:length(factcols)){

    print (paste('ratio for column',cols[i]))
    dt <- X[,i]
    # run table command
    tbl <- as.matrix(table(dt,cls))*1.0
    # sum by row
    tbl_total <- apply(tbl,1,sum)
    # get ratios
    individ_ratio <- as.matrix(tbl/tbl_total)
    tbl_total <- as.matrix(tbl_total)
    colnames(tbl_total) <- c('TotalCount')
    # subtract from overall ratio
    ov_ratio <- abs(t(as.data.frame(apply(individ_ratio,1,function(x) {x-overall_ratio}))))
    individ_ratio <- cbind(ov_ratio,tbl_total)
    individ_ratio <- individ_ratio[order(individ_ratio[,'TotalCount'],decreasing=TRUE),]
    # make sure that all the values are around 0 especially for categories where row numbers are quite high
    print(individ_ratio)
  }
}
#just take any data frame and then check whether the columns are independent but it does only for fact cols
chiindepndencetestforfactcols <- function(X){
  require(discretization,quietly=TRUE)
  factcols <- findallfactorcols(X)
  cls <- X[,ncol(X)]
  X <- X[,factcols]
  cols <- colnames(X)
  pvals <- as.data.frame(t(c('NULL',0.0)))
  colnames(pvals) <- c('col1','col1_pvalue')
  pvals$col1 <- as.character(pvals$col1)
  pvals$col1_pvalue <- as.numeric(pvals$col1_pvalue)
  #check for each column whether it is independent of the class col
  for (i in 1:(ncol(X)-1)){
  ak <- chisq.test(table(X[,i],cls))
  pdtl <- c(cols[i],ak$p.value)
  pvals <- rbind(pvals,pdtl)

}
#removefirst val
pvals <- pvals[-1,]
list(colpvals= pvals)
}


cutcust <- function(X,numofbins= 100){
  if (is.data.frame(X)){
    print ("it works on vectors")
  }

  # if numofbins are more than data then just chnage it to length of data
  if (numofbins > length(X)){
    numofbins <- length(X)
  }

  # if num of bins are not multiple of 10 and are more than 10 then change it to low multiple of 10's
  modo <- numofbins%%10
  div <- numofbins/10
  if (modo > 0 & div > 0){
    numofbins<- 10*div
  }
  brks <- unique(quantile(X, probs=seq(0,1, by=(1/numofbins))))
  numofbins <- length(brks)
  labels <- (1:(numofbins-1))
  ct <- cut(X, breaks=brks, include.lowest=TRUE,labels = labels)
  ct
}
# equally distribute the data in 100 bins
EqualFreqDiscretization <- function(X,numofbins= 100)
{
  numcols <- findallnumcols(X)
  columnames <- colnames(X)
  X[,numcols] <- sapply(X[,numcols],cutcust,numofbins)
  X <- convertchartonum(X)

  X
}


#This might take lots of time so be careful especially when running for large data with too many distinct values
#it is based on chim discretization
dicretizecontdata <- function(X,alpha=0.05)
{
  require(discretization,quietly=TRUE)
  numcols <- findallnumcols(X)
  #first make sure that we do the unspervised discretization this is to avoid the performance issue for chim discretization
  # for large data
  X[,numcols] <- EqualFreqDiscretization(X[,numcols])

  #now run the chim discretization
  X[,numcols] <- chiM(X[,numcols],alpha)$Disc.data
  # chim returns integers. Convert these to numerics
  X <- convertinttonum(X)

  X
}

# it will do a chi square for all columns in a data frame even numeric columns. But numeric columns will be discretized first.
checkindependenceofdata <- function(X,alpha=0.005)
{
  # amke all charcols to factorial
  X <- convertchartofact(X)
  #make all integer cols to num
  X <- convertinttonum(X)
  # discretize the data for numeric cols
  numcols <- findallnumcols(X)
  print (numcols)
  columnames <- colnames(X)
  X_num <- X[,numcols]
  #discretize the data first
  X[,numcols] <- dicretizecontdata(X_num,alpha)

  #now convert all num cols to factorial
  #print( findclassofcolumns(X))
  X <- convertnumtofact(X)

  colpvals <- chiindepndencetestforfactcols(X)
  colpvals <- colpvals$colpvals
  list(colpvals=colpvals)
}


