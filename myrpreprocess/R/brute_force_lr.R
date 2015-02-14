#brute force lr

brute_force_lr <- function(X,Y)
{
  #loop through all the columns and add the columsn with best p value
  cols <- colnames(X)
  ncol <- length(cols)
  mainfrml <- paste(cols[ncol],'~')
  colrange <- ncol-1
  singlecols <- as.data.frame(t(c(as.character('NULL'),as.numeric(0.0))))
  colnames(singlecols) <- c('col1','col1_pvalue')
  singlecols$col1 <- as.character(singlecols$col1)
  singlecols$col1_pvalue <- as.numeric(singlecols$col1_pvalue)
  cond <- TRUE
  prvpval <- 0
  bstpval <- 0.2
  while(bstpval > prvpval)
  {
    prvpval <- bstpval
    for (i in 1:colrange)
    {
      #generate the model for a single column only.See the p values
      frml <- paste(mainfrml,cols[i])
      #lm1 <- glm(as.formula(frml),data=X[,c(i,ncol)],family=binomial)
      #ab <- summary(lm1)$coefficients
      tst <-train_and_predict_log_reg_and_ret_auc(frml1,X,Y,predict_type='response')
      pvalvector <- c(cols[i])
      pvalvector <- append(pvalvector,tst[[2]])
      singlecols <-rbind(singlecols,pvalvector)
    }
    # find max value
    bstpvalpos <- which.max(singelcols[,2])
    bstpval <- singlecols[bstpvalpos,1]
    print(bstpval)
    bstcol <- singlecols[bstpvalpos,1]
    print(bstcol)

  }
}
