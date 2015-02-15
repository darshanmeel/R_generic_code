#Below methods try to find the best features using Logistic Regrssion and using AUC. I will generalize them further to accpet the algorithm and the metric type

#Run this with small number of rows and when columns are around say 40-50. If data is large run it with smaller number of columns
brute_force_lr <- function(X,Y)
{
  #loop through all the columns and add the columsn with best p value
  cols <- colnames(X)
  ncol <- length(cols)
  mainfrml <- paste(cols[ncol],'~')
  colrange <- ncol-1
  singlecols <- as.data.frame(t(c(as.numeric(0),as.numeric(0.0))))
  colnames(singlecols) <- c('col1','col1_pvalue')
  singlecols$col1 <- as.numeric(as.character(singlecols$col1))
  singlecols$col1_pvalue <- as.numeric(as.character(singlecols$col1_pvalue))
  cond <- TRUE
  prvpval <- as.numeric(0)
  bstpval <- as.numeric(0.2)
  while(bstpval > prvpval)
  {
    singlecols <- singlecols[1==2,c(1,2)]
    prvpval <- bstpval
    ncol <- length(cols)
    colrange <- ncol-1
    for (i in 1:colrange)
    {
      #generate the model for a single column only.See the p values
      frml <- paste(mainfrml,cols[i])
      print (frml)
      #lm1 <- glm(as.formula(frml),data=X[,c(i,ncol)],family=binomial)
      #ab <- summary(lm1)$coefficients
      tst <-train_and_predict_log_reg_and_ret_auc(frml,X,Y,predict_type='response')
      pvalvector <- c(i)

      pvalvector <- append(pvalvector,tst$auc)
      pvalvector <- as.data.frame(pvalvector,stringsAsFactors=FALSE)
      colnames(pvalvector) <- c('col1','col1_pvalue')
      singlecols <-rbind(singlecols,pvalvector)


    }
    # find max value
    bstpvalpos <- which.max(singlecols[,2])
    bstpval <- as.numeric(as.character(singlecols[bstpvalpos,2]))
    print(bstpval)
    bstcol <- singlecols[bstpvalpos,1]
    print(bstcol)
    mainfrml <- paste(mainfrml,cols[bstcol],'+')
    cols <- cols[-bstcol]
  }
  print (mainfrml)
  print(bstpval)
}
#brute_force_lr(train_data,valid_data)
#
fc <- function(X,cl)
{
  xps <- match(cl,X) + 1
  ab <- paste0(cl,':',X[seq(xps,length(X))])
  ab
}


#do not run this method if you have more than say 20 columns. It will be seriously slow and if your data is large than run it upto 10-15 columns.
#here you can proivde the formula as obtained by single columns above or a blank formula.
brute_force_lr_interaction <- function(X,Y,frml1)
{
  #loop through all the columns and add the columsn with best p value
  cols <- colnames(X)
  cols <- cols[-length(cols)]
  cols <- unlist(sapply(cols[-length(cols)], function(cl) fc(cols,cl)))
  ncol <- length(cols)
  mainfrml <- frml1
  singlecols <- as.data.frame(t(c(as.numeric(0),as.numeric(0.0))))
  colnames(singlecols) <- c('col1','col1_pvalue')
  singlecols$col1 <- as.numeric(as.character(singlecols$col1))
  singlecols$col1_pvalue <- as.numeric(as.character(singlecols$col1_pvalue))
  cond <- TRUE
  prvpval <- as.numeric(0)
  bstpval <- as.numeric(0.2)
  while(bstpval > prvpval)
  {
    singlecols <- singlecols[1==2,c(1,2)]
    prvpval <- bstpval
    ncol <- length(cols)
    colrange <- ncol
    for (i in 1:colrange)
    {
      #generate the model for a single column only.See the p values
      frml <- paste(mainfrml,cols[i])
      print (frml)
      #lm1 <- glm(as.formula(frml),data=X[,c(i,ncol)],family=binomial)
      #ab <- summary(lm1)$coefficients
      print ("here")
      tst <-train_and_predict_log_reg_and_ret_auc(as.formula(frml),X,Y,predict_type='response')
      pvalvector <- c(i)

      pvalvector <- append(pvalvector,tst$auc)
      pvalvector <- as.data.frame(pvalvector)
      colnames(pvalvector) <- c('col1','col1_pvalue')
      singlecols <-rbind(singlecols,pvalvector)


    }
    # find max value
    bstpvalpos <- which.max(singlecols[,2])
    bstpval <- as.numeric(as.character(singlecols[bstpvalpos,2]))
    print(bstpval)
    bstcol <- singlecols[bstpvalpos,1]
    print(bstcol)
    mainfrml <- paste(mainfrml,cols[bstcol],'+')
    cols <- cols[-bstcol]
  }
  print (mainfrml)
  print(bstpval)
}

#brute_force_lr(train_data,valid_data)
#frml1 <- 'Class ~ '

#brute_force_lr_interaction(train_data,valid_data,frml1)


#This methdo will run once and return the auc values for all interaction terms.
#Then we can select the interaction terms which we want say top 20 or 30
brute_force_lr_interaction_one_pass <- function(X,Y,frml1)
{
  #loop through all the columns and add the columsn with best p value
  cols <- colnames(X)
  cols <- cols[-length(cols)]
  cols <- unlist(sapply(cols[-length(cols)], function(cl) fc(cols,cl)))
  ncol <- length(cols)
  mainfrml <- frml1
  singlecols <- as.data.frame(t(c(as.numeric(0),as.numeric(0.0))))
  colnames(singlecols) <- c('col1','col1_pvalue')
  singlecols$col1 <- as.numeric(as.character(singlecols$col1))
  singlecols$col1_pvalue <- as.numeric(as.character(singlecols$col1_pvalue))
  cond <- TRUE
  prvpval <- as.numeric(0)
  bstpval <- as.numeric(0.2)
  run <- 1

    ncol <- length(cols)
    colrange <- ncol
    for (i in 1:colrange)
    {
      #generate the model for a single column only.See the p values
      frml <- paste(mainfrml,cols[i])
      print (frml)
      frml <- as.formula(frml)
      #lm1 <- glm(as.formula(frml),data=X[,c(i,ncol)],family=binomial)
      #ab <- summary(lm1)$coefficients
      print ("here")
      tst <-train_and_predict_log_reg_and_ret_auc(frml,X,Y,predict_type='response')
      pvalvector <- c(i)

      pvalvector <- append(pvalvector,tst$auc)
      pvalvector <- as.data.frame(pvalvector)
      colnames(pvalvector) <- c('col1','col1_pvalue')
      singlecols <-rbind(singlecols,pvalvector)



  }
  print (mainfrml)
  # find max value
  list(singlecols=singlecols)
}
frml1 <- 'Class ~ '
sgl <- brute_force_lr_interaction_one_pass(train_data,valid_data,frml1)
#once you haveselected top interaction terms. Thenrun these here along with our formula for all individual columns and see if these increase the auc any more.
brute_force_lr_interaction_top_only <- function(X,Y,frml1)
{
  #loop through all the columns and add the columsn with best p value
  cols <- colnames(X)
  cols <- cols[-length(cols)]
  cols <- unlist(sapply(cols[-length(cols)], function(cl) fc(cols,cl)))
  cols<- as.vector(cols[sgl1[1:30,1]])
  ncol <- length(cols)
  mainfrml <- frml1
  singlecols <- as.data.frame(t(c(as.numeric(0),as.numeric(0.0))))
  colnames(singlecols) <- c('col1','col1_pvalue')
  singlecols$col1 <- as.numeric(as.character(singlecols$col1))
  singlecols$col1_pvalue <- as.numeric(as.character(singlecols$col1_pvalue))
  cond <- TRUE
  prvpval <- as.numeric(0)
  bstpval <- as.numeric(0.2)
  run <- 1
  while(bstpval > prvpval)
  {
    singlecols <- singlecols[1==2,c(1,2)]
    prvpval <- bstpval
    ncol <- length(cols)
    colrange <- ncol
    for (i in 1:colrange)
    {
      #generate the model for a single column only.See the p values
      frml <- paste(mainfrml,cols[i])
      print (frml)
      frml <- as.formula(frml)
      #lm1 <- glm(as.formula(frml),data=X[,c(i,ncol)],family=binomial)
      #ab <- summary(lm1)$coefficients
      print ("here")
      tst <-train_and_predict_log_reg_and_ret_auc(frml,X,Y,predict_type='response')
      pvalvector <- c(i)

      pvalvector <- append(pvalvector,tst$auc)
      pvalvector <- as.data.frame(pvalvector)
      colnames(pvalvector) <- c('col1','col1_pvalue')
      singlecols <-rbind(singlecols,pvalvector)


    }
    # find max value
    bstpvalpos <- which.max(singlecols[,2])
    bstpval <- as.numeric(as.character(singlecols[bstpvalpos,2]))
    print(bstpval)
    bstcol <- singlecols[bstpvalpos,1]
    print(bstcol)
    mainfrml <- paste(mainfrml,cols[bstcol],'+')
    cols <- cols[-bstcol]

  }
    print (mainfrml)
    # find max value
    list(singlecols=singlecols)
}

#brute_force_lr(train_data,valid_data)
frml1 <- 'Class ~ Loan.Term_0 + Annual.Income + FICO.Credit.Score_0 + Use.Of.Credit.Line_3 + Loan.Purpose_0 + FICO.Credit.Score_4 + Use.Of.Credit.Line_4 + No..Adverse.Public.Records_0 + Loan.Amount + Total.Number.Of.Credit.Lines_0 + Debt.To.Income.Ratio_0 + FICO.Credit.Score_1 + No..Inquiries.In.Last.6.Months_0 + No..Of.Public.Record.Bankruptcies_0 + Address.State_3 + Address.State_4 + Earliest.Credit.Line.Opened + Home.Ownership_0 + Use.Of.Credit.Line_0 + No..Delinquencies.In.Last.2.Years_0 + Total.Credit.Balance + Loan.Term_1 +'
sgl5 <- brute_force_lr_interaction_top_only(train_data,valid_data,frml1)
