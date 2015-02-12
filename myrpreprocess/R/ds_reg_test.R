train_log_reg <- function(frml1,train,test,...)
{
  frml1 <- as.formula(frml1)
  lm1 <- glm(frml1,data=train,family=binomial)
  #print(summary(lm1))
  test$predprob <- predict(lm1,test,type='response')
  list(test=test,model =lm1)
}

train_rf <- function(frml1,train,test,...)
{
  rf <- randomForest(frml1,train,importance=TRUE,...)
  test$predprob <- predict(rf,test,type='prob')[,2]
  test
}

roc_of_models <- function(test,clscolpos)
{
    lvls <- levels(test[,clscolpos])
    if (length(lvls) > 2){
      stop("More than 2 levels.ROC works with only 2 levels")
    }
    pred <- prediction(test[,(clscolpos+1)], test[,clscolpos])
    ## precision/recall curve (x-axis: recall, y-axis: precision)
    perf1 <- performance(pred, "prec", "rec")
    plot(perf1)
    ## sensitivity/specificity curve (x-axis: specificity,
    ## y-axis: sensitivity)
    perf1 <- performance(pred, "sens", "spec")
    plot(perf1)
    auc <- performance(pred,"auc")
    auc <- unlist(slot(auc, "y.values"))
    print (auc)
    perf <- performance(pred,"tpr","fpr")
    plot(perf)
}
