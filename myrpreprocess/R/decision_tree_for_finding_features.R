#This is a decision tree which works on the data and will show you the useful trees
# This might generate lots of output and thus. Make sure that you output to docuemnt using rmd and then
# view the output at your leisure.

decision_tree_for_finding_features <- function(X,cp = 0.001,...)
{
  require(rpart,quietly=TRUE)
  cols <- colnames(X)
  clpos <- length(cols)-1
  mfrml <- 'Class ~ '
  frml <- ''
  frml1 <- ''
  for (i in 1:clpos)
  {
    print (paste('generate tree for column',cols[i],' starts'))
    k = i+1
    frml <- paste(mfrml,cols[i])
    ft <- rpart(as.formula(frml), data = X,control = rpart.control(cp = cp))
    par(mfrow = c(1,2), xpd = NA)
    tryCatch(eval(
                {
                  print(plot(ft))
                  print(text(ft, use.n = TRUE))
                  printcp(ft)
                }
                    )
                ,error= function(e) print("not good tree"))

  }


  mfrml <- 'Class ~ '
  frml <- ''
  frml1 <- ''
  for (i in 1:clpos)
  {
    print (paste('generate tree for column',cols[i],' starts'))
    k = i+1
    frml <- paste(mfrml,cols[i])
    for (j in k:clpos){
      print(frml1)
      frml1 <- paste(frml,'+',cols[j])
      ft <- rpart(as.formula(frml1), data = X,control = rpart.control(cp = cp))
      par(mfrow = c(1,2), xpd = NA)
      tryCatch(eval(
              {
               print(plot(ft))
               print(text(ft, use.n = TRUE))
               printcp(ft)
                }
               )
               ,error= function(e) print("not good tree"))

    }
  }
}
