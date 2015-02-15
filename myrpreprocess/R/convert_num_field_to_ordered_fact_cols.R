#Here we will try to reduce the factors of the columns so that independet factors could be combined and we could reduce the number of levels
# It would be a supervised method.Thus, it need class info and should be done only on train data and do not do it on the test and valid data.

convert_num_field_to_ordered_fact_cols <- function(X,numcols_to_be_converted,alpha=0.05)
{
  cls <- X[,ncol(X)]
  clscolname <- colnames(ncol(X))
  X <- X[,numcols_to_be_converted]
  X <- round(X)
  cols <- colnames(X)
  cols <- append(cols,clscolname)
  X <- cbind(X,cls)
  colnames(X) <- cols

  #Now use chimerge. This is for the columns which has very few distinct values. These will be converted to ordered factors but here these will be converted
  # to even fewer levels
  X <- chiM(X,alpha)$Disc.data
  #do not return class column
  X <- X[,-ncol(X)]
  X <- as.data.frame(lapply(X,function(cl) ordered(as.numeric(as.character(cl)))))
  X
}


