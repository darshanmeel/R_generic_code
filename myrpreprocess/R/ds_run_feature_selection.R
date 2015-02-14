# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)

# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.

#As this is a supervised method I am assuming thta you have class/response column.
# Here k means how many parameters to bring
# ranking means what ranking to use for random forest. You can pass random forest parameters as well.
findusefulfeatures <- function(X,alpha=0.05,ranking='accuracy',k=20,frml1,...)
{
  k <- ifelse(ncol(X) < k,ncol(X),k)
  # Do a chisquare independece test
  ci <- checkindependenceofdata(X,alpha=alpha)
  colpvals <- ci$colpvals
  print (colpvals)
  # Do a random forest
  rf <- rf_feature_selection (X,as.formula(frml1),k=k,ranking=ranking,...)
  print (paste('top ranked features based on random forest',ranking))
  print(rf$feature_rank)
  # Thius might generate huge data so you might not want to use it.
  #print(rf$imp_ordered)
  print ("now rankinsg based on logistic regression. This is custom log reg")

  lr <- lr_feature_selection(X)
  singlecols <- lr$singlecols
  print ("importance of single columns")
  #singlecols <- singlecols[order(singlecols[,2]),]
  print(singlecols)
  interactioncols <- lr$interactioncols
  interactioncols <- interactioncols[order(interactioncols[,5]),]
  print(interactioncols)
  # Finally find the columns where the individual factor levels could be independent and thus these could be merged.
  # These are useful especially when we will need to reduce the levels.
  pvals <- reduce_levels_for_unord_fact_cols(X)
  print(pvals)

  # These are used for all the columns which are ordered factor columns
  pvals <- reduce_levels_for_ordered_fact_cols(X)
  print(pvals)

}
