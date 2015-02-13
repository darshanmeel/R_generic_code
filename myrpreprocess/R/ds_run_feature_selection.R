# I HAVE GIVEN LONG NAMES SO THAT I CAN LOOK AT METHOD AND KNOW WHAT IT IS.IN PROD CODE YOU WONT HAVE THIS MUCH LONG NAMES
# RATHER YOU CAN CUT THESE A BIT SORT :)

# DO NOT USE ANY OF THESE METHODS DIRECTLY AS IT EXPECT OUTPUTS TO BE IN CERTAIN FORMAT.

#As this is a supervised method I am assuming thta you have class/response column.
# Here k means how many parameters to bring
# ranking means what ranking to use for random forest. You can pass random forest parameters as well.
findusefulfeatures <- function(X,alpha=0.05,ranking='accuracy',k=20...)
{

  # Do a chisquare independece test
  ci <- checkindependenceofdata(X,alpha=alpha)
  colpvals <- ci$colpvals
  print (colpvals)
  # Do a random forest
  rf <- rf_feature_selection (X,frml,k=k,ranking=ranking,...)
  print (paste('top ranked features based on random forest',ranking))
  print(rf$feature_rank)
  # Thius might generate huge data so you might not want to use it.
  #print(rf$imp_ordered)
  print ("now rankinsg based in logistic regression")

  lr <- lr_feature_selection(X)
  singlecols <- lr$singlecols
  print ("importance of single columns")
  singlecols <- singlecols[order(singelcols[,2]),]
  print(singlecols[1:k,])
  interactioncols <- lr$interactioncols
  interactioncols <- interactioncols[order(interactioncols[,5]),]
  print(interactioncols[1:k,])
}
