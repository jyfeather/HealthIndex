#############################################
# classification accuracy
# (TP+TN)/(TP+TN+FP+FN)
#############################################
accuracy <- function(true, pred) {
  accu <- sum(true==pred) / length(true)
  return(accu)
}

#############################################
# 
#############################################