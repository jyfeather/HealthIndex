############################################
# discriminative value
############################################
rule.class <- function(data, value) {
  class.pred <- rep("normal", nrow(data))
  class.pred[apply(data, 1, function(x) any(x>=value))] <- "AD"
  return(class.pred)
}

############################################
# two discriminative values
# at least value 1
# greater than value 2
############################################
rule.class2 <- function(data, value1, value2) {
  class.pred <- rep("normal", nrow(data))
  for(i in 1:nrow(data)) {
    min <- min(data[i,])    
    max <- max(data[i,])
    if(min>=value1 && max>=value2) class.pred[i] <- "AD"
  }
  return(class.pred)
}

############################################
# change range
############################################
rule.trend <- function(data, value) {
  data <- as.matrix(data)
  require(matrixStats)
  dif <- rowDiffs(data)
  class.pred <- rep("normal", nrow(data))
  class.pred[rowMeans(dif) >= value] <- "AD"
  return(class.pred)
}