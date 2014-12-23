diff2 <- function(df) {
  return(cbind(df[,2]-df[,1], df[,3]-df[,2], df[,3]-df[,1]))
}

# Level 1: bl - m06 - m12  + + +
# Level 2: bl - m12 - m06  + - +
#          m06 - bl - m12  - + +
#          m12 - bl - m06  + - -
# Level 3: m06 - m12 - bl  - + -
#          m12 - m06 - bl  - - -
check <- function(dif) {
  extent <- apply(dif, 1, function(x) {
    sig <- 0
    if (min(x) >= 0) {
      sig <- 1
    } else if (x[1] < 0 && x[3] < 0) {
      sig <- 3 
    } else {
      sig <- 2 
    }
    return(sig)
  })
  return(table(extent))
}