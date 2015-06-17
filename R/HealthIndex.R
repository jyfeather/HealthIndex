library(quadprog) # this is a quadratic problem

#################################################
# using kernel trick, support
# - linear
# - gaussian
# - polynomial
#################################################
kernel <- function(x, y, type = "linear") {
  switch(type,
         linear     =  x%*%y,
         gaussian   =  exp(-sum((x-y)^2)/2),
         polynomial =  (x%*%y+1)^2)
}

#################################################
## compute the kernel matrix in the Lagrange dual
## computeK = k(x2,y2)-k(x1,y1)-k(x2,y1)+k(x1,y1)
#################################################
computeK <- function(x1, x2, y1, y2, type = "linear") {
  return(kernel(x1,y1,type)-kernel(x1,y2,type)-kernel(x2,y1,type)+kernel(x2,y2,type))  
}

#################################################
## merge dataset by observations
#################################################
merge.all <- function(x, ..., by = "row.names") {
  L <- list(...)
  for (i in seq_along(L)) {
    x <- merge(x, L[[i]], by = by)
    rownames(x) <- x$Row.names
    x$Row.names <- NULL
  }
  return(x)
}

#################################################
## construct D
## Currently, num.epoch is euqal
#################################################
Kmat <- function(data, num.obs, num.epoch, num.var, type = "linear") {
  data <- as.matrix(data)
  num <- num.obs*(num.epoch-1)
  Dmat <- matrix(NA, nrow = num, ncol = num)
  for(i in 1:num) {
    # generate x1, x2
    x.i <- (i+1)%/%(num.epoch-1) # current subject
    x.j <- (i-1)%%(num.epoch-1)    # current time
    x1 <- data[x.i, (x.j*num.var+1):(x.j*num.var+num.var)]  
    x2 <- data[x.i, ((x.j+1)*num.var+1):((x.j+1)*num.var+num.var)]  
    # generate y1, y2  
    for(j in 1:i) {
      y.i <- (j+1)%/%(num.epoch-1)
      y.j <- (j-1)%%(num.epoch-1)
      y1 <- data[y.i, (y.j*num.var+1):(y.j*num.var+num.var)]  
      y2 <- data[y.i, ((y.j+1)*num.var+1):((y.j+1)*num.var+num.var)]      
      Dmat[i,j] <- computeK(x1, x2, y1, y2, type)
      Dmat[j,i] <- Dmat[i,j]
    }
  }
  return(Dmat)
}

#################################################
## C should be negavtive, determined by CV
#################################################
solveHI <- function(data, num.obs, num.epoch, num.var, C, type = "linear") {
  num <- num.obs*(num.epoch-1)
  Dmat <- Kmat(data, num.obs, num.epoch, num.var, type) 
  dvec <- rep(1, num)
  bvec <- c(rep(0, num), rep(C, num))
  Amat <- rbind(diag(num), -diag(num))
  
  # find nearest positive definite matrix since Dmat is not always PD
  require(Matrix)
  Dmat_PD <- as.matrix(nearPD(Dmat)$mat)
  
  result <- solve.QP(Dmat = Dmat_PD, dvec = dvec, Amat = t(Amat), bvec = bvec)
  return(result$solution)  
}

#################################################
## test is one observation
#################################################
computeHI <- function(train, test, coef, num.obs, num.epoch, num.var, type = "linear") {
  train <- as.matrix(train)
  test <- as.matrix(test)
  num <- num.obs*(num.epoch-1)  
  index <- 0
  for(i in 1:num) {
    # generate x1, x2
    x.i <- (i+1)%/%(num.epoch-1) # current subject
    x.j <- (i-1)%%(num.epoch-1)    # current time
    x1 <- train[x.i, (x.j*num.var+1):(x.j*num.var+num.var)]  
    x2 <- train[x.i, ((x.j+1)*num.var+1):((x.j+1)*num.var+num.var)]  
    
    index <- index + coef[i]*(kernel(x2,test)-kernel(x1,test))  
  }
  return(index)
}