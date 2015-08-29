rm(list=ls())
#######################################################################
#                          Read Data  
#######################################################################
if (FALSE) {
  data.des <- read.csv("./data/mPOWEr/mpower_v1_desp.csv") 
  data.orig <- read.csv("./data/mPOWEr/mpower_v1_data.csv")
  data <- cbind(data.orig$pidnr, data.orig$postopday, data.orig$degreeexudate,
                data.orig$degreeslough, data.orig$granulation, data.orig$typeexudate, data.orig$typeslough)
  data <- as.data.frame(data)
  names(data) <- c("pidnr", "postopday", "degreeexudate", "degreeslough", "granulation", "typeexudate", "typeslough")
  
  # standarize
  for(i in 3:(ncol(data))) {
    data[,i] <- (data[,i]-mean(data[,i],na.rm = T))/sd(data[,i],na.rm = T)
  }
  # EWMA transformed
  ewma.loop <- function(rets, lambda) {
    n <- length(rets)+1
    sig.s <- rep(0, n)
    for(i in 2:n) {
      if(is.na(rets[i-1])) {
        sig.s[i] <- sig.s[i-1]
      } else {
        sig.s[i] <- sig.s[i-1]*lambda + (rets[i-1]^2)*(1-lambda)
      }
    }
    return(sqrt(tail(sig.s,n-1)))
  }
  data.ewma <- data
  for(i in unique(data[,1])) {
    for(j in 3:ncol(data)) {
      data.ewma[data[,1]==i,j] <- ewma.loop(data[data[,1]==i,j],lambda = 0.2)
    }
  }
  # reshape
  data.ewma <- as.matrix(data.ewma)
  data.ewma2 <- matrix(NA, ncol = 100)
  for(i in unique(data.ewma[,1])) {
    tmp <- as.vector(t(data.ewma[data.ewma[,1]==i,-c(1,2)]))
    if(length(tmp)!=100) next
    data.ewma2 <- rbind(data.ewma2, tmp)
  }
  
  rm(i,j,tmp) 
  # save data
  unlink("mPOWEr.RData")
  save.image(file = "mPOWEr.RData")
}

# load data
load(file = "./mPOWEr.RData")

#######################################################################
#               Testing & Training Set Construction  
# total 860 subjects
#######################################################################
no.test <- sample(nrow(data.ewma2), 300)
no.train <- setdiff(1:nrow(data.ewma2), no.test)
dat.test <- data.ewma2[no.test,]
dat.train <- data.ewma2[no.train,]

#######################################################################
#                  Computing the coefficients w  
#######################################################################
source(file = "./R/HealthIndex.R")
type = "linear"
coef <- solveHI(dat.train, nrow(dat.train), 20, 5, -1, type)

#######################################################################
#                         Health Index Construction
#######################################################################
ind.bs <- apply(test.bs, 1, 
                function(x) computeHI(train.all, x, coef, nrow(train.all), 3, 90, type))
ind.m6 <- apply(test.m6, 1,
                function(x) computeHI(train.all, x, coef, nrow(train.all), 3, 90, type))
ind.m12 <- apply(test.m12, 1,
                function(x) computeHI(train.all, x, coef, nrow(train.all), 3, 90, type))

num.test <- nrow(test.bs)
dat <- as.data.frame(cbind(c(1:num.test), ind.bs, ind.m6, ind.m12))

#######################################################################
#                         Visualization
#######################################################################
library(ggplot2)
num.ad <- nrow(ad.bs)
num.nl <- nrow(nl.bs)
# Full plot
ggplot(data = dat, aes(y = V1)) + 
  geom_point(aes(x = ind.bs), color = "green") + 
  geom_point(aes(x = ind.m6), color = "yellow") + 
  geom_point(aes(x = ind.m12), color = "red") +
  geom_hline(yintercept = 26)

#######################################################################
# apply rules to compute classification rate
#######################################################################
source(file = "./R/Rules.R")
source(file = "./R/measure.R")
class.correct <- rep("AD", nrow(dat))
class.correct[as.integer(row.names(dat)) >= num.ad] <- "normal"
dat <- dat[,-1]
class.pred <- rule.class(dat, 1.5)
accuracy(class.correct, class.pred)

class.pred <- rule.trend(dat, 0.3)
accuracy(class.correct, class.pred)

class.pred <- rule.class2(dat, 0.1, 1.8)
accuracy(class.correct, class.pred)

#######################################################################
# apply rules to compute AUC
#######################################################################