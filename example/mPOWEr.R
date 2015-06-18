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
      data.ewma[data$V1==i,j] <- ewma.loop(data[data$V1==i,j],lambda = 0.2)
    }
  }
  rm(i,j) 
  # save data
  unlink("mPOWEr.RData")
  save.image(file = "mPOWEr.RData")
}

# load data
load(file = "./mPOWEr.RData")

#######################################################################
#               Testing & Training Set Construction  
#######################################################################
num.sub <- nrow(dat.bs) # num of subjects is 324 
dat.no <- c(1:num.sub)
num.epo <- 3 # num of epochs is 3, baseline, m06, m12
num.aal <- ncol(dat.bs) # num of regions of interests

# 3 - folder Cross Validation 
folder.1 <- sample(num.sub, round(1/3*num.sub), replace = FALSE)
folder.2 <- sample(setdiff(dat.no, folder.1), round(1/3*num.sub), replace = FALSE)
folder.3 <- setdiff(dat.no, union(folder.1, folder.2))

train.no <- union(folder.2, folder.3)
test.no <- folder.1; test.no <- sort(test.no)

train.bs <- dat.bs[train.no,]
train.m6 <- dat.m6[train.no,]
train.m12 <- dat.m12[train.no,]

test.bs <- dat.bs[test.no,]
test.m6 <- dat.m6[test.no,]
test.m12 <- dat.m12[test.no,]

#######################################################################
#                  Computing the coefficients w  
#######################################################################
source(file = "./R/HealthIndex.R")
type = "linear"
train.all <- merge.all(train.bs, train.m6, train.m12)
coef <- solveHI(train.all, nrow(train.all), 3, 90, -1, type)

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
library(ROCR)
