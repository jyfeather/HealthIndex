rm(list=ls())
#######################################################################
#                          Read Data  
#######################################################################
if (FALSE) {
  Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_31/')
  library(xlsx)
  ad.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_AD.xls", header = TRUE, sheetIndex = 1)
  ad.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_AD.xls", header = TRUE, sheetIndex = 1)
  ad.m12 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M12_AD.xls", header = TRUE, sheetIndex = 1)
#  mci.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_MCI.xls", header = TRUE, sheetIndex = 1)
#  mci.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_MCI.xls", header = TRUE, sheetIndex = 1)
#  mci.m12 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M12_MCI.xls", header = TRUE, sheetIndex = 1)
  nl.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_NL.xls", header = TRUE, sheetIndex = 1)
  nl.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_NL.xls", header = TRUE, sheetIndex = 1)
  nl.m12 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M12_NL.xls", header = TRUE, sheetIndex = 1)
  #ad.bs[,1] == ad.m6[,1] # check ID consistency
  # Whole dataset
  dat.bs <- rbind(ad.bs[,c(-1,-2)], nl.bs[,-1])
  dat.m6 <- rbind(ad.m6[,-1], nl.m6[,-1])
  dat.m12 <- rbind(ad.m12[,-1], nl.m12[,-1])
  rid <- c(ad.m12[,1], nl.m12[,1])
  # First 90 regions of interest
  dat.bs <- dat.bs[,1:90]
  dat.m6 <- dat.m6[,1:90]
  dat.m12 <- dat.m12[,1:90]  
  omega.name <- read.table(file = "./data/ADNI/aal.txt", sep = " ")
  # save data
  unlink("ADNI.RData")
  save.image(file = "ADNI.RData")
}

# load data
load(file = "./ADNI.RData")

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
type = "gaussian"
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
for(val in seq(0,2,by = 0.1)) {
  class.pred <- rule.class(dat, val)
  print(accuracy(class.correct, class.pred))
}

for(val in seq(0,2,by = 0.1)) {
  class.pred <- rule.trend(dat, val)
  print(accuracy(class.correct, class.pred))
}

for(val1 in seq(-0.5,1,by = 0.1)) {
  for(val2 in seq(0,3,by = 0.5)) {
    class.pred <- rule.class2(dat, val1, val2)
    print(accuracy(class.correct, class.pred))
  }
}
#######################################################################
# apply rules to compute AUC
#######################################################################
