rm(list=ls())
#######################################################################
#                         Input
#######################################################################
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_25/')
library(xlsx)
mci.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_MCI.xls", header = TRUE, sheetIndex = 1)
mci.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_MCI.xls", header = TRUE, sheetIndex = 1)
mci.bs <- cbind(mci.bs[,-1], y=1)
mci.m6 <- cbind(mci.m6[,-1], y=-1)
dat <- rbind(mci.bs, mci.m6)
dat$y <- as.factor(dat$y)
train.no <- sample(c(1:nrow(dat)), 1/2 * nrow(dat))
dat.train <- dat[train.no,]
dat.test <- dat[-train.no,]
y.test <- dat.test[,ncol(dat.test)]
dat.test <- dat.test[,-ncol(dat.test)]

#######################################################################
#                         Classification Model  
#######################################################################
library(e1071)
model <- svm(y~., data = dat.train, kernel = "sigmoid")
pred <- predict(model, dat.test)
table(pred, y.test)
err <- sum(abs(as.numeric(pred) - as.numeric(y.test))) / nrow(dat.test)

