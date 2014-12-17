rm(list=ls())
#######################################################################
#                         Input
#######################################################################
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_25/')
library(xlsx)
ad.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_AD.xls", header = TRUE, sheetIndex = 1)
ad.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_AD.xls", header = TRUE, sheetIndex = 1)
ad.m12 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M12_AD.xls", header = TRUE, sheetIndex = 1)
mci.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_MCI.xls", header = TRUE, sheetIndex = 1)
mci.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_MCI.xls", header = TRUE, sheetIndex = 1)
mci.m12 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M12_MCI.xls", header = TRUE, sheetIndex = 1)
nl.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_NL.xls", header = TRUE, sheetIndex = 1)
nl.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_NL.xls", header = TRUE, sheetIndex = 1)
nl.m12 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M12_NL.xls", header = TRUE, sheetIndex = 1)
#ad.bs[,1] == ad.m6[,1] # check ID consistency
# Whole dataset
dat.bs <- rbind(ad.bs[,c(-1,-2)], mci.bs[,-1], nl.bs[,-1])
dat.m6 <- rbind(ad.m6[,-1], mci.m6[,-1], nl.m6[,-1])
dat.m12 <- rbind(ad.m12[,-1], mci.m12[,-1], nl.m12[,-1])
no.set <- nrow(dat.bs)

no.permutation <- 1000
no.sample <- 150
source("./R/TrendStat.R")
sig <- 0
perset <- c()
for (i in 1:no.permutation) {
  shuffle <- sample(1:no.set, no.sample)  
  idx.bl <- rowSums(dat.bs[shuffle,])
  idx.m6 <- rowSums(dat.m6[shuffle,])
  idx.m12 <- rowSums(dat.m12[shuffle,])
  idx.diff <- diff2(as.data.frame(cbind(idx.bl, idx.m6, idx.m12)))
  idx.check <- check(idx.diff)
  idx.check <- as.data.frame(idx.check)
  per <- (idx.check[1,2] + idx.check[2,2]) / nrow(idx.diff)
  perset <- c(perset, per)
  if (per >= 0.8349) sig <- sig + 1
}

library(ggplot2)
viz.dat <- as.data.frame(cbind(x=c(1:length(perset)), y=perset))
ggplot(data = viz.dat) + geom_density(aes(y)) + geom_vline(aes(xintercept = 0.8349))
