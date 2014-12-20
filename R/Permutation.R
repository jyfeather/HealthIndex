rm(list=ls())
#######################################################################
#                         Input
#######################################################################
load("./ADNI.RData")

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
  if (per >= 0.8532) sig <- sig + 1
}

library(ggplot2)
viz.dat <- as.data.frame(cbind(x=c(1:length(perset)), y=perset))
ggplot(data = viz.dat) + geom_density(aes(y)) + geom_vline(aes(xintercept = 0.8532)) +
  xlab("Percentage of Monotone")
