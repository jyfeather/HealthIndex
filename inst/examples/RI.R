rm(list=ls())
#######################################################################
#                         Input
#######################################################################
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
dat.bs <- rbind(ad.bs[,c(-1,-2)], mci.bs[,-1], nl.bs[,-1])
dat.m6 <- rbind(ad.m6[,-1], mci.m6[,-1], nl.m6[,-1])
dat.m12 <- rbind(ad.m12[,-1], mci.m12[,-1], nl.m12[,-1])

#######################################################################
#                         Quadratic Formulation  
#######################################################################
num.sub <- nrow(dat.bs) # num of subjects is 74
num.epo <- 3 # num of epochs is 3, baseline, m06, m12
num.aal <- ncol(dat.bs) # num of regions of interests
train.no <- sample(num.sub, round(2/3*num.sub))
train.bs <- dat.bs[train.no,]
train.m6 <- dat.m6[train.no,]
train.m12 <- dat.m12[train.no,]
test.bs <- dat.bs[-train.no,]
test.m6 <- dat.m6[-train.no,]
test.m12 <- dat.m12[-train.no,]
train.sub <- nrow(train.bs)

#library(quadprog)
H <- matrix(0, nrow = (num.aal + train.sub*(num.epo-1)), ncol = (num.aal + train.sub*(num.epo-1)))
diag(H)[1:num.aal] = 1
l <- c(rep(0, num.aal), rep(1, train.sub*(num.epo-1)))
e1 <- train.m6 - train.bs
e2 <- train.m12 - train.m6
E <- rbind(e1, e2)
E <- cbind(E, diag(train.sub*(num.epo-1)))
pn <- matrix(0, nrow = train.sub*(num.epo-1), ncol = num.aal)
pn <- cbind(pn, diag(train.sub*(num.epo-1)))
E <- rbind(as.matrix(E), pn)
b0 <- rep(0, nrow(E))
#solve.QP(Dmat = 2*H, dvec = -l, Amat = t(E), bvec = b0)

#######################################################################
#                         Use matlab to solve this problem  
#######################################################################

# pass coefficients to matlab to solve this problem
write.csv(H, file = "./inst/dat/H.csv", row.names = FALSE, col.names = FALSE)
write.csv(l, file = "./inst/dat/l.csv", row.names = FALSE, col.names = FALSE)
write.csv(E, file = "./inst/dat/E.csv", row.names = FALSE, col.names = FALSE)
write.csv(b0, file = "./inst/dat/b0.csv", row.names = FALSE, col.names = FALSE)

res <- read.csv(file = "./inst/dat//res.csv", header = FALSE)
omega <- res[1:num.aal,]

#######################################################################
#                         Health Index Construction
#######################################################################
ind.bs <- as.matrix(test.bs) %*% omega
ind.m6 <- as.matrix(test.m6) %*% omega
ind.m12 <- as.matrix(test.m12) %*% omega
test.sub <- nrow(test.bs)
library(ggplot2)
dat <- as.data.frame(cbind(c(1:test.sub), ind.bs, ind.m6, ind.m12))
# Full plot
ggplot(data = dat) + geom_point(aes(y = V1, x = V2), colour = "#FFCC00") + 
  geom_point(aes(y = V1, x = V3), colour = "#0033CC") + 
  geom_point(aes(y = V1, x = V4), colour = "#CC0033") + 
  scale_x_continuous(name="Health Index Value") +
  scale_y_continuous(name="Subject No.", breaks = c(1:nrow(dat))) +
  ggtitle("Health Index, Yellow = baseline, Blue = m06, Red = m12")

# Less subjects, clearer plot
dat2 <- dat[sample(c(1:test.sub), 10),] # for clear visualization
ggplot(data = dat2) + geom_point(aes(y = V1, x = V2), colour = "#FFCC00", size = 10) + 
  geom_point(aes(y = V1, x = V3), colour = "#0033CC", size = 10) + 
  geom_point(aes(y = V1, x = V4), colour = "#CC0033", size = 10) + 
  scale_x_continuous(name="Health Index Value") +
  scale_y_continuous(name="Subject No.") +
  ggtitle("Health Index, Yellow = baseline, Blue = m06, Red = m12")
