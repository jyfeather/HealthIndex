#######################################################################
#                         Input
#######################################################################
library(xlsx)
ad.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_AD.xls", header = TRUE, sheetIndex = 1)
ad.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_AD.xls", header = TRUE, sheetIndex = 1)
ad.m12 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M12_AD.xls", header = TRUE, sheetIndex = 1)
#ad.bs[,1] == ad.m6[,1] # check ID consistency
ad.bs <- ad.bs[,c(-1,-2)]
ad.m6 <- ad.m6[,-1]
ad.m12 <- ad.m12[,-1]

#######################################################################
#                         Quadratic Formulation  
#######################################################################
num.sub <- nrow(ad.bs) # num of subjects is 74
num.epo <- 3 # num of epochs is 3, baseline, m06, m12
num.aal <- ncol(ad.bs) # num of regions of interests
library(quadprog)
H <- matrix(0, nrow = (num.aal + num.sub*(num.epo-1)), ncol = (num.aal + num.sub*(num.epo-1)))
diag(H)[1:num.aal] = 1
l <- c(rep(0, num.aal), rep(1, num.sub*(num.epo-1)))
e1 <- ad.m6 - ad.bs
e2 <- ad.m12 - ad.m6
E <- rbind(e1, e2)
E <- cbind(E, diag(num.sub*(num.epo-1)))
pn <- matrix(0, nrow = num.sub*(num.epo-1), ncol = num.aal)
pn <- cbind(pn, diag(num.sub*(num.epo-1)))
E <- rbind(as.matrix(E), pn)
b0 <- rep(0, nrow(E))
solve.QP(Dmat = 2*H, dvec = -l, Amat = t(E), bvec = b0)
