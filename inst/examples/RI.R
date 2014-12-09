rm(list=ls())
#######################################################################
#                         Input
#######################################################################
library(xlsx)
ad.bs <- read.xlsx(file = "./data/ADNI/ADNIaalBM_BSL_NL.xls", header = TRUE, sheetIndex = 1)
ad.m6 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M06_NL.xls", header = TRUE, sheetIndex = 1)
ad.m12 <- read.xlsx(file = "./data/ADNI/ADNIaalBM_M12_NL.xls", header = TRUE, sheetIndex = 1)
#ad.bs[,1] == ad.m6[,1] # check ID consistency
ad.bs <- ad.bs[,c(-1)]
ad.m6 <- ad.m6[,-1]
ad.m12 <- ad.m12[,-1]

#######################################################################
#                         Quadratic Formulation  
#######################################################################
num.sub <- nrow(ad.bs) # num of subjects is 74
num.epo <- 3 # num of epochs is 3, baseline, m06, m12
num.aal <- ncol(ad.bs) # num of regions of interests
#library(quadprog)
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
ind.bs <- as.matrix(ad.bs) %*% omega
ind.m6 <- as.matrix(ad.m6) %*% omega
ind.m12 <- as.matrix(ad.m12) %*% omega
library(ggplot2)
dat <- as.data.frame(cbind(c(1:num.sub), ind.bs, ind.m6, ind.m12))
# Full plot
ggplot(data = dat) + geom_point(aes(y = V1, x = V2), colour = "#FFCC00") + 
  geom_point(aes(y = V1, x = V3), colour = "#0033CC") + 
  geom_point(aes(y = V1, x = V4), colour = "#CC0033") + 
  scale_x_continuous(name="Health Index Value") +
  scale_y_continuous(name="Subject No.", breaks = c(1:nrow(dat))) +
  ggtitle("Health Index, Yellow = baseline, Blue = m06, Red = m12")

# Less subjects, clearer plot
dat2 <- dat[sample(c(1:num.sub), 5),] # for clear visualization
ggplot(data = dat2) + geom_point(aes(y = V1, x = V2), colour = "#FFCC00", size = 10) + 
  geom_point(aes(y = V1, x = V3), colour = "#0033CC", size = 10) + 
  geom_point(aes(y = V1, x = V4), colour = "#CC0033", size = 10) + 
  scale_x_continuous(name="Health Index Value") +
  scale_y_continuous(name="Subject No.") +
  ggtitle("Health Index, Yellow = baseline, Blue = m06, Red = m12")
