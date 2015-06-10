rm(list=ls())
#######################################################################
#                         Input
#######################################################################
if (FALSE) {
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
  rid <- c(ad.m12[,1], mci.m12[,1], nl.m12[,1])
  # First 90 regions of interest
  dat.bs <- dat.bs[,1:90]
  dat.m6 <- dat.m6[,1:90]
  dat.m12 <- dat.m12[,1:90]  
  omega.name <- read.table(file = "C:/Users/jyfea_000/Dropbox/Research/Health_Index/dataset/ADNI/aal.txt", sep = " ")
  # save data
  unlink("*.RData")
  save.image(file = "ADNI.RData")
}


# load data
load(file = "./ADNI.RData")

#######################################################################
#                         Quadratic Formulation  
#######################################################################
num.break <- c(nrow(ad.bs), nrow(ad.bs) + nrow(mci.bs), nrow(ad.bs) + nrow(mci.bs) + nrow(nl.bs))
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

#library(quadprog)
e1 <- train.m6 - train.bs
e2 <- train.m12 - train.m6
E <- rbind(e1, e2)
#solve.QP(Dmat = 2*H, dvec = -l, Amat = t(E), bvec = b0)

#######################################################################
#                         Use AMPL to solve this problem  
#######################################################################
# pass coefficients to AMPL to solve this problem
E <- round(E, 5)
E <- cbind(c(1:nrow(E)), E)
E <- rbind(as.integer(c(0:ncol(E))), E)
write.table(E, file = "./data/tmp/E.dat", sep = " ", row.names = FALSE, col.names = FALSE)

omega <- read.table(file = "./data/tmp/w.res", header = FALSE)
unlink("./data/tmp/w.res")

#######################################################################
#                         Health Index Construction
#######################################################################
ind.bs <- as.matrix(test.bs) %*% omega$V1
ind.m6 <- as.matrix(test.m6) %*% omega$V1
ind.m12 <- as.matrix(test.m12) %*% omega$V1

err.vec <- 1 - c(ind.m6[,1] - ind.bs[,1], ind.m12[,1] - ind.m6[,1])
err <- sum(err.vec[err.vec > 0])

test.sub <- nrow(test.bs)
dat <- as.data.frame(cbind(c(1:test.sub), ind.bs, ind.m6, ind.m12))
# add group info, AD, MCI, NI
dat$group = NA;
dat[which(test.no <= num.break[3]),]$group = "NC"
dat[which(test.no <= num.break[2]),]$group = "MCI"
dat[which(test.no <= num.break[1]),]$group <- "AD"
as.factor(dat$group)

write.csv(cbind(rid = rid[test.no], dat[,-1]), file = "./data/tmp/idx.csv")

library(ggplot2)
# Full plot
ggplot(data = dat, aes(y = V1, x = val, shape = group, color = val2)) + 
  geom_point(aes(x = V2, shape = group, color = "00")) + 
  geom_point(aes(x = V3, shape = group, color = "06")) + 
  geom_point(aes(x = V4, shape = group, color = "12")) + 
  scale_color_manual(values = c("#FFCC00", "#0033CC", "#CC0033"), breaks = c("00", "06", "12"), 
                     labels = c("Baseline", "M06", "M12"), name = "time") +
  scale_x_continuous(name="Health Index Value") +
  scale_y_continuous(name="Subject No.", breaks = c(1:nrow(dat))) +
  ggtitle("Health Index on Testing Set")

# Less subjects, clearer plot
dat2 <- dat[sample(c(1:test.sub), 5),] # for clear visualization
ggplot(data = dat2) + geom_point(aes(y = V1, x = V2), colour = "#FFCC00", size = 10) + 
  geom_point(aes(y = V1, x = V3), colour = "#0033CC", size = 10) + 
  geom_point(aes(y = V1, x = V4), colour = "#CC0033", size = 10) + 
  scale_x_continuous(name="Health Index Value") +
  scale_y_continuous(name="Subject No.") +
  ggtitle("Health Index, Yellow = baseline, Blue = m06, Red = m12")

#######################################################################
#                         Other Visualization   
#######################################################################
omega.name <- omega.name$V2
viz.omega <- cbind(omega, name = omega.name[1:90])
viz.omega$no <- c(1:length(omega$V1))
ggplot(data = viz.omega, aes(y = V1, x = no)) + geom_bar(stat = "identity") +
  scale_x_discrete(breaks = viz.omega$no, labels = omega.name[1:90]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank()) +
  ylab("Relative Importance")
