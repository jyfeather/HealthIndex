rm(list=ls())
#######################################################################
#                         Input
#######################################################################
if (FALSE) {
  raw.cor <- read.csv(file = "C:/Users/jyfea_000/Dropbox/Research/Health_Index/dataset/SLS/cors.csv")
  dat.cor <- raw.cor[,c(-2:-5)]
  cols <- colnames(dat.cor)
  cols.1 <- which(lapply(cols, function(x) substr(x, nchar(x), nchar(x))) %in% c("1"))
  cols.2 <- which(lapply(cols, function(x) substr(x, nchar(x), nchar(x))) %in% c("2"))
  cols.3 <- which(lapply(cols, function(x) substr(x, nchar(x), nchar(x))) %in% c("3"))
  cols <- strtrim(cols, nchar(cols)-1)
  lapply(1:55, function(x) identical(cols[cols.1[x]], cols[cols.2[x]]))
  lapply(1:55, function(x) identical(cols[cols.2[x]], cols[cols.3[x]]))
  check.2 <- unlist(lapply(1:nrow(dat.cor), function(x) any(is.na(dat.cor[x,1:111]))))
  table(check.2)
  dat.2 <- dat.cor[which(!check.2),]
  
  # cognitive score
  raw.cog <- read.csv(file = "C:/Users/jyfea_000/Dropbox/Research/Health_Index/dataset/SLS/cog.csv")
  
  rm(dat.cor, check.2, cols.1, cols.2, cols.3)
  # save .RData
  unlink("Paul.RData")
  save.image(file = "Paul.RData")
}

# load .RData
load(file = "./Paul.RData")

#######################################################################
#                         standarization 
#######################################################################
tot.1 <- dat.2[,2:56]
tot.2 <- dat.2[,57:111]
tot.3 <- dat.2[,112:166]
# standardization
library(matrixStats)
mean.1 <- colMeans(tot.1)
sd.1 <- colSds(as.matrix(tot.1))
for (i in 1:55) {
  tot.1[,i] = (tot.1[,i]-mean.1[i])/sd.1[i]
  tot.2[,i] = (tot.2[,i]-mean.1[i])/sd.1[i]
  tot.3[,i] = (tot.3[,i]-mean.1[i])/sd.1[i]
}
num.epo <- 3 # num of epochs is 3
num.cor <- 55

omega.name <- colnames(dat.2)[-1] 
omega.name <- substr(omega.name, 3, nchar(omega.name) - 2)

#######################################################################
#                         cognitive score
#######################################################################
#sub.dec <- subset(raw.cog, REASTAT == 1) # decline
#sub.nondec <- subset(raw.cog, REASTAT == 0) # gain/stable
sub.dec <- subset(raw.cog, SPEEDSTA == 1) # decline
sub.nondec <- subset(raw.cog, SPEEDSTA == 0) # gain/stable
#sub.dec <- subset(raw.cog, MEMSTAT == 1) # decline
#sub.nondec <- subset(raw.cog, MEMSTAT == 0) # gain/stable

dec.name <- dat.2[which(dat.2$SUBJECT %in% sub.dec$IDNUM),1]
nondec.name <- dat.2[which(dat.2$SUBJECT %in% sub.nondec$IDNUM),1]
dec.1 <- tot.1[which(dat.2$SUBJECT %in% sub.dec$IDNUM),]
dec.2 <- tot.2[which(dat.2$SUBJECT %in% sub.dec$IDNUM),]
dec.3 <- tot.3[which(dat.2$SUBJECT %in% sub.dec$IDNUM),]
nondec.1 <- tot.1[which(dat.2$SUBJECT %in% sub.nondec$IDNUM),]
nondec.2 <- tot.2[which(dat.2$SUBJECT %in% sub.nondec$IDNUM),]
nondec.3 <- tot.3[which(dat.2$SUBJECT %in% sub.nondec$IDNUM),]

#######################################################################
#                         training set & testing set
#######################################################################
# for decliner
train.dec.no <- 1:30
test.dec.no <- which(!(1:nrow(dec.1))%in%train.dec.no)
train.dec.1 <- dec.1[train.dec.no,]
train.dec.2 <- dec.2[train.dec.no,]
train.dec.3 <- dec.3[train.dec.no,]
test.dec.1 <- dec.1[test.dec.no,]
test.dec.2 <- dec.2[test.dec.no,]
test.dec.3 <- dec.3[test.dec.no,]

# for non-decliner
train.nondec.no <- sample(1:nrow(nondec.1), round(nrow(nondec.1)*0.7))
test.nondec.no <- which(!(1:nrow(nondec.1))%in%train.nondec.no)
train.nondec.1 <- nondec.1[train.nondec.no,]
train.nondec.2 <- nondec.2[train.nondec.no,]
train.nondec.3 <- nondec.3[train.nondec.no,]
test.nondec.1 <- nondec.1[test.nondec.no,]
test.nondec.2 <- nondec.2[test.nondec.no,]
test.nondec.3 <- nondec.3[test.nondec.no,]

# difference table
dec.e1 <- train.dec.2 - train.dec.1
dec.e2 <- train.dec.3 - train.dec.2
nondec.e1 <- train.nondec.2 - train.nondec.1
nondec.e2 <- train.nondec.3 - train.nondec.2
dec.e1 <- setNames(dec.e1, 1:55)
dec.e2 <- setNames(dec.e2, 1:55)
nondec.e1 <- setNames(nondec.e1, 1:55)
nondec.e2 <- setNames(nondec.e2, 1:55)
E.dec <- rbind(dec.e1, dec.e2)
E.dec <- na.omit(E.dec)
E.nondec <- rbind(nondec.e1, nondec.e2)
E.nondec <- na.omit(E.nondec)

write.table(E.dec, file = "./data/Edec.csv", sep = ",", row.names = FALSE, col.names = FALSE)
write.table(E.nondec, file = "./data/Enondec.csv", sep = ",", row.names = FALSE, col.names = FALSE)

#######################################################################
#                         Use CVX to solve this problem  
#######################################################################
readline("Press any key to return after solving the problem.")

omega <- read.table(file = "./data/w.res", header = FALSE)

#######################################################################
#                         Health Index Construction
#######################################################################
test.1 <- test.dec.1
test.2 <- test.dec.2
test.3 <- test.dec.3
test.sub <- dec.name[test.dec.no]
test.no <- nrow(test.1)

test.1 <- test.nondec.1
test.2 <- test.nondec.2
test.3 <- test.nondec.3
test.sub <- nondec.name[test.nondec.no]
test.no <- nrow(test.1)

ind.1 <- as.matrix(test.1) %*% omega$V1
ind.2 <- as.matrix(test.2) %*% omega$V1
ind.3 <- as.matrix(test.3) %*% omega$V1

viz.idx <- rbind(cbind(test.sub, index = ind.1, time = rep(1, length(test.no))), 
      cbind(test.sub, index = ind.2, time = rep(2, length(test.no))), 
      cbind(test.sub, index = ind.3, time = rep(3, length(test.no))))
rownames(viz.idx) <- NULL
viz.idx <- as.data.frame(viz.idx)
as.factor(viz.idx$time)
viz.idx$no <- rep(1:length(test.no),3)
viz.idx$time <- as.factor(viz.idx$time)

library(ggplot2)
# trend plot, curve
ggplot(data = na.omit(viz.idx[viz.idx$no %in% 1:25, ]), aes(x=time, y=V2, group = test.sub)) + geom_point() + geom_line() +
  scale_y_continuous(name = "Health Index Value") +
  scale_x_discrete(labels = c("BL", "6", "12")) + facet_grid(.~test.sub) +
  ggtitle("Health Index on Testing Set")

# trend plot, dot
ggplot(data = na.omit(viz.idx), aes(x = V2, y = no, group = time, color = time)) +
  geom_point() +
  scale_color_manual(values = c("#FFCC00", "#0033CC", "#CC0033"), labels = c("Time 1", "Time 2", "Time 3")) +
  scale_x_continuous(name="Health Index Value") +
  scale_y_discrete(name="Subject ID", breaks = c(1:length(test.no)), labels = test.sub) +
  ggtitle("Health Index on Testing Set")