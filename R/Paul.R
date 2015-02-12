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
#                         cognitive score
#######################################################################
sub.dec <- subset(raw.cog, REASTAT == 1) # decline
sub.nondec <- subset(raw.cog, REASTAT == 0) # gain/stable
#sub.dec <- subset(raw.cog, SPEEDSTA == 1) # decline
#sub.nondec <- subset(raw.cog, SPEEDSTA == 0) # gain/stable
#sub.dec <- subset(raw.cog, MEMSTAT == 1) # decline
#sub.nondec <- subset(raw.cog, MEMSTAT == 0) # gain/stable

dat.orig <- dat.2
dat.nondec <- dat.2[which(dat.2$SUBJECT %in% sub.nondec$IDNUM),]
dat.2 <- dat.2[which(dat.2$SUBJECT %in% sub.dec$IDNUM),]

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
num.sub <- nrow(dat.2)
num.epo <- 3 # num of epochs is 3
num.cor <- 55

omega.name <- colnames(dat.2)[-1] 
omega.name <- substr(omega.name, 3, nchar(omega.name) - 2)
#######################################################################
#                         3-fold cross validation 
#######################################################################
dat.no <- c(1:num.sub)
fold.size <- round(1/3*num.sub)
fold.1 <- sample(num.sub, fold.size, replace = FALSE)
fold.2 <- sample(dat.no[!dat.no %in% fold.1], fold.size, replace = FALSE)
fold.3 <- dat.no[!dat.no %in% c(fold.1, fold.2)]

train.no <- c(fold.1, fold.3)
train.1 <- tot.1[train.no,]
train.2 <- tot.2[train.no,]
train.3 <- tot.3[train.no,]
test.no <- fold.2

test.sub <- dat.nondec[test.no, 1]
test.1 <- dat.nondec[test.no,2:56]
test.2 <- dat.nondec[test.no,57:111]
test.3 <- dat.nondec[test.no,112:166]
#test.sub <- dat.2[test.no, 1]
#test.1 <- tot.1[test.no,]
#test.2 <- tot.2[test.no,]
#test.3 <- tot.3[test.no,]

e1 <- train.2 - train.1
e2 <- train.3 - train.2
e1 <- setNames(e1, 1:55)
e2 <- setNames(e2, 1:55)
E <- rbind(e1, e2)
E <- na.omit(E)

write.table(E, file = "./data/E.csv", sep = ",", row.names = FALSE, col.names = FALSE)
#######################################################################
#                   Bootstraping Confidence Interval
#######################################################################
if(FALSE) {
  boot.num <- 100
  for (i in 1:boot.num) {
    boot.dat <- sample(num.sub, num.sub-1, replace = TRUE)
    e1 <- tot.2[boot.dat,] - tot.1[boot.dat,]
    e2 <- tot.3[boot.dat,] - tot.2[boot.dat,]
    e1 <- setNames(e1, 1:55)
    e2 <- setNames(e2, 1:55)
    E <- rbind(e1, e2)
    E <- na.omit(E)
    write.table(E, file = paste("./data/bootstrapping/E/E",i,".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
  }
  readline("Press any key to return after solving the problem.")
  W <- read.table(file = "./data/bootstrapping/W/W1.csv", header = FALSE)  
  for (i in 2:boot.num) {
    W <- cbind(W, read.table(file = paste("./data/bootstrapping/W/W",i,".csv", sep = ""), header = FALSE))
  }
  boot.W <- data.frame(mean = rowMeans(W), sd = rowSds(as.matrix(W)))
  library(ggplot2)
  ggplot(data = boot.W, aes(x=factor(1:55),y=mean, ymin=mean-2*sd, ymax=mean+2*sd)) +
    geom_pointrange() + ylab("weights") + xlab("variables") +
    ggtitle("Bootstrapping Confidence Interval for Weights")
}

#######################################################################
#                         Use CVX to solve this problem  
#######################################################################
readline("Press any key to return after solving the problem.")

omega <- read.table(file = "./data/w.res", header = FALSE)

#######################################################################
#                         Health Index Construction
#######################################################################
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

# variable trend plot, curve
no.var = 2 
viz.var <- as.data.frame(c(dat.2[,no.var+1], dat.2[,no.var+56], dat.2[,no.var+111]))
viz.var$time <- c(rep(1,nrow(viz.var)/3), rep(2, nrow(viz.var)/3), rep(3, nrow(viz.var)/3))
viz.var$group <- rep(1:as.integer(nrow(viz.var)/3), 3)
colnames(viz.var) <- c("variable", "time", "group")
viz.var2 <- c(mean(viz.var[viz.var$time == 1,1]), 1)
viz.var2 <- rbind(viz.var2, c(mean(viz.var[viz.var$time == 2,1]), 2))
viz.var2 <- rbind(viz.var2, c(mean(viz.var[viz.var$time == 3,1]), 3))
viz.var2 <- as.data.frame(viz.var2)
rownames(viz.var2) <- NULL
viz.var3 <- as.data.frame(cbind(dat.2[,no.var + 1], dat.2[,no.var+56], dat.2[,no.var+111]))
source("./R/TrendStat.R")
diff.var3 <- diff2(viz.var3)
check.var3 <- as.data.frame(check(diff.var3))
ggplot() + 
  geom_point(data = viz.var, aes(x=time, y=variable, group = group)) +
  geom_line(data = viz.var, aes(x=time, y=variable, group = group)) + 
  geom_point(data = viz.var2, aes(x=V2,y=V1), colour = "red") +
  geom_line(data = viz.var2, aes(x=V2,y=V1), colour = "red") +
  ggtitle(omega.name[no.var]) +
  annotate("text", x = 2.5, y = 0.75, label = paste("Strong Mono = ", check.var3[1,2], 
                                                   "\nMono = ", check.var3[2,2],
                                                   "\nAgainst Mono = ", check.var3[3,2]))

# mean of variable trend plot, curve
mean <- colMeans(dat.2[train.no,-1])
mean.1 <- mean[1:30]
mean.2 <- mean[56:85]
mean.3 <- mean[111:140]
dat.mean <- rbind(cbind(mean.1, time = rep(1,30)),
                  cbind(mean.2, time = rep(2,30)),
                  cbind(mean.3, time = rep(3,30)))
dat.mean <- as.data.frame(dat.mean)
dat.mean <- cbind(name = rep(omega.name[1:30], 3), dat.mean)
rownames(dat.mean) <- NULL
as.factor(dat.mean$time)
ggplot(data = dat.mean, aes(x=time, y=mean.1, group = name)) + geom_point() + geom_line() +
  scale_y_continuous(name = "Correlation Value") +
  scale_x_discrete(labels = c("BL", "6", "12")) + facet_grid(.~name)

# variable importance plot.
viz.omega <- cbind(omega, name = omega.name)
viz.omega$no <- c(1:length(omega$V1))
ggplot(data = viz.omega, aes(y = V1, x = no)) + geom_bar(stat = "identity") +
  scale_x_discrete(breaks = viz.omega$no, labels = omega.name) +
  scale_y_continuous(name = "weight") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
omega.name[order(omega, decreasing = TRUE)]
