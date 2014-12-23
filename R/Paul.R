rm(list=ls())
#######################################################################
#                         Input
#######################################################################
if (FALSE) {
  raw.cor <- read.csv(file = "C:/Users/jyfea_000/Dropbox/Research/Health_Index/dataset/Paul Corr/cors.csv")
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
  # save .RData
  unlink("Paul.RData")
  save.image(file = "Paul.RData")
}

# load .RData
load(file = "./Paul.RData")

#######################################################################
#                         Quadratic Formulation  
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
train.no <- sample(num.sub, round(2/3*num.sub))
train.1 <- tot.1[train.no,]
train.2 <- tot.2[train.no,]
train.3 <- tot.3[train.no,]
dat.no <- c(1:num.sub)
test.no <- dat.no[!dat.no %in% train.no]
test.sub <- dat.2[test.no, 1]
test.1 <- tot.1[test.no,]
test.2 <- tot.2[test.no,]
test.3 <- tot.3[test.no,]

omega.name <- colnames(dat.2)[-1] 
omega.name <- substr(omega.name, 3, nchar(omega.name) - 2)

e1 <- train.2 - train.1
e2 <- train.3 - train.2
e1 <- setNames(e1, 1:55)
e2 <- setNames(e2, 1:55)
E <- rbind(e1, e2)
E <- na.omit(E)

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
