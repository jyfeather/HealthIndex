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
  check.3 <- unlist(lapply(1:nrow(dat.cor), function(x) any(is.na(dat.cor[x,]))))
  table(check.3)
  dat.3 <- dat.cor[which(!check.3),]
  # save .RData
  unlink("Paul.RData")
  save.image(file = "Paul.RData")
}

# load .RData
load(file = "./Paul.RData")

#######################################################################
#                         Quadratic Formulation  
#######################################################################
num.sub <- nrow(dat.3)
num.epo <- 3 # num of epochs is 3
num.cor <- 55
train.no <- sample(num.sub, round(2/3*num.sub))
train.1 <- dat.3[train.no,2:56]
train.2 <- dat.3[train.no,57:111]
train.3 <- dat.3[train.no,112:166]
dat.no <- c(1:num.sub)
test.no <- dat.no[!dat.no %in% train.no]
test.sub <- dat.3[test.no, 1]
test.1 <- dat.3[test.no,2:56]
test.2 <- dat.3[test.no,57:111]
test.3 <- dat.3[test.no,112:166]

#library(quadprog)
e1 <- train.2 - train.1
e2 <- train.3 - train.2
e1 <- setNames(e1, 1:55)
e2 <- setNames(e2, 1:55)
E <- rbind(e1, e2)

#######################################################################
#                         Use AMPL to solve this problem  
#######################################################################

# pass coefficients to AMPL to solve this problem
E <- round(E, 5)
E <- cbind(c(1:nrow(E)), E)
E <- rbind(as.integer(c(0:ncol(E))), E)
write.table(E, file = "./data/tmp/E.dat", sep = " ", row.names = FALSE, col.names = FALSE)

omega <- read.table(file = "./data/tmp/w.res", header = FALSE)

#######################################################################
#                         Health Index Construction
#######################################################################
ind.1 <- as.matrix(test.1) %*% omega$V1
ind.2 <- as.matrix(test.2) %*% omega$V1
ind.3 <- as.matrix(test.3) %*% omega$V1

dat.viz <- rbind(cbind(test.sub, index = ind.1, time = rep(1, 41)), 
      cbind(test.sub, index = ind.2, time = rep(2, 41)), 
      cbind(test.sub, index = ind.3, time = rep(3, 41)))
rownames(dat.viz) <- NULL
dat.viz <- as.data.frame(dat.viz)
as.factor(dat.viz$time)

library(ggplot2)
ggplot(data = dat.viz, aes(x=time, y=V2, group = test.sub)) + geom_point() + geom_line() +
  scale_y_continuous(name = "Health Index Value") +
  scale_x_discrete(labels = c("BL", "6", "12")) + facet_grid(.~test.sub)

ggplot(data = dat.viz, aes(x = value, y = no, colour = valuable)) +
  geom_point(aes(x = V3, col = "1")) +
  geom_point(aes(x = V4, col = "2")) +
  geom_point(aes(x = V5, col = "3")) +
  scale_color_manual(values = c("#FFCC00", "#0033CC", "#CC0033"), breaks = c("1", "2", "3"), 
                     labels = c("Time 1", "Time 2", "Time 3")) +
  scale_x_continuous(name="Health Index Value") +
  scale_y_discrete(name="Subject ID", breaks = c(1:length(test.no)), labels = dat.viz$id) +
  ggtitle("Health Index on Testing Set")

omega.name <- colnames(train.1) 
omega.name <- substr(omega.name, 3, nchar(omega.name) - 2)
mean <- colMeans(dat.3[,-1])
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
  scale_y_continuous(name = "Health Index Value") +
  scale_x_discrete(labels = c("BL", "6", "12")) + facet_grid(.~name)


#######################################################################
#                         Other Visualization   
#######################################################################
viz.omega <- cbind(omega, name = omega.name)
viz.omega$no <- c(1:length(omega$V1))
ggplot(data = viz.omega, aes(y = V1, x = no)) + geom_bar(stat = "identity") +
  scale_x_discrete(breaks = viz.omega$no, labels = omega.name) +
  scale_y_continuous(name = "weight") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
omega.name[order(omega, decreasing = TRUE)]
