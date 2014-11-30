library("xlsx")
dat.health <- read.xlsx(file = "./data/DRIVE16.xlsx", header = TRUE, sheetName = "HEALTH")
dat.sem <- read.xlsx(file = "./data/DRIVE16.xlsx", header = TRUE, sheetName = "SEM")
dat.sem2 <- read.xlsx(file = "./data/DRIVE16.xlsx", header = TRUE, sheetName = "SEM2")
dat.sd <- read.xlsx(file = "./data/DRIVE16.xlsx", header = TRUE, sheetName = "SD")

library(plyr)
dat.all <- rbind.fill(dat.health, dat.sem, dat.sem2, dat.sd)
dat.all <- dat.all[order(dat.all$IDNUM),]
dat.all[is.na(dat.all)] <- 0 # na replaced by 0

stat <- data.frame(matrix(data = NA, ncol = 4))
for (i in unique(dat.all[,1])) {
  num1 <- sum(dat.all[dat.all$IDNUM == i, ] == 1)
  num2 <- sum(dat.all[dat.all$IDNUM == i, ] == 2)
  num3 <- sum(dat.all[dat.all$IDNUM == i, ] == 3)
  num <- num1 + num2 + num3
  stat <- rbind(stat, c(i, num1/num, num2/num, num3/num))
}
stat <- stat[-1,]
write.csv(stat, file = "./inst/examples/tmp.csv")

library(ggplot2)
viz.age <- cbind(stat, age = dat.health[,4])
viz.age <- viz.age[order(viz.age$age),]
viz.age <- cbind(no = c(1:nrow(stat)), stat)
ggplot(data = viz.age) + 
  geom_line(aes(x = no, y = X2, color = "ratio1")) +
  geom_line(aes(x = no, y = X3, color = "ratio2")) +
  geom_line(aes(x = no, y = X4, color = "ratio3")) +
  scale_colour_manual("", breaks = c("ratio1", "ratio2", "ratio3"),values = c("red", "blue", "purple")) +
  scale_x_discrete("Decreasing Age", labels = viz.age$X1) + 
  ggtitle("statistics VS. age")
  
viz.edu <- cbind(stat, edu = dat.health[,3])
viz.edu <- viz.edu[order(viz.edu$edu), ]
ggplot(data = viz.edu) + 
  geom_point(aes(x = edu, y = X2, color = "ratio1"), size = 5) +
  geom_point(aes(x = edu, y = X3, color = "ratio2"), size = 5) +
  geom_point(aes(x = edu, y = X4, color = "ratio3"), size = 5) +
  scale_colour_manual("", breaks = c("ratio1", "ratio2", "ratio3"),values = c("red", "blue", "purple")) +
  scale_x_continuous(breaks = c(11:22)) +
  ggtitle("statistics VS. edu")

viz.sex <- cbind(stat, sex = dat.health[,2])
viz.sex <- viz.sex[order(viz.sex$edu), ]
ggplot(data = viz.sex) + 
  geom_point(aes(x = sex, y = X2, color = "ratio1"), size = 5) +
  geom_point(aes(x = sex, y = X3, color = "ratio2"), size = 5) +
  geom_point(aes(x = sex, y = X4, color = "ratio3"), size = 5) +
  scale_colour_manual("", breaks = c("ratio1", "ratio2", "ratio3"),values = c("red", "blue", "purple")) +
  scale_x_discrete(labels = c(1,2)) +
  ggtitle("statistics VS. sex")

dat.3 <- rbind.fill(dat.sem, dat.sem2, dat.sd)
dat.3 <- dat.3[order(dat.3$IDNUM),]
dat.3[is.na(dat.3)] <- 0 # na replaced by 0
dat.cluster <- data.frame(matrix(data = NA, ncol = ncol(dat.3)))
for (i in unique(dat.3[,1])) {
  tmp <- colSums(dat.3[dat.3$IDNUM == i, ])
  for (j in 1:ncol(dat.3)) {
    if (tmp[j] >= 3) tmp[j] = tmp[j] / 3
  }
  dat.cluster <- rbind(dat.cluster, tmp)
}
dat.cluster <- dat.cluster[-1,]
kmeans(dat.cluster[,-c(1,2,3,4)], centers = 2)

library(lars)
dat.lar <- as.matrix(dat.cluster)
colnames(dat.lar) <- c(colnames(dat.3))
dat.lar <- cbind(dat.lar, y=c(1,2,2,2,2,1,2,2,1,2,2,2,2,2,2,2))
numy <- ncol(dat.lar)
res.lasso <- lars(dat.lar[,-c(1,2,3,4,numy)], dat.lar[,numy])
plot(res.lasso)
res.test <- c("delay.call", "DLR", "NEOO", "memory", "verbal", "NEOO", "reason", "NEON", "NEOA", "PPS")
write.csv(res.test, "./inst/examples//tmp.csv")
