# figure 5, actual post-op day versus predicted post-op day

library(ggplot2)
library(plyr)
rm(list=ls())
load(file = "./mPOWEr.RData")
ssi <- unique(data.orig[data.orig$indicbinary==1, c("pidnr", "indicbinary", "dayofpred")])
nonssi <- unique(data.orig[data.orig$indicbinary==0, c("pidnr", "indicbinary")])
nonssi <- cbind(nonssi, rep(0, length(nonssi)))

ssi <- as.data.frame(ssi)
names(ssi) <- c("pidnr", "indicbinary", "actual")
nonssi <- as.data.frame(nonssi)
names(nonssi) <- c("pidnr", "indicbinary", "actual")

compare_data <- read.csv(file = "./example/4paper/compare.csv", header = T)
compare_data[compare_data$indicbinary==1, "number"] <- compare_data[compare_data$indicbinary==1, "number"] * 8
compare_data[compare_data$indicbinary==0, "indicbinary"] <- "non-SSI group"
compare_data[compare_data$indicbinary==1, "indicbinary"] <- "SSI group"

ggplot(data = compare_data, aes(x = actual, y = predict, size = number)) +
  geom_point() + facet_wrap(~indicbinary) + theme_bw() + 
  theme(text = element_text(size = 18), legend.position = "none") +
  xlab("actual post operation day") + ylab("predicted post operation day")
