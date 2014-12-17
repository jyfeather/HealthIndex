#rm(list = ls())

idx <- read.csv(file = "./data/tmp/idx.csv")
tot.idx <- idx[,c(-1,-2,-6)]
ad.idx <- idx[idx$group == "AD",c(-1,-2,-6)]
mci.idx <- idx[idx$group == "MCI",c(-1,-2,-6)]
nc.idx <- idx[idx$group == "NC",c(-1,-2,-6)]

#######################################################################
#                         Distribution & T test  
#######################################################################
library(ggplot2)
ggplot(data = ad.idx) +
  geom_density(aes(V2), colour = "#FFCC00") +
  geom_density(aes(V3), colour = "#0033CC") +
  geom_density(aes(V4), colour = "#CC0033") +
  ggtitle("AD")
t.test(ad.idx$V2, ad.idx$V3)
t.test(ad.idx$V2, ad.idx$V4)
t.test(ad.idx$V3, ad.idx$V4)

ggplot(data = mci.idx) +
  geom_density(aes(V2), colour = "#FFCC00") +
  geom_density(aes(V3), colour = "#0033CC") +
  geom_density(aes(V4), colour = "#CC0033") +
  ggtitle("MCI")
t.test(mci.idx$V2, mci.idx$V3)
t.test(mci.idx$V2, mci.idx$V4)
t.test(mci.idx$V3, mci.idx$V4)

ggplot(data = nc.idx) +
  geom_density(aes(V2), colour = "#FFCC00") +
  geom_density(aes(V3), colour = "#0033CC") +
  geom_density(aes(V4), colour = "#CC0033") +
  ggtitle("NC")
t.test(nc.idx$V2, nc.idx$V3)
t.test(nc.idx$V2, nc.idx$V4)
t.test(nc.idx$V3, nc.idx$V4)

#######################################################################
#                         Monotone Check  
#######################################################################
diff2 <- function(df) {
  return(cbind(df[,2]-df[,1], df[,3]-df[,2], df[,3]-df[,1]))
}
ad.dif <- diff2(ad.idx)
mci.dif <- diff2(mci.idx)
nc.dif <- diff2(nc.idx)
tot.dif <- diff2(tot.idx)
# Level 1: bl - m06 - m12  + + +
# Level 2: bl - m12 - m06  + - +
#          m06 - bl - m12  - + +
#          m12 - bl - m06  + - -
# Level 3: m06 - m12 - bl  - + -
#          m12 - m06 - bl  - - -
check <- function(dif) {
  extent <- apply(dif, 1, function(x) {
    sig <- 0
    if (min(x) >= 0) {
      sig <- 1
    } else if (x[1] < 0 && x[3] < 0) {
      sig <- 3 
    } else {
      sig <- 2 
    }
    return(sig)
  })
  return(table(extent))
}
ad.check <- as.data.frame(check(ad.dif))
mci.check <- as.data.frame(check(mci.dif))
nc.check <- as.data.frame(check(nc.dif))
tot.check <- as.data.frame(check(tot.dif))
