rm(list = ls())
#######################################################################
#                         Input
#######################################################################
idx <- read.csv(file = "./data/tmp/idx.csv")
adas <- read.csv(file = "C:/Users/jyfea_000/Dropbox/Research/Health_Index/dataset/ADNI/SCORE/ADASSCORES.csv")
mmse <- read.csv(file = "C:/Users/jyfea_000/Dropbox/Research/Health_Index/dataset/ADNI/SCORE/MMSE.csv")
adas <- adas[c("RID", "VISCODE", "TOTALMOD")]
mmse <- mmse[c("RID", "VISCODE", "MMSCORE")]
levels(mmse$VISCODE)[levels(mmse$VISCODE) %in% c("f", "sc")] <- "bl"

# transfromation
idx <- idx[order(idx$rid),c(-1, -6)]
sub.adas <- adas[which(adas$RID %in% idx$rid),]
sub.mmse <- mmse[which(mmse$RID %in% idx$rid),]
sub.adas <- sub.adas[order(sub.adas$RID, sub.adas$VISCODE),]
sub.mmse <- sub.mmse[order(sub.mmse$RID, sub.mmse$VISCODE),] 

transform <- function(df) {
  rids <- unique(df$RID)
  re <- matrix(0, nrow = length(rids), ncol = 4)
  re[,1] <- rids
  for (i in rids) {
    sc <- df[df$RID == i, 3]
    if (length(sc) < 3) {
      sc <- c(sc, rep(0, 3 - length(sc)))
    } else {
      sc <- sc[1:3]
    }
    re[re[,1]==i,] <- c(i,sc)
  }
  return(re)
}

sub.adas <- transform(sub.adas)
sub.mmse <- transform(sub.mmse)

#######################################################################
#                         Correlation  
#######################################################################
vec.mmse <- as.vector(t(sub.mmse[,-1]))
vec.adas <- as.vector(t(sub.adas[,-1]))
vec.idx <- as.vector(t(idx[,-1]))
cor.1 <- cor.test(vec.mmse, vec.adas); plot(vec.mmse, vec.adas)
cor.2 <- cor.test(vec.mmse, vec.idx); plot(vec.mmse, vec.idx)
cor.3 <- cor.test(vec.adas, vec.idx); plot(vec.adas, vec.idx)
