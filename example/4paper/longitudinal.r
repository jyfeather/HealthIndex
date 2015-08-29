# this is for figure 2
library(reshape2)
library(ggplot2)
rm(list=ls())

# define multiplot function
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

## plot
load(file = "./mPOWEr.RData")

isSSI <- data.orig[,"indicbinary"]
isSSI <- as.factor(isSSI)
data_wide <- as.data.frame(cbind(data.ewma, isSSI))

#data_long <- dcast(data_wide, pidnr + isSSI ~ postopday, value.var = "degreeexudate")
p1 <- ggplot(data = data_wide, aes(x = postopday, y = degreeslough, group = pidnr)) +
  theme_bw() + theme(legend.position = c(0.15, 0.8)) + 
  stat_smooth(aes(group = isSSI, color = as.factor(isSSI)), size = 1) +
  theme(legend.title = element_blank(), text = element_text(size = 18)) +
  scale_color_discrete(labels=c("non-SSI", "SSI")) + xlab("post operation day")

p2 <- ggplot(data = data_wide, aes(x = postopday, y = granulation, group = pidnr)) +
  theme_bw() + theme(legend.position = c(0.15, 0.8)) + 
  stat_smooth(aes(group = isSSI, color = as.factor(isSSI)), size = 1) +
  theme(legend.title = element_blank(), text = element_text(size = 18)) +
  scale_color_discrete(labels=c("non-SSI", "SSI")) + xlab("post operation day")

multiplot(p1, p2, cols = 2)
