################################################################################

#                          Financial Trading With R                            #

# DOWNLOAD FOREX DATA FROM INVESTING.COM

# CLEAN AND ORGANIZE THE DATA FRAME

# CANDLESTICK CHART

# ICHIMOKU CLOUD

# William Fractals

# Strategy creation and Backtesting
################################################################################

rm(list = ls())
# install.packages("TTR")
setwd("C:/Users/USER/Desktop/FOREX")
source("C:/Users/USER/Desktop/FOREX/Ichimoku RpubS.r")
currency=read.csv("USD_CAD Historical Data.csv")    # Need to edit for different currency.
library(TTR)
currencyname="USDCAD"   # Need to edit for different currency.


# Data time is in accending order
currency=currency[(dim(currency)[1]):1,]
# Data entry has to be high, low, close, open, time
data=currency[,c(4,5,2,3,1)]
colnames(data)=c("high","low","close","open","time")


# Set parameter and compute ichimoku line
pars <- c(9,26,52)
out <- ichimoku(data,pars)
len=c(1:length(out[,1]))
out=fractals(currency,data,out)


png(paste0("C:/Users/USER/Desktop/FOREX/Ichimoku",currencyname,".png"),res = 300,height=5000,width = (20*length(len)+4000))
par(mar=c(9,3,3,3))
################################################################################
# Function for Plotting Candlestick and Ichimoku
################################################################################
plotichimoku(data,out,len)
################################################################################
# Function for Plotting William Fractals
################################################################################
points(out[,11], pch = 17)
points(out[,12], pch = 25,col="black",bg="black")
dev.off()



listofout=Strategy1(out,pars)
outdf=listofout[[1]]
tradehist=listofout[[2]]




png(paste0("C:/Users/USER/Desktop/FOREX/PurchaseLINE",currencyname,".png"),res = 300,height=5000,width = (20*length(len)+4000))
par(mar=c(9,3,3,3))

################################################################################
# Function for Plotting Candlestick and Ichimoku
################################################################################
plotichimoku(data,out,len)
################################################################################
# Function for Plotting William Fractals
################################################################################
points(out[,11], pch = 17)
points(out[,12], pch = 25,col="black",bg="black")
################################################################################
# Function for Plotting Trading Profit and Location
################################################################################
tradecol=ifelse(tradehist$Profit>0,"purple", "brown")
for (lin in 1:length(tradehist[,1])){
  lines(tradehist[lin,c(3,6)],outdf$Close[as.matrix(tradehist[lin,c(3,6)])],lwd=5,col=tradecol[lin])
}

dev.off()


