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
library(TTR)


UC=read.csv("AUD-USD Historical Data.csv")
UC=UC[nrow(UC):1,]
sar=SAR(UC[,c(4,5)], accel = c(0.02, 0.2))

SELL= (sar > (UC$High+0.0100))*1
BUY= ((UC$Low-0.0100) > sar)*1

point_col=ifelse(BUY+SELL >0 , "green", "black")

BUY = c(0,BUY[-1601])
SELL = c(0,SELL[-1601])

BUYGAIN = UC$Price-UC$Open
SELLGAIN = UC$Open-UC$Price

point_col[which((BUY*BUYGAIN+SELL*SELLGAIN)<0)]="red"
sum(BUY*BUYGAIN+SELL*SELLGAIN)

# plot(sar,col=point_col)

currencyname=c("USD-CHF","USD-GBP","USD-EUR","USD-JPY","NZD-USD","USD-CAD","AUD-USD","EUR-GBP","EUR-AUD","GBP-JPY","CHF-JPY","NZD-JPY","GBP-CAD")


# EUR/GBP - Euro/British pound
# EUR/AUD - Euro/Australian dollar
# GBP/JPY - British pound/Japanese yen
# CHF/JPY - Swiss franc/Japanese yen
# NZD/JPY - New Zealand dollar/Japanese yen
# GBP/CAD - British pound/Canadian dollar

profit={}
for (i in currencyname){
  tradehist = BacktestingMain(i)
  profit=c(profit,tradehist[nrow(tradehist),ncol(tradehist)])
}
profit[c(4,10:12)]=profit[c(4,10:12)]/100

sum(profit)

sd(profit)
