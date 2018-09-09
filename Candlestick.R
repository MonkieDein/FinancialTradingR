################################################################################

#                          Financial Trading With R                            #

# DOWNLOAD FOREX DATA FROM INVESTING.COM

# CLEAN AND ORGANIZE THE DATA FRAME

# CANDLESTICK CHART

# ICHIMOKU CLOUD


################################################################################

rm(list = ls())
setwd("~/Desktop/FOREX")

# DOWNLOAD FOREX DATA FROM fxhistoricaldata.com

# Currency is Capitalized
# get_url take currency pair as input and output the URL for the data, and get_data, take the URL and generate a csv file.

get_url = function(currency){
  return(paste0("http://api.fxhistoricaldata.com/indicators?instruments=",currency,"&expression=open,high,low,close&item_count=10000&format=csv&timeframe=hour"))
} 

get_data = function(currency){
  return(read.csv(url(get_url(currency)), header = FALSE))
}

EURUSD = get_data("EURUSD")
head(EURUSD)

summary(EURUSD)




# Clean and organize the data frame

# Give the data column names
colnames(EURUSD) = c("currency pair","time","open","high","low","close")

# Order the data that the last data is the most frequent data.
EURUSD = EURUSD[order(EURUSD$time),]

EURUSD[,2] = as.POSIXct(strptime(EURUSD[,2], "%Y-%m-%d %H:%M:%S"))




# Candlestick Chart

# Candlestick chart set up
df = EURUSD[c((nrow(EURUSD)-200):nrow(EURUSD)),]

mn = min(df$low)
mx = max(df$high)

xs = c(1:nrow(df))
xs2 = seq(1,nrow(df),5)

color_list = ifelse(df$close >= df$open , "green", "red")


# Plot candlestick

png("~/Desktop/FOREX/GraphEURUSD.png",res=500,height=5000,width = 9000)
par(mar=c(9,3,3,3))

plot(df$high,main = "EURUSD",xaxt="n",xlab="",ylab="price",ylim=c(mn,mx),type="n")
par(new=T)

plot(df$low,main = "",axes=F,xlab="",ylab="",ylim=c(mn,mx),type="n")
segments( x0=xs, y0=df$open,x1=xs,y1=df$close,col = color_list,lwd=4)
segments( x0=xs, y0=df$low,x1=xs,y1=df$high,col = color_list,lwd=1)

axis(1,at=xs2,labels=df$time[xs2],las=2)

dev.off()






# Ichimoku Cloud







