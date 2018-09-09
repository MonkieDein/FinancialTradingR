# The function for computing the Ichimoku cloud
ichimoku = function(data,pars){
  
  # REMEMBER THAT THE DATA SHOULD BE IN ORDER
  #
  # HIGH, LOW and CLOSE
  #
  # ==========================================
  
  # Number of observations
  Nobs <- NROW(data)
  
  # Get the three parameters
  p1 <- pars[1]
  p2 <- pars[2]
  p3 <- pars[3]
  
  # The maximum of these should be p3, check
  if ((p1 > p2) | (p1 > p3) | (p2 > p3)) 
  { 
    stop("parameters should enter in ascending order") 
  }
  # Set the max
  maxp = p3
  
  # You will leave out maxp observations
  cloud.lines <- matrix(NA,nrow=Nobs+maxp,ncol=10)
  colnames(cloud.lines) <- c("Tenkan","Kijun","SenkouA","SenkouB","Chikou","High","Low","Close","Open","Time")
  
  # Filled in Chikou span
  chikou=data[,3]
  cloud.lines[1:Nobs,5]=chikou
  
  cloud.lines[(1+p2):(Nobs+p2),6:10]=as.matrix(data)
  
  # Run a loop to make the computations
  for (i in seq(p1,Nobs+p2,1))
  {
    # Compute the tenkan lines
    tenkan <- (max(data[(i-p1+1):i,1])+min(data[(i-p1+1),2]))/2
    
    # Save in appropriate places
    cloud.lines[i+p2,1] <- tenkan
  }  
  
  for (i in seq(p2,Nobs,1))
  {
    # Compute the kijun and senkouA lines
    kijun <- (max(data[(i-p2+1):i,1])+min(data[(i-p2+1):i,2]))/2

    # Save in appropriate places
    cloud.lines[i+p2,2] <- c(kijun) 
  }
  
  cloud.lines[(1+p2):(Nobs+2*p2),3]=(as.double(cloud.lines[1:(Nobs+p2),1])+as.double(cloud.lines[1:(Nobs+p2),2]))/2
  
  for (i in seq(p3,Nobs,1))
  {
    # Compute the senkouB lines
    senkouB<- (max(data[ (i-p3+1):i ,1])+min(data[(i-p3+1):i ,2]))/2
     
    # Save in appropriate places
    cloud.lines[i+2*p2,4] <- c(senkouB) 
  }
  
  # OK, return everything
  return(cloud.lines)
}



# Set the ichimoku parameters
# pars <- c(9,26,52)
# Data input (high, low, close, open, time)
# out <- ichimoku(data,pars)


plotichimoku = function(data,out,len){
  
  mn=min(data$low)
  mx=max(data$high)
  
  
  plot(len,main = "EURUSD",xaxt="n",xlab="",ylab="price",ylim=c(mn,mx),type="n")
  par(new=T)
  
  lines(out[,1],type="l",col="blue")
  lines(out[,2],col="magenta")
  lines(out[,3],col="orange",type="l")
  lines(out[,4],col="yellow")
  lines(out[,5],col="chartreuse4")
  
  color_list_cloud = ifelse(out[,3] >= out[,4] , "mediumspringgreen", "cadetblue1")
  
  
  segments( x0=len, y0=as.double(out[,3]),x1=len,y1=as.double(out[,4]),col = color_list_cloud,lwd=1)
  
  outdf=as.data.frame(out)
  color_list =ifelse(as.double(as.matrix(outdf$Open)) >= as.double(as.matrix(outdf$Close)) , "red", "green")
  segments( x0=len, y0=as.double(as.matrix(outdf$Open)),x1=len,y1=as.double(as.matrix(outdf$Close)),col = color_list,lwd=4)
  segments( x0=len, y0=as.double(as.matrix(outdf$Low)),x1=len,y1=as.double(as.matrix(outdf$High)),col = color_list,lwd=1)
  axis(1,at=seq(1,nrow(outdf),5),labels=outdf$Time[seq(1,nrow(outdf),5)],las=2)
  # axis(1,at=seq(1,nrow(outdf),5),labels=seq(1,nrow(outdf),5),las=2)
}

fractals = function(currency,data,out){
  n = length(data[,1])
  Fractals = matrix(0,nrow=n,ncol=2)
  
  one=currency$High[-((n-3):n)]
  two=currency$High[-c(1,(n-2):n)] 
  three=currency$High[-c(1,2,n-1,n)]
  four=currency$High[-c(1:3,n)]
  five=currency$High[-c(1:4)]
  
  Fractals[3:(n-2),1]=as.numeric(pmax(one,two,three,four,five)==three)
  
  lone=currency$Low[-((n-3):n)]
  ltwo=currency$Low[-c(1,(n-2):n)] 
  lthree=currency$Low[-c(1,2,n-1,n)]
  lfour=currency$Low[-c(1:3,n)]
  lfive=currency$Low[-c(1:4)]
  
  Fractals[3:(n-2),2]=as.numeric(pmin(lone,ltwo,lthree,lfour,lfive)==lthree)
  
  Fractals=Fractals*currency[,c(4,5)] 
  Fractals[Fractals==0]=NA
  Fractals[,1]=Fractals[,1] + 0.0025
  Fractals[,2]=Fractals[,2] - 0.0025
  
  n = length(out[,1])
  Fractalsbind = matrix(NA,nrow=n,ncol=2)
  Fractalsbind[(is.na(out[,6])==FALSE),]=as.matrix(Fractals)
  colnames(Fractalsbind)=c("UpperFrac","LowerFrac")
  out=cbind(out,Fractalsbind) 
  return(out)
}


Strategy1 = function(out,pars){
  
  ################################################################################
  # Creating Strategy
  
  # Indicate Trend 
  # 1. TK Cross Over (T>K  = BULL ; T<K = BEAR)
  # 2. Fractals position Comparison (Higher HIGH & LOW = BULL ; Lower HIGH & LOW = BEAR)
  # 3. Span A & Span B (Span A > Span B = BULL ; Span A < Span B = BEAR)
  
  # Trade Set up
  # 1. Fractals position Comparison ( Higher Low ; Lower High )
  # 2. Fractals position Comparison ( Higher High ; Lower Low )
  
  # 3. Touch Tenkan (High > T > Low)
  
  # 4. TK Cross Over (T>K  = BULL ; T<K = BEAR)
  # 5. Span A & Span B (Span A > Span B = BULL ; Span A < Span B = BEAR)
  
  
  # MISTAKE :  THE UPPER&LOWER.FRACTALS DOES NOT ALWAYS UPDATE
  
  ################################################################################
  
  
  outdf=as.data.frame(out)
  for (c in c(1:9,11:12)){
    outdf[,c]=as.numeric(levels(outdf[,c]))[outdf[,c]]
  }
  
  outdf$RSI=NA
  rsi=RSI(outdf$Close[which(is.na(outdf$Close)==0)], n = 14)
  outdf$RSI[(1+pars[2]):(length(rsi)+pars[2])]=rsi
  
  outdf$SAR=NA
  outdf$SAR[(1+pars[2]):(length(rsi)+pars[2])]=SAR(outdf[(1+pars[2]):(length(rsi)+pars[2]),c(6,7)], accel = c(0.02, 0.2))
  
  
  
  n = length(out[,1])-pars[2]
  upper.fractal=NA
  lower.fractal=NA
  i = pars[2]
  level = 0
  
  BUY = 0
  SELL = 0
  profit=0
  
  
  tradehin = as.data.frame(matrix(0,ncol=4,nrow = 1),row.names = c("Option","Price","IOpen","SL"))
  tradehout = as.data.frame(matrix(0,ncol=4,nrow = 1),row.names = c("CloseTrade","IClose","Profit","TotalProfit"))                  
  while (i<n){
    i = i + 1
    
    if ((is.na(outdf$UpperFrac[i])==0)&(is.na(upper.fractal))){
      upper.fractal=outdf$UpperFrac[i]
    }
    
    if ((is.na(outdf$LowerFrac[i])==0)&(is.na(lower.fractal))){
      lower.fractal=outdf$LowerFrac[i]
    }
    
    if (is.na(outdf$LowerFrac[i])==0){
      ################################################################################
      ################################################################################
      if ((outdf$LowerFrac[i] > lower.fractal)&(level<1)){
        level = 1
        print(paste0(level,"   :index",i))
      } 
      lower.fractal = outdf$LowerFrac[i]
    }   
    ################################################################################
    ################################################################################
    if (is.na(outdf$UpperFrac[i])==0){
      ################################################################################
      if (outdf$UpperFrac[i] < upper.fractal){
        level = -1
        print(paste0(level,"   :index",i))
        upper.fractal = outdf$UpperFrac[i]
      }
      
      ################################################################################
      ################################################################################
      if ((level == 1)&(outdf$UpperFrac[i] > upper.fractal)){
        level = 2
        upper.fractal = outdf$UpperFrac[i]
        i=i+2
        print(paste0(level,"   :index",i))
      }
      
    }
    ################################################################################
    ################################################################################
    
    # & ((outdf$Low[i]-0.0050) > outdf$SAR[i]) &(outdf$Low[i]<(lower.fractal+0.01))
    if (i>(2*pars[3])){
      if ( (level == 2) & (outdf$RSI[i]<75)  & (outdf$SenkouA[i] > outdf$SenkouB[i]) & (outdf$Tenkan[i] > outdf$Kijun[i]) & (outdf$Tenkan[i] > outdf$Low[i])  & (outdf$High[i] > outdf$Tenkan[i])){
        BUY = 1
        SL = lower.fractal
        price = outdf$Close[i]
        tradehin=rbind(tradehin,c("BUY",price,i,SL))
        print(paste0("BUY in at = ",price,"   :i=",i,"   :SL=",SL))
      }
      ################################################################################
    }
    
    ################################################################################
    #                               Create TP and SL                               #
    ################################################################################  
    warning=0
    while(BUY == 1){
      i=i+1
      
      if (outdf$Close[i]<SL){
        profit = outdf$Close[i] - price +profit
        tradehout=rbind(tradehout,c("Stop Loss",i,(outdf$Close[i] - price),profit))
        print(paste0(profit,"  :By Stop Lost","    :BUY=",BUY,"   :SELL=",SELL,"   :i=",i))
        warning = 0
        BUY = 0
        level = 0
      } else if (outdf$RSI[i]>75){
        profit = outdf$Close[i] - price +profit
        tradehout=rbind(tradehout,c("RSI overbuy",i,(outdf$Close[i] - price),profit))
        print(paste0(profit,"  :By RSI overbuy","    :BUY=",BUY,"   :SELL=",SELL,"   :i=",i))
        warning = 0
        BUY = 0
        level = 0
      } else if (is.na(outdf$UpperFrac[i])==0){
        if ((warning == 0)&(outdf$UpperFrac[i] < upper.fractal)){
          warning = 1
        }
        if ((warning == 2)&(outdf$UpperFrac[i] < upper.fractal)){
          warning = 3
        }
        if ((warning == 4)&(outdf$UpperFrac[i] < upper.fractal)){
          profit =  outdf$Close[(i+2)] - price +profit
          tradehout=rbind(tradehout,c("Violation",(i+2),(outdf$Close[i+2] - price),profit))
          print(paste0(profit,"  :By violation","    :BUY=",BUY,"   :SELL=",SELL,"   :i=",i))
          BUY = 0
          warning = 0
          level = 0
        }
        upper.fractal = outdf$UpperFrac[i]
      } else if (is.na(outdf$LowerFrac[i])==0){
        if ((warning == 1)&(outdf$LowerFrac[i] < lower.fractal)){
          warning = 2
        }
        if ((warning == 3)&(outdf$LowerFrac[i] < lower.fractal)){
          warning = 4
        }
        lower.fractal = outdf$LowerFrac[i]
      }
      if ((BUY==1)&(i==n)){
        BUY=0
        profit = outdf$Close[i] - price +profit
        tradehout=rbind(tradehout,c(":By end of data",i,(outdf$Close[i] - price),profit))
      }
    }

    ################################################################################
  }

  
  print(paste0(profit,"  :By end of data","    :BUY=",BUY,"   :SELL=",SELL,"   :i=",i))
  
  
  
  
  
  
  
  # 
  n = length(out[,1])-pars[2]
  upper.fractal=NA
  lower.fractal=NA
  i = pars[2]
  level = 0
  
  BUY = 0
  SELL = 0
  
  while (i<n){
    i = i + 1
    
    if ((is.na(outdf$UpperFrac[i])==0)&(is.na(upper.fractal))){
      upper.fractal=outdf$UpperFrac[i]
    }
    
    if ((is.na(outdf$LowerFrac[i])==0)&(is.na(lower.fractal))){
      lower.fractal=outdf$LowerFrac[i]
    }
    
    if ((is.na(outdf$UpperFrac[i])==0)&(level>-1)){
      if (outdf$UpperFrac[i] < upper.fractal){
        level = -1
        print(paste0(level,"   :index",i))
      }
      upper.fractal = outdf$UpperFrac[i]
    }
    if (is.na(outdf$LowerFrac[i])==0){
      
      if (outdf$LowerFrac[i] > lower.fractal){
        level = 1
        print(paste0(level,"   :index",i))
        lower.fractal = outdf$LowerFrac[i]
      }
      if ((level == -1)&(outdf$LowerFrac[i] < lower.fractal)){
        level = -2
        lower.fractal = outdf$LowerFrac[i]
        i=i+2
        print(paste0(level,"   :index",i))
      }
    }
    
    # (outdf$SAR[i] > (outdf$High[i]+0.0050)) &(outdf$High[i]<(upper.fractal+0.01))
    if (i>(2*pars[3])){
      if ( (level == -2) & (outdf$RSI[i]>25) & (outdf$High[i] > outdf$Tenkan[i]) & (outdf$Tenkan[i] > outdf$Low[i]) & (outdf$SenkouA[i] < outdf$SenkouB[i]) & (outdf$Tenkan[i] < outdf$Kijun[i])){
        SELL = 1
        SL = upper.fractal
        price = outdf$Close[i]
        tradehin=rbind(tradehin,c("SELL",price,i,SL))
        print(paste0("SELL in at = ",price,"   :i=",i,"   :SL=",SL))
      }
    }
    
    warning=0
    while(SELL == 1){
      i=i+1
      if (outdf$Close[i]>SL){
        profit = price - outdf$Close[i] +profit
        tradehout=rbind(tradehout,c("Stop Loss",i,(price - outdf$Close[i]),profit))
        print(paste0(profit,"  :by Stop Lost","    :BUY=",BUY,"   :SELL=",SELL,"   :i=",i))
        SELL = 0
        warning = 0
        level = 0
      } else if (outdf$RSI[i]<25){
        profit = price - outdf$Close[i] +profit
        tradehout=rbind(tradehout,c("RSI oversell",i,(price - outdf$Close[i]),profit))
        print(paste0(profit,"  :By RSI oversell","    :BUY=",BUY,"   :SELL=",SELL,"   :i=",i))
        warning = 0
        SELL = 0
        level = 0
      } else if (is.na(outdf$UpperFrac[i])==0){
        if ((warning == 1)&(outdf$UpperFrac[i] > upper.fractal)){
          warning = 2
        }
        if ((warning == 3)&(outdf$UpperFrac[i] > upper.fractal)){
          warning = 4
        }
        upper.fractal = outdf$UpperFrac[i]
      } else if(is.na(outdf$LowerFrac[i])==0){
        if ((warning == 0)&(outdf$LowerFrac[i] > lower.fractal)){
          warning = 1
        }
        if ((warning == 2)&(outdf$LowerFrac[i] > lower.fractal)){
          warning = 3
        }
        if ((warning == 4)&(outdf$LowerFrac[i] > lower.fractal)){
          profit = price - outdf$Close[(i+2)] +profit
          tradehout=rbind(tradehout,c("Violation",(i+2),(price - outdf$Close[i+2]),profit))
          print(paste0(profit,"  :By violation","    :BUY=",BUY,"   :SELL=",SELL,"   :i=",i))
          warning = 0
          SELL = 0
          level = 0
        }
        lower.fractal = outdf$LowerFrac[i]
      }
      if ((SELL==1)&(i==n)){
        SELL=0
        profit = price - outdf$Close[(i)] +profit
        tradehout=rbind(tradehout,c(":By end of data",i,(price - outdf$Close[i]),profit))
      }
      
    }
  }
  
  
  print(paste0(profit,"  :By end of data","    :BUY=",BUY,"   :SELL=",SELL,"   :i=",i))
  
  
  tradehist=cbind(tradehin,tradehout)
  tradehist=tradehist[-1,]
  colnames(tradehist)=c("Option","Price","IOpen","SL","CloseTrade","IClose","Profit","TotalProfit")
  
  
  for (c in c(2:4,6:8)){
    tradehist[,c]=as.numeric(tradehist[,c])
  }
  
  
  
  return(list(outdf,tradehist))
}





BacktestingMain = function(currencyname){
  currency=read.csv(paste0(currencyname," Historical Data.csv"))    # Need to edit for different currency.
  
  
  
  
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
  
  
  
  
  # png(paste0("C:/Users/USER/Desktop/FOREX/Ichimoku",currencyname,".png"),res = 300,height=5000,width = (20*length(len)+4000))
  # par(mar=c(9,3,3,3))
  # ################################################################################
  # # Function for Plotting Candlestick and Ichimoku
  # ################################################################################
  # plotichimoku(data,out,len)
  # ################################################################################
  # # Function for Plotting William Fractals
  # ################################################################################
  # points(out[,11], pch = 17)
  # points(out[,12], pch = 25,col="black",bg="black")
  # dev.off()
  # 
  
  
  
  listofout=Strategy1(out,pars)
  outdf=listofout[[1]]
  tradehist=listofout[[2]]
  
  
  

  # png(paste0("C:/Users/USER/Desktop/FOREX/PurchaseLINE",currencyname,".png"),res = 300,height=5000,width = (20*length(len)+4000))
  # par(mar=c(9,3,3,3))
  # 
  # ################################################################################
  # # Function for Plotting Candlestick and Ichimoku
  # ################################################################################
  # plotichimoku(data,out,len)
  # ################################################################################
  # # Function for Plotting William Fractals
  # ################################################################################
  # points(out[,11], pch = 17)
  # points(out[,12], pch = 25,col="black",bg="black")
  # ################################################################################
  # # Function for Plotting Trading Profit and Location
  # ################################################################################
  # tradecol=ifelse(tradehist$Profit>0,"purple", "brown")
  # for (lin in 1:length(tradehist[,1])){
  #   lines(tradehist[lin,c(3,6)],outdf$Close[as.matrix(tradehist[lin,c(3,6)])],lwd=5,col=tradecol[lin])
  # }
  # 
  # points(outdf$SAR)
  # 
  # dev.off()
  
  tradehist=tradehist
  return(tradehist)
  # return(list(outdf,tradehist))
  
}




