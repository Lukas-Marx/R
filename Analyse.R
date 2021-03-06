#Lukas Marx, 07.10.2020 Fürth, Friedrich-Alexander-Universität Erlangen-Nuernberg
#Lukas.Marx@Fau.de
library(tseries)
library(zoo)
library(quantmod)
compute_normalReturn.marketmodel <- function(estimationWindow.prices_stock, estimationWindow.prices_market, actualReturn.price_market) {
  # Ordinary Least Squares (OLS)
  M = lm(estimationWindow.prices_stock ~ estimationWindow.prices_market)
  
  nd = data.frame(estimationWindow.prices_market = actualReturn.price_market)
  normalReturn = predict(M, newdata = nd)
  
  return(normalReturn)
}


compute_normalReturn.constantmeanmodel <- function(estimationWindow.prices_stock) {
  
  normalReturn = mean(estimationWindow.prices_stock)
  
  return(normalReturn)
}
abnormalReturn <- function(prices_stock, prices_market=NULL, from=NULL, to=NULL,
                           model = "marketmodel",
                           estimationWindowLength = 10, c = 10, attributeOfInterest = "Close",
                           showPlot = FALSE) {
  
  if (is.null(prices_stock)) {
    stop("Argument 'prices_stock' is required.")
  }
  
  if (class(model) != 'character') {
    stop("Argument 'model' must be a character object.")
  }
  
  if (class(attributeOfInterest) != 'character') {
    stop("Argument 'attributeOfInterest' must be a character object.")
  }
  
  if (class(estimationWindowLength) != 'numeric') {
    stop("Argument 'estimationWindowLength' must be a numeric object")
  }
  
  if (class(c) != 'numeric') {
    stop("Argument 'c' must be a numeric object")
  }
  
  if (class(showPlot) != 'logical') {
    stop("Argument 'showPlot' must be a logical object")
  }
  
  if (model == "marketmodel" && is.null(prices_market)) {
    stop("Argument 'prices_market' is required for the market model.")
  }
  
  if (class(prices_stock) == 'character') {
    # if prices_stock contains a stock symbol, utilize 'quantmod' to download data from Yahoo Finance.
    prices_stock.symbol = prices_stock
    
    if (is.null(from) || is.null(to)) {
      stop("Argument 'from' and 'to' are required.")
    }
    
    if (as.POSIXct(from) > as.POSIXct(to)) {
      stop("Argument 'from' must define a date preceding that one specified in argument 'to'.")
    }
    
    options("getSymbols.warning4.0"=FALSE)
    data <- getSymbols(prices_stock.symbol, src='yahoo', from=from, to=to, auto.assign=FALSE)
    
    prices_stock <- data.frame(Date=index(data), coredata(data))
    names(prices_stock) = c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    comment(prices_stock) = prices_stock.symbol
    prices_stock$Date <- as.POSIXct(prices_stock$Date)
    prices_stock <- prices_stock[order(prices_stock$Date),]
    
    if (class(prices_market) == 'character') {
      prices_market.symbol = prices_market
      data <- getSymbols(prices_market.symbol, src='yahoo', from=from, to=to, auto.assign=FALSE)
      
      prices_market <- data.frame(Date=index(data), coredata(data))
      names(prices_market) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
      comment(prices_market) = prices_market.symbol
      prices_market$Date <- as.POSIXct(prices_market$Date)
      prices_market <- prices_market[order(prices_market$Date),]
      
      # make sure both data sets contain the same dates.
      prices_stock = prices_stock[prices_stock$Date %in% prices_market$Date,]
      prices_market = prices_market[prices_market$Date %in% prices_stock$Date,]
    }
  }
  
  if(class(prices_stock) != 'data.frame') {
    stop("Argument 'prices_stock' must be a data.frame")
  }
  
  if(class(prices_market) != 'NULL' && class(prices_market) != 'data.frame') {
    stop("Argument 'prices_market' must be a data.frame or NULL")
  }
  
  # create subset of data.frame, according to arguments 'from' and 'to'.
  if (!is.null(from)) {
    prices_stock = prices_stock[prices_stock$Date > as.POSIXct(from),]
    if (!is.null(prices_market)) {
      prices_market = prices_market[prices_market$Date > as.POSIXct(from),]
    }
  }
  if (!is.null(to)) {
    prices_stock = prices_stock[prices_stock$Date < as.POSIXct(to),]
    if (!is.null(prices_market)) {
      prices_market = prices_market[prices_market$Date < as.POSIXct(to),]
    }
  }
  
  if (nrow(prices_stock) < (estimationWindowLength + 1)) {
    stop(paste("Error! Not enough data points in 'prices_stock'. Number of rows:", nrow(prices_stock)))
  }
  
  # check if queried data.frame attributes exist.
  if (!("Date" %in% names(prices_stock)) || !attributeOfInterest %in% names(prices_stock) ) {
    stop(paste0("Error! Please check prices_stock attributes'. Available Attributes: ",
                paste(names(prices_stock), collapse = ", ")))
  }
  
  if (!is.null(prices_market)) {
    if (!("Date" %in% names(prices_market)) || !attributeOfInterest %in% names(prices_market) ) {
      stop(paste0("Error! Please check prices_market attributes'. Available Attributes: ",
                  paste(names(prices_market), collapse = ", ")))
    }
    
    # check wether prices_market and prices_stock data sets fit together
    if (nrow(prices_market) != nrow(prices_stock)) {
      stop(paste("Error! prices_market and prices_stock data should be of same length:",
                 nrow(prices_market), "vs.", nrow(prices_stock)))
    }
    
    if (any(prices_market$Date != prices_stock$Date)) {
      stop("Error! Dates of prices_market and prices_stock data sets do not match.")
    }
  }
  
  if (estimationWindowLength < 1) {
    stop("An estimation window size of at least 5 time steps is recommend for the regression to produce meaningful results.")
  }
  
  indices = (estimationWindowLength + 1):nrow(prices_stock)
  collect.abnRet = c()
  for (idx in indices) {
    # indices of data points used for estimating the OLS model
    indices.Estimation = (idx - estimationWindowLength):(idx - 1)
    
    # compute abnormal return
    if (model == 'marketmodel') {
      normalReturn = compute_normalReturn.marketmodel(prices_stock[indices.Estimation,][[attributeOfInterest]],
                                                      prices_market[indices.Estimation,][[attributeOfInterest]],
                                                      prices_market[idx,][[attributeOfInterest]])
    } else if (model == 'constantmeanmodel') {
      normalReturn = compute_normalReturn.constantmeanmodel(prices_stock[indices.Estimation,][[attributeOfInterest]])
    } else {
      # default case
      stop(paste("Error! Unknown normal return model specified:", model))
    }
    
    abnormal = prices_stock[idx,][[attributeOfInterest]] - normalReturn
    
    row = data.frame(
      Date = prices_stock$Date[idx],
      abnormalReturn = abnormal,
      cumulativeAbnormalReturn = NA,
      stockReturn = prices_stock[idx,][[attributeOfInterest]]
    )
    
    collect.abnRet = rbind(collect.abnRet, row)
  }
  # name each row according to its corresponding index.
  row.names(collect.abnRet) <- indices
  
  # Add CumulativeAdbnormalReturns
  if (c >= 2 && nrow(collect.abnRet)>c) {
    for (i in (c):nrow(collect.abnRet)) {
      collect.abnRet$cumulativeAbnormalReturn[i] <- sum(collect.abnRet$abnormalReturn[(i-c):i])
    }
  } else if (c < 2) {
    warning("Warning! Accumulating abnormalReturns makes only sense for c >= 2.")
  }
  
  if (showPlot == TRUE) {
    plotEventStudy(prices_stock,
                   prices_market,
                   attributeOfInterest,
                   estimationWindowLength,
                   collect.abnRet)
  }
  
  return(collect.abnRet)
}


#' @importFrom graphics plot legend points
plotEventStudy <- function(prices_stock, prices_market,
                           attributeOfInterest,
                           estimationWindowLength,
                           collect.abnRet) {
  if (!is.null(prices_market)) {
    title <- paste0("stock: '", comment(prices_stock),
                    "' - market: '", comment(prices_market),
                    "'\n(", format(min(prices_stock$Date), format="%Y-%m-%d"), " - ",
                    format(max(prices_stock$Date), format="%Y-%m-%d"), ")")
  } else {
    title <- paste0("stock: '", comment(prices_stock),
                    "'\n(", format(min(prices_stock$Date), format="%Y-%m-%d"), " - ",
                    format(max(prices_stock$Date), format="%Y-%m-%d"), ")")
  }
  
  subtitle <- paste("Length of estimation window:", estimationWindowLength)
  
  # plot whole time series of given prices_stock
  # p <- qplot(prices_stock$Date, prices_stock[[attributeOfInterest]], main=title, xlab='Date', ylab=paste("Daily", attributeOfInterest))
  plot(prices_stock$Date, prices_stock[[attributeOfInterest]],
       col = "grey",
       xlab = "Date", ylab = paste("Daily", attributeOfInterest),
       main = title,
       sub = subtitle)
  
  legend("topleft", c("abnormalReturn negative                ",
                      "abnormalReturn positive"),
         lty = FALSE,
         col = c("red", "green"),
         cex = 1, pch = 4)
  
  # mark events with positive and negative abnormal returns accordingly.
  neg = collect.abnRet[collect.abnRet$abnormalReturn <= 0, ]
  pos = collect.abnRet[collect.abnRet$abnormalReturn > 0, ]
  
  points(neg$Date,
         neg$stockReturn,
         col = "red",
         pch = 4)
  points(pos$Date,
         pos$stockReturn,
         col = "green",
         pch = 4)
}
Start = "2020-03-01"
Ende = "2020-10-01"

Subsequence1.Start= "2020-03-01"
Subsequence1.Ende = "2020-06-30"
Regressionszeitraumstart = as.POSIXct(Subsequence1.Start) - (250*24*60*60)
Subsequence2.Start = "2020-07-01"
Subsequence2.Ende ="2020-10-02"

Aktie = "CEV.DE"
Vergleichsindex = "^GDAXI"

AR = abnormalReturn(Aktie,Vergleichsindex,Subsequence1.Start, Subsequence2.Ende,model="marketmodel", estimationWindowLength = 10,
                         c=10, attributeOfInterest = "Adjusted",showPlot = TRUE)
AV = abnormalReturn(Aktie,Vergleichsindex,Subsequence1.Start, Subsequence2.Ende,model="marketmodel", estimationWindowLength = 10,
                         c=10, attributeOfInterest = "Volume",showPlot = TRUE)

Aktie.volume = get.hist.quote(instrument = Aktie, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)
Aktie.volume = na.omit(Aktie.volume)

Aktie.Adjusted = get.hist.quote(instrument = Aktie, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)

Aktie.Adjusted = na.omit(Aktie.Adjusted)

Aktie.volumesub1 = get.hist.quote(instrument = Aktie, 
                                   start = Subsequence1.Start, end = Subsequence1.Ende,
                                   quote = "Volume", provider = "yahoo",
                                   compression = "d"
)

Aktie.volumesub1 = na.omit(Aktie.volumesub1)

Aktie.Adjustedsub1 = get.hist.quote(instrument = Aktie, 
                                     start = Subsequence1.Start, end = Subsequence1.Ende,
                                     quote = "Adjusted", provider = "yahoo",
                                     compression = "d"
)

Aktie.Adjustedsub1 = na.omit(Aktie.Adjustedsub1)

Aktie.volumesub2 = get.hist.quote(instrument = Aktie, 
                                   start = Subsequence2.Start, end = Subsequence2.Ende,
                                   quote = "Volume", provider = "yahoo",
                                   compression = "d"
)

Aktie.volumesub2 = na.omit(Aktie.volumesub2)

Aktie.Adjustedsub2 = get.hist.quote(instrument = Aktie, 
                                     start = Subsequence2.Start, end = Subsequence2.Ende,
                                     quote = "Adjusted", provider = "yahoo",
                                     compression = "d"
)

Aktie.Adjustedsub2 = na.omit(Aktie.Adjustedsub2)

ERW_whole_vol = mean (Aktie.volume)
Var_single_vol = (Aktie.volume - ERW_whole_vol)^2
Var_whole_vol = sum(Var_single_vol) / length(Var_single_vol)
StabW_whole_vol = sqrt(Var_whole_vol)
Stabw_whole_vol_up = ERW_whole_vol + StabW_whole_vol
Stabw_whole_vol_down = ERW_whole_vol - StabW_whole_vol


ERW_whole_vol_sub1 = mean (Aktie.volumesub1)
Var_single_vol_sub1 = (Aktie.volumesub1 - ERW_whole_vol_sub1)^2
Var_whole_vol_sub1 = sum(Var_single_vol_sub1) / length(Var_single_vol_sub1)
StabW_whole_vol_sub1 = sqrt(Var_whole_vol_sub1)
Stabw_whole_vol_sub1_up = ERW_whole_vol_sub1 + StabW_whole_vol_sub1
Stabw_whole_vol_sub1_down = ERW_whole_vol_sub1 - StabW_whole_vol_sub1


ERW_whole_vol_sub2 = mean (Aktie.volumesub2)
Var_single_vol_sub2 = (Aktie.volumesub2 - ERW_whole_vol_sub2)^2
Var_whole_vol_sub2 = sum(Var_single_vol_sub2) / length(Var_single_vol_sub2)
StabW_whole_vol_sub2 = sqrt(Var_whole_vol_sub2)
Stabw_whole_vol_sub2_up = ERW_whole_vol_sub2 + StabW_whole_vol_sub2
Stabw_whole_vol_sub2_down = ERW_whole_vol_sub2 - StabW_whole_vol_sub2


#png("Vol.png", width = 1280, height = 720)
plot(Aktie.volume, xlab="gesamter Zeitraum", ylab='Volume  (black)', col="black")
abline(v=as.Date(Subsequence2.Start),col="green")
abline(h = Stabw_whole_vol_up, lty =2, col= "blue")
abline(h = Stabw_whole_vol_down, lty =2, col= "blue")
abline(h = ERW_whole_vol, col= "blue")
legend("topleft", legend = c("EV", "SD","Splitdate"), col = c("blue","blue","green"), lty = 1:2,cex = 0.6)
#dev.off()


#png("Vol_with_subsequence.png", width = 1280, height = 720)
plot(Aktie.volume, xlab="gesamter Zeitraum", ylab='Volume  (black)', col="black")
abline(v=as.Date(Subsequence2.Start),col="green")
abline(h = Stabw_whole_vol_sub1_up, lty =2, col= "blue")
abline(h = Stabw_whole_vol_sub1_down, lty =2, col= "blue")
abline(h = ERW_whole_vol_sub1, col= "blue")
abline(h = Stabw_whole_vol_sub2_up, lty =2, col= "red")
abline(h = Stabw_whole_vol_sub2_down, lty =2, col= "red")
abline(h = ERW_whole_vol_sub2, col= "red")
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post","Splitdate"), col = c("blue","blue","red","red","green"), lty = 1:2,cex = 0.6)
#dev.off()


ERW_whole_adj = mean (Aktie.Adjusted)
Var_single_adj = (Aktie.Adjusted - ERW_whole_adj)^2
Var_whole_adj = sum(Var_single_adj) / length(Var_single_adj)
StabW_whole_adj = sqrt(Var_whole_adj)
Stabw_whole_adj_up = ERW_whole_adj + StabW_whole_adj
Stabw_whole_adj_down = ERW_whole_adj - StabW_whole_adj


ERW_whole_adj_sub1 = mean (Aktie.Adjustedsub1)
Var_single_adj_sub1 = (Aktie.Adjustedsub1 - ERW_whole_adj_sub1)^2
Var_whole_adj_sub1 = sum(Var_single_adj_sub1) / length(Var_single_adj_sub1)
StabW_whole_adj_sub1 = sqrt(Var_whole_adj_sub1)
Stabw_whole_adj_sub1_up = ERW_whole_adj_sub1 + StabW_whole_adj_sub1
Stabw_whole_adj_sub1_down = ERW_whole_adj_sub1 - StabW_whole_adj_sub1


ERW_whole_adj_sub2 = mean (Aktie.Adjustedsub2)
Var_single_adj_sub2 = (Aktie.Adjustedsub2 - ERW_whole_adj_sub2)^2
Var_whole_adj_sub2 = sum(Var_single_adj_sub2) / length(Var_single_adj_sub2)
StabW_whole_adj_sub2 = sqrt(Var_whole_adj_sub2)
Stabw_whole_adj_sub2_up = ERW_whole_adj_sub2 + StabW_whole_adj_sub2
Stabw_whole_adj_sub2_down = ERW_whole_adj_sub2 - StabW_whole_adj_sub2




#png("Adj.png", width = 1280, height = 720)
plot(Aktie.Adjusted, xlab="gesamter Zeitraum", ylab='Adjusted Stock  (black)', col="black")
abline(v=as.Date(Subsequence2.Start),col="green")
abline(h = Stabw_whole_adj_up, lty =2, col= "blue")
abline(h = Stabw_whole_adj_down, lty =2, col= "blue")
abline(h = ERW_whole_adj, col= "blue")
legend("topleft", legend = c("EV", "SD","Splitdate"), col = c("blue","blue","green"), lty = 1:2,cex = 0.6)
#dev.off()


#png("Adj_with_subsequence.png", width = 1280, height = 720)
plot(Aktie.Adjusted, xlab="gesamter Zeitraum", ylab='Adjusted Stock  (black)', col="black")
abline(v=as.Date(Subsequence2.Start),col="green")
abline(h = Stabw_whole_adj_sub1_up, lty =2, col= "blue")
abline(h = Stabw_whole_adj_sub1_down, lty =2, col= "blue")
abline(h = ERW_whole_adj_sub1, col= "blue")
abline(h = Stabw_whole_adj_sub2_up, lty =2, col= "red")
abline(h = Stabw_whole_adj_sub2_down, lty =2, col= "red")
abline(h = ERW_whole_adj_sub2, col= "red")
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post","Splitdate"), col = c("blue","blue","red","red","green"), lty = 1:2,cex = 0.6)
dev.off()




#png("APPL.png", width = 1280, height = 720)
par(mfrow=c(2,1))
plot(Aktie.volume, xlab="gesamter Zeitraum", ylab='Volume  (black)', col="black")
abline(v=as.Date(Subsequence2.Start),col="green")
abline(h = Stabw_whole_vol_sub1_up, lty =2, col= "blue")
abline(h = Stabw_whole_vol_sub1_down, lty =2, col= "blue")
abline(h = ERW_whole_vol_sub1, col= "blue")
abline(h = Stabw_whole_vol_sub2_up, lty =2, col= "red")
abline(h = Stabw_whole_vol_sub2_down, lty =2, col= "red")
abline(h = ERW_whole_vol_sub2, col= "red")
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post","Splitdate"), col = c("blue","blue","red","red","green"), lty = 1:2,cex = 0.6)

plot(Aktie.Adjusted, xlab="gesamter Zeitraum", ylab='Adjusted Stock  (black)', col="black")
abline(v=as.Date(Subsequence2.Start),col="green")
abline(h = Stabw_whole_adj_sub1_up, lty =2, col= "blue")
abline(h = Stabw_whole_adj_sub1_down, lty =2, col= "blue")
abline(h = ERW_whole_adj_sub1, col= "blue")
abline(h = Stabw_whole_adj_sub2_up, lty =2, col= "red")
abline(h = Stabw_whole_adj_sub2_down, lty =2, col= "red")
abline(h = ERW_whole_adj_sub2, col= "red")
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post","Splitdate"), col = c("blue","blue","red","red","green"), lty = 1:2,cex = 0.6)

#dev.off()

reg_line_AR = lm(AR$abnormalReturn~AR$Date)
AR_reg = fitted.values(reg_line_AR)
AR_reg_res = residuals(reg_line_AR)

reg_line_AV = lm(AV$abnormalReturn~AV$Date)
AV_reg = fitted.values(reg_line_AV)
AV_reg_res = residuals(reg_line_AV)

plot(AR$Date,AR$abnormalReturn, xlab = "Datum", ylab="Daily Abnormal Returns")
abline (h = 0,col= "black")
abline (v= as.POSIXct("2020-08-11 02:00:00"), lty = 2,col ="red")
abline (v= as.POSIXct("2020-08-28 02:00:00"), lty = 2,col ="green")
abline (reg_line_AR,col ="blue")

plot(AV$Date, AV$abnormalReturn, xlab = "Datum", ylab="Daily Abnormal Volume")
abline (h = 0, col="black")
abline (v= as.POSIXct("2020-08-11 02:00:00"), lty = 2,col ="red")
abline (v= as.POSIXct("2020-08-28 02:00:00"), lty = 2,col ="green")
abline (reg_line_AV,col ="blue")

par(mfrow=c(1,1))
plot(window(Aktie.Adjusted, start = as.Date(Subsequence1.Start), end = as.Date(Subsequence2.Ende)), xlab="gesamter Zeitraum", ylab='Adjusted Stock  (black)', col="black")
abline(v=as.Date(Subsequence2.Start),col="green")
abline(h = Stabw_whole_adj_sub1_up, lty =2, col= "blue")
abline(h = Stabw_whole_adj_sub1_down, lty =2, col= "blue")
abline(h = ERW_whole_adj_sub1, col= "blue")
abline(h = Stabw_whole_adj_sub2_up, lty =2, col= "red")
abline(h = Stabw_whole_adj_sub2_down, lty =2, col= "red")
abline(h = ERW_whole_adj_sub2, col= "red")
par(new=TRUE)
plot(AR$Date,AR$abnormalReturn,type = "n", xlab=NA, ylab=NA, col="purple",axes = F)
abline (h=0, col="black", lty = 2)
abline (reg_line_AR, col="purple")
axis(side = 4)
mtext(side=4,'Regression der AR (Lila)')
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post","Eventdate", "AR = 0"), col = c("blue","blue","red","red","green","black"), lty = 1:2,cex = 0.4)
