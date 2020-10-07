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
Aktie = "CEV.DE"
Vergleichsindex = "^GDAXI"

Start = "2018-10-07"
Ende = "2020-10-08"

Subsequence.Start = "2020-02-01"
Subsequence.Ende = "2020-10-08"

AR = abnormalReturn(prices_stock = Aktie,prices_market = Vergleichsindex,Subsequence.Start,Subsequence.Ende,model="marketmodel",estimationWindowLength = 10,c=5, attributeOfInterest = "Adjusted",showPlot = FALSE)
AV = abnormalReturn(prices_stock = Aktie,prices_market = Vergleichsindex,Subsequence.Start,Subsequence.Ende,model="marketmodel",
                    estimationWindowLength = 10,c=5, attributeOfInterest = "Volume",showPlot = FALSE)

Aktie.volume1 = get.hist.quote(instrument = Aktie, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)
Aktie.volume = na.omit(Aktie.volume1)
#Bestimmte das maximale Trading volume
Aktie.max.volume = max(Aktie.volume)

#Bestimme den Erwartungswert des Tradingvolumens
Aktie.mean.volume = mean(Aktie.volume)
#Bestimme die einzelnen varianzwerte
Aktie.varianz.volume.einzel= (Aktie.volume - Aktie.mean.volume)^2
#Bestimme die einzelnen Standardabweichungen
Aktie.Stabw.volume.einzel = (Aktie.volume - Aktie.mean.volume)
#Bestimme die varianz über die gesamte Zeit
Aktie.varianz.volume.gesamt= sum(Aktie.varianz.volume.einzel) / length(Aktie.varianz.volume.einzel)
#Bestimme die Standardabweichung über die gesamte Zeit
Aktie.Stabw.volume.gesamt = sqrt(Aktie.varianz.volume.gesamt)

Aktie.Stabw.volume.up = Aktie.mean.volume + Aktie.Stabw.volume.gesamt
Aktie.Stabw.volume.down = Aktie.mean.volume - Aktie.Stabw.volume.gesamt
#Bestimmte Abweichungsfaktor für max
Aktie.Abweichungsfaktor.volume = (Aktie.max.volume - Aktie.Stabw.volume.gesamt)/Aktie.Stabw.volume.gesamt



Aktie.Adjusted1 = get.hist.quote(instrument = Aktie, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)
Aktie.Adjusted = na.omit(Aktie.Adjusted1)
#Bestimmte das maximale Trading Adjusted
Aktie.max.Adjusted = max(Aktie.Adjusted)

#Bestimme den Erwartungswert des TradingAdjustedns
Aktie.mean.Adjusted = mean(Aktie.Adjusted)
#Bestimme die einzelnen varianzwerte
Aktie.varianz.Adjusted.einzel= (Aktie.Adjusted - Aktie.mean.Adjusted)^2
#Bestimme die einzelnen Standardabweichungen
Aktie.Stabw.Adjusted.einzel = (Aktie.Adjusted - Aktie.mean.Adjusted)
#Bestimme die varianz über die gesamte Zeit
Aktie.varianz.Adjusted.gesamt= sum(Aktie.varianz.Adjusted.einzel) / length(Aktie.varianz.Adjusted.einzel)
#Bestimme die Standardabweichung über die gesamte Zeit
Aktie.Stabw.Adjusted.gesamt = sqrt(Aktie.varianz.Adjusted.gesamt)

Aktie.Stabw.Adjusted.up = Aktie.mean.Adjusted + Aktie.Stabw.Adjusted.gesamt
Aktie.Stabw.Adjusted.down = Aktie.mean.Adjusted - Aktie.Stabw.Adjusted.gesamt

#Bestimmte Abweichungsfaktor für max
Aktie.Abweichungsfaktor.Adjusted = (Aktie.max.Adjusted - Aktie.Stabw.Adjusted.gesamt)/Aktie.Stabw.Adjusted.gesamt


Index.Adjusted1 = get.hist.quote(instrument = Vergleichsindex, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)
Index.Adjusted = na.omit(Index.Adjusted1)

#Bestimmte das maximale Trading Adjusted
Index.max.Adjusted = max(Index.Adjusted)

#Bestimme den Erwartungswert des TradingAdjustedns
Index.mean.Adjusted = mean(Index.Adjusted)
#Bestimme die einzelnen varianzwerte
Index.varianz.Adjusted.einzel= (Index.Adjusted - Index.mean.Adjusted)^2
#Bestimme die einzelnen Standardabweichungen
Index.Stabw.Adjusted.einzel = (Index.Adjusted - Index.mean.Adjusted)
#Bestimme die varianz über die gesamte Zeit
Index.varianz.Adjusted.gesamt= sum(Index.varianz.Adjusted.einzel) / length(Index.varianz.Adjusted.einzel)
#Bestimme die Standardabweichung über die gesamte Zeit
Index.Stabw.Adjusted.gesamt = sqrt(Index.varianz.Adjusted.gesamt)

Index.Stabw.Adjusted.up = Index.mean.Adjusted + Index.Stabw.Adjusted.gesamt
Index.Stabw.Adjusted.down = Index.mean.Adjusted - Index.Stabw.Adjusted.gesamt

#Bestimmte Abweichungsfaktor für max
Index.Abweichungsfaktor.Adjusted = (Index.max.Adjusted - Index.Stabw.Adjusted.gesamt)/Index.Stabw.Adjusted.gesamt

#Aktie Adjusted und Volume
plot(Aktie.Adjusted, xlab="Zeitraum", ylab="Aktie (green)", col="green")
par(new=TRUE)
plot(Aktie.volume, xlab=NA, ylab=NA, col="purple",axes = F)
axis(side = 4)
mtext(side=4,'Volume (purple)')

#Aktie Volumen 
plot(Aktie.volume, xlab="Zeitraum", ylab='Volume (purple)', col="purple")
abline(h = Aktie.Stabw.volume.down, lty =2, col= "blue")
abline(h = Aktie.Stabw.volume.up, lty =2, col= "blue")
abline(h = Aktie.mean.volume, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

#Aktie Adjusted 
plot(Aktie.Adjusted, xlab="Zeitraum", ylab='Aktie (green)', col="green")
abline(h = Aktie.Stabw.Adjusted.down, lty =2, col= "blue")
abline(h = Aktie.Stabw.Adjusted.up, lty =2, col= "blue")
abline(h = Aktie.mean.Adjusted, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

#Subsequence Volume
Teilzeitreihe.Volume = window(Aktie.volume, start = as.Date(Subsequence.Start), end = as.Date(Subsequence.Ende))
#Subsequence Adjusted
Teilzeitreihe.Adjusted = window(Aktie.Adjusted, start = as.Date(Subsequence.Start), end = as.Date(Subsequence.Ende))               

#Aktie Volumen Subsequence
plot(Teilzeitreihe.Volume, xlab="Zeitraum", ylab='Volume (purple)', col="purple")
abline(h = Aktie.Stabw.volume.down, lty =2, col= "blue")
abline(h = Aktie.Stabw.volume.up, lty =2, col= "blue")
abline(h = Aktie.mean.volume, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

#Aktie Adjusted Subsequence
plot(Teilzeitreihe.Adjusted, xlab="Zeitraum", ylab='Aktie (green)', col="green")
abline(h = Aktie.Stabw.Adjusted.down, lty =2, col= "blue")
abline(h = Aktie.Stabw.Adjusted.up, lty =2, col= "blue")
abline(h = Aktie.mean.Adjusted, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

#Index Adjusted 
plot(Index.Adjusted, xlab="Zeitraum", ylab='Index (red)', col="red")
abline(h = Index.Stabw.Adjusted.down, lty =2, col= "blue")
abline(h = Index.Stabw.Adjusted.up, lty =2, col= "blue")
abline(h = Index.mean.Adjusted, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

#Aktie Adjusted und Index Adjusted
plot(Aktie.Adjusted, xlab="Zeitraum", ylab="Aktie (green)/(blue)", col="green")
abline(h = Aktie.Stabw.Adjusted.down, lty =2, col= "blue")
abline(h = Aktie.Stabw.Adjusted.up, lty =2, col= "blue")
abline(h = Aktie.mean.Adjusted, col= "blue")
par(new=TRUE)
plot(Index.Adjusted, xlab=NA, ylab=NA, col="red",axes = F)
abline(h = Index.Stabw.Adjusted.down, lty =2, col= "black")
abline(h = Index.Stabw.Adjusted.up, lty =2, col= "black")
abline(h = Index.mean.Adjusted, col= "black")
axis(side = 4)
mtext(side=4,'Index (red)/(black)')
legend("topleft", legend = c("EV Index", "SD Index","EV Aktie", "SD Aktie"), col = c("black","black","blue","blue"), lty = 1:2, cex = 0.6)

#Aktie Adjusted und Index Adjusted ohne Erw. STAB
plot(Aktie.Adjusted, xlab="Zeitraum", ylab="Aktie (green)", col="green")
par(new=TRUE)
plot(Index.Adjusted, xlab=NA, ylab=NA, col="red",axes = F)
axis(side = 4)
mtext(side=4,'Index (red)')

x = which.max(Aktie.volume)
Aktie.volume[x,]

reg_AR = lm(AR$abnormalReturn~AR$Date)
plot(AR$Date,AR$abnormalReturn)
abline (h=0, col="red")
abline (reg_AR, col="blue")

reg_AV = lm(AV$abnormalReturn~AV$Date)
plot(AV$Date,AV$abnormalReturn)
abline (h=0, col="red")
abline (reg_AV, col="blue")

