#Lukas Marx, 11.09.2020 Fürth, Friedrich-Alexander-Universität Erlangen-Nuernberg
#Lukas.Marx@Fau.de
library(tseries)
library(zoo)

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
  
  if (estimationWindowLength < 5) {
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


Start = "2017-01-01"
Ende = "2020-10-03"

Health.Start= "2018-03-16"
Energy.Start = "2020-09-28"

Aktie1="SIE.DE"
Aktie2="SHL.DE"
Aktie3="ENR.F"

Aktie.Volume1 = get.hist.quote(instrument = Aktie1, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)
Aktie.Volume1 = na.omit(Aktie.Volume1)

Aktie.Volume2 = get.hist.quote(instrument = Aktie2, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)
Aktie.Volume2 = na.omit(Aktie.Volume2)

Aktie.Volume3 = get.hist.quote(instrument = Aktie3, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)
Aktie.Volume3 = na.omit(Aktie.Volume3)

Aktie.Adjusted1 = get.hist.quote(instrument = Aktie1, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)
Aktie.Adjusted1 = na.omit(Aktie.Adjusted1)

Aktie.Adjusted2 = get.hist.quote(instrument = Aktie2, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)
Aktie.Adjusted2 = na.omit(Aktie.Adjusted2)

Aktie.Adjusted3 = get.hist.quote(instrument = Aktie3, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)
Aktie.Adjusted3 = na.omit(Aktie.Adjusted3)

PF.Adjusted = cbind(Aktie.Adjusted1, Aktie.Adjusted2, Aktie.Adjusted3)
PF.Volume=  Aktie.Volume1 + Aktie.Volume2 + Aktie.Volume3
PF.Adjusted1 = Aktie.Adjusted1 + Aktie.Adjusted2
PF.Adjusted2 = PF.Adjusted1 + Aktie.Adjusted3
PF.Adjusted0 = get.hist.quote(instrument = Aktie1, 
                              start = Start, end = Health.Start,
                              quote = "Adjusted", provider = "yahoo",
                              compression = "d"
)
PF.Volume0 = get.hist.quote(instrument = Aktie1, 
                              start = Start, end = Health.Start,
                              quote = "Volume", provider = "yahoo",
                              compression = "d"
)
PF.Volume1 = Aktie.Volume1 + Aktie.Volume2
PF.Volume2 = PF.Volume1 + Aktie.Volume3

