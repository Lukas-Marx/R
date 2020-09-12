#Lukas Marx, 11.09.2020 Fürth, Friedrich-Alexander-Universität Erlangen-Nuernberg
#Lukas.Marx@Fau.de
library(tseries)
library(zoo)

Start = "2020-07-30"
Ende = "2020-09-30"

Subsequence1.Start= "2020-07-30"
Subsequence1.Ende = "2020-08-31"

Subsequence2.Start = "2019-09-01"
Subsequence2.Ende ="2020-09-30"

Aktie1 = "AAPL"
Aktie2 = "TSLA"

Aktie.volume1 = get.hist.quote(instrument = Aktie1, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)

Aktie.volume2 = get.hist.quote(instrument = Aktie2, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)

Aktie.Adjusted1 = get.hist.quote(instrument = Aktie1, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)

Aktie.Adjusted2 = get.hist.quote(instrument = Aktie2, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)


Aktie.volume1sub1 = get.hist.quote(instrument = Aktie1, 
                               start = Subsequence1.Start, end = Subsequence1.Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)

Aktie.volume2sub1 = get.hist.quote(instrument = Aktie2, 
                               start = Subsequence1.Start, end = Subsequence1.Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)

Aktie.Adjusted1sub1 = get.hist.quote(instrument = Aktie1, 
                                 start = Subsequence1.Start, end = Subsequence1.Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)

Aktie.Adjusted2sub1 = get.hist.quote(instrument = Aktie2, 
                                 start = Subsequence1.Start, end = Subsequence1.Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)

Aktie.volume1sub2 = get.hist.quote(instrument = Aktie1, 
                                   start = Subsequence2.Start, end = Subsequence2.Ende,
                                   quote = "Volume", provider = "yahoo",
                                   compression = "d"
)

Aktie.volume2sub2 = get.hist.quote(instrument = Aktie2, 
                                   start = Subsequence2.Start, end = Subsequence2.Ende,
                                   quote = "Volume", provider = "yahoo",
                                   compression = "d"
)

Aktie.Adjusted1sub2 = get.hist.quote(instrument = Aktie1, 
                                     start = Subsequence2.Start, end = Subsequence2.Ende,
                                     quote = "Adjusted", provider = "yahoo",
                                     compression = "d"
)

Aktie.Adjusted2sub2 = get.hist.quote(instrument = Aktie2, 
                                     start = Subsequence2.Start, end = Subsequence2.Ende,
                                     quote = "Adjusted", provider = "yahoo",
                                     compression = "d"
)

ERW_AAPL_whole_vol = mean (Aktie.volume1)
ERW_TSLA_whole_vol = mean(Aktie.volume2)
Var_AAPL_single_vol = (Aktie.volume1 - ERW_AAPL_whole_vol)^2
Var_AAPL_whole_vol = sum(Var_AAPL_single_vol) / length(Var_AAPL_single_vol)
StabW_AAPL_whole_vol = sqrt(Var_AAPL_whole_vol)
Stabw_AAPL_whole_vol_up = ERW_AAPL_whole_vol + StabW_AAPL_whole_vol
Stabw_AAPL_whole_vol_down = ERW_AAPL_whole_vol - StabW_AAPL_whole_vol
Var_TSLA_single_vol = (Aktie.volume2 - ERW_TSLA_whole_vol)^2
Var_TSLA_whole_vol = sum(Var_TSLA_single_vol) / length(Var_TSLA_single_vol)
StabW_TSLA_whole_vol = sqrt(Var_TSLA_whole_vol)
Stabw_TSLA_whole_vol_up = ERW_TSLA_whole_vol + StabW_TSLA_whole_vol
Stabw_TSLA_whole_vol_down = ERW_TSLA_whole_vol - StabW_TSLA_whole_vol

ERW_AAPL_whole_vol_sub1 = mean (Aktie.volume1sub1)
ERW_TSLA_whole_vol_sub1 = mean(Aktie.volume2sub1)
Var_AAPL_single_vol_sub1 = (Aktie.volume1sub1 - ERW_AAPL_whole_vol_sub1)^2
Var_AAPL_whole_vol_sub1 = sum(Var_AAPL_single_vol_sub1) / length(Var_AAPL_single_vol_sub1)
StabW_AAPL_whole_vol_sub1 = sqrt(Var_AAPL_whole_vol_sub1)
Stabw_AAPL_whole_vol_sub1_up = ERW_AAPL_whole_vol_sub1 + StabW_AAPL_whole_vol_sub1
Stabw_AAPL_whole_vol_sub1_down = ERW_AAPL_whole_vol_sub1 - StabW_AAPL_whole_vol_sub1
Var_TSLA_single_vol_sub1 = (Aktie.volume2sub1 - ERW_TSLA_whole_vol_sub1)^2
Var_TSLA_whole_vol_sub1 = sum(Var_TSLA_single_vol_sub1) / length(Var_TSLA_single_vol_sub1)
StabW_TSLA_whole_vol_sub1 = sqrt(Var_TSLA_whole_vol_sub1)
Stabw_TSLA_whole_vol_sub1_up = ERW_TSLA_whole_vol_sub1 + StabW_TSLA_whole_vol_sub1
Stabw_TSLA_whole_vol_sub1_down = ERW_TSLA_whole_vol_sub1 - StabW_TSLA_whole_vol_sub1

ERW_AAPL_whole_vol_sub2 = mean (Aktie.volume1sub2)
ERW_TSLA_whole_vol_sub2 = mean(Aktie.volume2sub2)
Var_AAPL_single_vol_sub2 = (Aktie.volume1sub2 - ERW_AAPL_whole_vol_sub2)^2
Var_AAPL_whole_vol_sub2 = sum(Var_AAPL_single_vol_sub2) / length(Var_AAPL_single_vol_sub2)
StabW_AAPL_whole_vol_sub2 = sqrt(Var_AAPL_whole_vol_sub2)
Stabw_AAPL_whole_vol_sub2_up = ERW_AAPL_whole_vol_sub2 + StabW_AAPL_whole_vol_sub2
Stabw_AAPL_whole_vol_sub2_down = ERW_AAPL_whole_vol_sub2 - StabW_AAPL_whole_vol_sub2
Var_TSLA_single_vol_sub2 = (Aktie.volume2sub2 - ERW_TSLA_whole_vol_sub2)^2
Var_TSLA_whole_vol_sub2 = sum(Var_TSLA_single_vol_sub2) / length(Var_TSLA_single_vol_sub2)
StabW_TSLA_whole_vol_sub2 = sqrt(Var_TSLA_whole_vol_sub2)
Stabw_TSLA_whole_vol_sub2_up = ERW_TSLA_whole_vol_sub2 + StabW_TSLA_whole_vol_sub2
Stabw_TSLA_whole_vol_sub2_down = ERW_TSLA_whole_vol_sub2 - StabW_TSLA_whole_vol_sub2


plot(Aktie.volume1, xlab="gesamter Zeitraum", ylab='Volume AAPL (purple)', col="purple")
abline(h = Stabw_AAPL_whole_vol_up, lty =2, col= "blue")
abline(h = Stabw_AAPL_whole_vol_down, lty =2, col= "blue")
abline(h = ERW_AAPL_whole_vol, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

plot(Aktie.volume2, xlab="gesamter Zeitraum", ylab='Volume TSLA (purple)', col="purple")
abline(h = Stabw_TSLA_whole_vol_up, lty =2, col= "blue")
abline(h = Stabw_TSLA_whole_vol_down, lty =2, col= "blue")
abline(h = ERW_TSLA_whole_vol, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

plot(Aktie.volume1, xlab="gesamter Zeitraum", ylab='Volume AAPL (purple)', col="purple")
abline(h = Stabw_AAPL_whole_vol_sub1_up, lty =2, col= "blue")
abline(h = Stabw_AAPL_whole_vol_sub1_down, lty =2, col= "blue")
abline(h = ERW_AAPL_whole_vol_sub1, col= "blue")
abline(h = Stabw_AAPL_whole_vol_sub2_up, lty =2, col= "red")
abline(h = Stabw_AAPL_whole_vol_sub2_down, lty =2, col= "red")
abline(h = ERW_AAPL_whole_vol_sub2, col= "red")
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post"), col = c("blue","blue","red","red"), lty = 1:2,cex = 0.6)

plot(Aktie.volume2, xlab="gesamter Zeitraum", ylab='Volume TSLA (purple)', col="purple")
abline(h = Stabw_TSLA_whole_vol_sub1_up, lty =2, col= "blue")
abline(h = Stabw_TSLA_whole_vol_sub1_down, lty =2, col= "blue")
abline(h = ERW_TSLA_whole_vol_sub1, col= "blue")
abline(h = Stabw_TSLA_whole_vol_sub2_up, lty =2, col= "red")
abline(h = Stabw_TSLA_whole_vol_sub2_down, lty =2, col= "red")
abline(h = ERW_TSLA_whole_vol_sub2, col= "red")
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post"), col = c("blue","blue","red","red"), lty = 1:2,cex = 0.6)

ERW_AAPL_whole_adj = mean (Aktie.Adjusted1)
ERW_TSLA_whole_adj = mean(Aktie.Adjusted2)
Var_AAPL_single_adj = (Aktie.Adjusted1 - ERW_AAPL_whole_adj)^2
Var_AAPL_whole_adj = sum(Var_AAPL_single_adj) / length(Var_AAPL_single_adj)
StabW_AAPL_whole_adj = sqrt(Var_AAPL_whole_adj)
Stabw_AAPL_whole_adj_up = ERW_AAPL_whole_adj + StabW_AAPL_whole_adj
Stabw_AAPL_whole_adj_down = ERW_AAPL_whole_adj - StabW_AAPL_whole_adj
Var_TSLA_single_adj = (Aktie.Adjusted2 - ERW_TSLA_whole_adj)^2
Var_TSLA_whole_adj = sum(Var_TSLA_single_adj) / length(Var_TSLA_single_adj)
StabW_TSLA_whole_adj = sqrt(Var_TSLA_whole_adj)
Stabw_TSLA_whole_adj_up = ERW_TSLA_whole_adj + StabW_TSLA_whole_adj
Stabw_TSLA_whole_adj_down = ERW_TSLA_whole_adj - StabW_TSLA_whole_adj

ERW_AAPL_whole_adj_sub1 = mean (Aktie.Adjusted1sub1)
ERW_TSLA_whole_adj_sub1 = mean(Aktie.Adjusted2sub1)
Var_AAPL_single_adj_sub1 = (Aktie.Adjusted1sub1 - ERW_AAPL_whole_adj_sub1)^2
Var_AAPL_whole_adj_sub1 = sum(Var_AAPL_single_adj_sub1) / length(Var_AAPL_single_adj_sub1)
StabW_AAPL_whole_adj_sub1 = sqrt(Var_AAPL_whole_adj_sub1)
Stabw_AAPL_whole_adj_sub1_up = ERW_AAPL_whole_adj_sub1 + StabW_AAPL_whole_adj_sub1
Stabw_AAPL_whole_adj_sub1_down = ERW_AAPL_whole_adj_sub1 - StabW_AAPL_whole_adj_sub1
Var_TSLA_single_adj_sub1 = (Aktie.Adjusted2sub1 - ERW_TSLA_whole_adj_sub1)^2
Var_TSLA_whole_adj_sub1 = sum(Var_TSLA_single_adj_sub1) / length(Var_TSLA_single_adj_sub1)
StabW_TSLA_whole_adj_sub1 = sqrt(Var_TSLA_whole_adj_sub1)
Stabw_TSLA_whole_adj_sub1_up = ERW_TSLA_whole_adj_sub1 + StabW_TSLA_whole_adj_sub1
Stabw_TSLA_whole_adj_sub1_down = ERW_TSLA_whole_adj_sub1 - StabW_TSLA_whole_adj_sub1

ERW_AAPL_whole_adj_sub2 = mean (Aktie.Adjusted1sub2)
ERW_TSLA_whole_adj_sub2 = mean(Aktie.Adjusted2sub2)
Var_AAPL_single_adj_sub2 = (Aktie.Adjusted1sub2 - ERW_AAPL_whole_adj_sub2)^2
Var_AAPL_whole_adj_sub2 = sum(Var_AAPL_single_adj_sub2) / length(Var_AAPL_single_adj_sub2)
StabW_AAPL_whole_adj_sub2 = sqrt(Var_AAPL_whole_adj_sub2)
Stabw_AAPL_whole_adj_sub2_up = ERW_AAPL_whole_adj_sub2 + StabW_AAPL_whole_adj_sub2
Stabw_AAPL_whole_adj_sub2_down = ERW_AAPL_whole_adj_sub2 - StabW_AAPL_whole_adj_sub2
Var_TSLA_single_adj_sub2 = (Aktie.Adjusted2sub2 - ERW_TSLA_whole_adj_sub2)^2
Var_TSLA_whole_adj_sub2 = sum(Var_TSLA_single_adj_sub2) / length(Var_TSLA_single_adj_sub2)
StabW_TSLA_whole_adj_sub2 = sqrt(Var_TSLA_whole_adj_sub2)
Stabw_TSLA_whole_adj_sub2_up = ERW_TSLA_whole_adj_sub2 + StabW_TSLA_whole_adj_sub2
Stabw_TSLA_whole_adj_sub2_down = ERW_TSLA_whole_adj_sub2 - StabW_TSLA_whole_adj_sub2


plot(Aktie.Adjusted1, xlab="gesamter Zeitraum", ylab='Adjusted Stock AAPL (purple)', col="purple")
abline(h = Stabw_AAPL_whole_adj_up, lty =2, col= "blue")
abline(h = Stabw_AAPL_whole_adj_down, lty =2, col= "blue")
abline(h = ERW_AAPL_whole_adj, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

plot(Aktie.Adjusted2, xlab="gesamter Zeitraum", ylab='Adjusted Stock TSLA (purple)', col="purple")
abline(h = Stabw_TSLA_whole_adj_up, lty =2, col= "blue")
abline(h = Stabw_TSLA_whole_adj_down, lty =2, col= "blue")
abline(h = ERW_TSLA_whole_adj, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

plot(Aktie.Adjusted1, xlab="gesamter Zeitraum", ylab='Adjusted Stock AAPL (purple)', col="purple")
abline(h = Stabw_AAPL_whole_adj_sub1_up, lty =2, col= "blue")
abline(h = Stabw_AAPL_whole_adj_sub1_down, lty =2, col= "blue")
abline(h = ERW_AAPL_whole_adj_sub1, col= "blue")
abline(h = Stabw_AAPL_whole_adj_sub2_up, lty =2, col= "red")
abline(h = Stabw_AAPL_whole_adj_sub2_down, lty =2, col= "red")
abline(h = ERW_AAPL_whole_adj_sub2, col= "red")
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post"), col = c("blue","blue","red","red"), lty = 1:2,cex = 0.6)

plot(Aktie.Adjusted2, xlab="gesamter Zeitraum", ylab='Adjusted Stock TSLA (purple)', col="purple")
abline(h = Stabw_TSLA_whole_adj_sub1_up, lty =2, col= "blue")
abline(h = Stabw_TSLA_whole_adj_sub1_down, lty =2, col= "blue")
abline(h = ERW_TSLA_whole_adj_sub1, col= "blue")
abline(h = Stabw_TSLA_whole_adj_sub2_up, lty =2, col= "red")
abline(h = Stabw_TSLA_whole_adj_sub2_down, lty =2, col= "red")
abline(h = ERW_TSLA_whole_adj_sub2, col= "red")
legend("topleft", legend = c("EV_pre", "SD_pre","EV_post","SD_post"), col = c("blue","blue","red","red"), lty = 1:2,cex = 0.6)
