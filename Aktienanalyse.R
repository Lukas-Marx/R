#Lukas Marx, 27.08.2020 Fürth, Friedrich-Alexander-Universität Erlangen-Nuernberg
#Lukas.Marx@Fau.de
library(tseries)
library(zoo)
Aktie = "DHER.DE"
Vergleichsindex = "^GDAXI"

Start = "2018-08-23"
Ende = "2020-10-09"

Subsequence1.Start = "2020-08-24"
Subsequence1.Ende = "2020-10-09"


Subsequence2.Start = "2020-07-08"
Subsequence2.Ende = "2020-08-24"



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
Teilzeitreihe.Volume = window(Aktie.volume, start = as.Date(Subsequence1.Start), end = as.Date(Subsequence1.Ende))
#Subsequence Adjusted
Teilzeitreihe.Adjusted = window(Aktie.Adjusted, start = as.Date(Subsequence1.Start), end = as.Date(Subsequence1.Ende))               

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

#Subsequence Volume
Teilzeitreihe2.Volume = window(Aktie.volume, start = as.Date(Subsequence2.Start), end = as.Date(Subsequence2.Ende))
#Subsequence Adjusted
Teilzeitreihe2.Adjusted = window(Aktie.Adjusted, start = as.Date(Subsequence2.Start), end = as.Date(Subsequence2.Ende))               

#Aktie Volumen Subsequence
plot(Teilzeitreihe2.Volume, xlab="Zeitraum", ylab='Volume (purple)', col="purple")
abline(h = Aktie.Stabw.volume.down, lty =2, col= "blue")
abline(h = Aktie.Stabw.volume.up, lty =2, col= "blue")
abline(h = Aktie.mean.volume, col= "blue")
legend("topleft", legend = c("EV", "SD"), col = c("blue","blue"), lty = 1:2,cex = 0.6)

#Aktie Adjusted Subsequence
plot(Teilzeitreihe.Adjusted2, xlab="Zeitraum", ylab='Aktie (green)', col="green")
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



