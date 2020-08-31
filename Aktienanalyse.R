#Lukas Marx, 27.08.2020 Fürth, Friedrich-Alexander-Universität Erlangen-Nuernberg
#Lukas.Marx@Fau.de
library(tseries)
Aktie = "DHER.DE"
Vergleichsindex = "^GDAXI"

Start = "2020-08-17"
Ende = "2020-08-31"


Aktie.volume = get.hist.quote(instrument = Aktie, 
                                 start = Start, end = Ende,
                                 quote = "Volume", provider = "yahoo",
                                 compression = "d"
)

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

#Bestimmte Abweichungsfaktor für max
Aktie.Abweichungsfaktor = (Aktie.max.volume - Aktie.Stabw.volume.gesamt)/Aktie.Stabw.volume.gesamt



Aktie.Adjusted = get.hist.quote(instrument = Aktie, 
                              start = Start, end = Ende,
                              quote = "Adjusted", provider = "yahoo",
                              compression = "d"
)

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

#Bestimmte Abweichungsfaktor für max
Aktie.Abweichungsfaktor = (Aktie.max.Adjusted - Aktie.Stabw.Adjusted.gesamt)/Aktie.Stabw.Adjusted.gesamt


Index.Adjusted = get.hist.quote(instrument = Vergleichsindex, 
                              start = Start, end = Ende,
                              quote = "Adjusted", provider = "yahoo",
                              compression = "d"
)

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

#Bestimmte Abweichungsfaktor für max
Index.Abweichungsfaktor = (Index.max.Adjusted - Index.Stabw.Adjusted.gesamt)/Index.Stabw.Adjusted.gesamt