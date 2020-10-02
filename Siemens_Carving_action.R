#Lukas Marx, 11.09.2020 Fürth, Friedrich-Alexander-Universität Erlangen-Nuernberg
#Lukas.Marx@Fau.de
library(tseries)
library(zoo)

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
Aktie.Volume1 = na.fill(Aktie.Volume1,0)

Aktie.Volume2 = get.hist.quote(instrument = Aktie2, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)
Aktie.Volume2 = na.fill(Aktie.Volume2,0)

Aktie.Volume3 = get.hist.quote(instrument = Aktie3, 
                               start = Start, end = Ende,
                               quote = "Volume", provider = "yahoo",
                               compression = "d"
)
Aktie.Volume3 = na.fill(Aktie.Volume3,0)

Aktie.Adjusted1 = get.hist.quote(instrument = Aktie1, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)
Aktie.Adjusted1 = na.fill(Aktie.Adjusted1,0)

Aktie.Adjusted2 = get.hist.quote(instrument = Aktie2, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)
Aktie.Adjusted2 = na.fill(Aktie.Adjusted2,0)

Aktie.Adjusted3 = get.hist.quote(instrument = Aktie3, 
                                 start = Start, end = Ende,
                                 quote = "Adjusted", provider = "yahoo",
                                 compression = "d"
)
Aktie.Adjusted3 = na.fill(Aktie.Adjusted3,0)

PF.Adjusted = Aktie.Adjusted1 + Aktie.Adjusted2 + Aktie.Adjusted3
PF.Volume=  Aktie.Volume1 + Aktie.Volume2 + Aktie.Volume3
