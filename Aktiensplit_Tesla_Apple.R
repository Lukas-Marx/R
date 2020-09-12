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
