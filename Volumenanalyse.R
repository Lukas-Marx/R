#Lukas Marx, 27.08.2020 Fürth, Friedrich-Alexander-Universität Erlangen-Nuernberg
#Lukas.Marx@Fau.de
library(tseries)
library(zoo)

Start = "2019-03-11"
Ende = "2020-09-12"

Subsequence1.Start= "2020-03-11"
Subsequence1.Ende = "2020-09-12"

Subsequence2.Start = "2019-09-01"
Subsequence2.Ende ="2020-03-10"

Subsequence3.Start = "2019-03-11"
Subsequence3.Ende = "2019-09-12"


Volume = list()
for(ticker in c("DPW.DE", "VOW3.DE","BMW.DE","DB1.DE","TKA.DE","DBK.DE","ALV.DE",
                "DAI.DE","LHA.DE","HEN3.DE","BEI.DE","BAYN.DE","BAS.DE","HEI.DE",
                "MUV2.DE","EOAN.DE","SIE.DE","SAP.DE","VNA.DE","RWE.DE","FRE.DE",
                "IFX.DE","MRK.DE","FME.DE","LIN.DE","CON.DE","1COV.DE","ADS.DE",
                "DTE.DE","DHER.DE")){
  Volume[[ticker]] = get.hist.quote(instrument = ticker, 
                          start = Start, end = Ende,
                          quote = "Volume", provider = "yahoo",
                          compression = "d",
  )
}

Sum.Volume = Volume$DPW.DE + Volume$VOW3.DE + Volume$BMW.DE + Volume$DB1.DE + Volume$TKA.DE +
                 Volume$DBK.DE + Volume$ALV.DE + Volume$DAI.DE + Volume$LHA.DE + Volume$HEN3.DE +
                 Volume$BEI.DE + Volume$BAYN.DE + Volume$BAS.DE + Volume$HEI.DE + Volume$MUV2.DE +
                 Volume$EOAN.DE + Volume$SIE.DE + Volume$SAP.DE + Volume$CON.DE + Volume$"1COV.DE" + Volume$ADS.DE +
                 Volume$DTE.DE + Volume$DHER.DE
Trading.Volumen.DAX30=na.omit(Sum.Volume)


Subsequence1 = window(Trading.Volumen.DAX30, start = as.Date(Subsequence1.Start), end = as.Date(Subsequence1.Ende))

Subsequence2 = window(Trading.Volumen.DAX30, start = as.Date(Subsequence2.Start), end = as.Date(Subsequence2.Ende))

Subsequence3 = window(Trading.Volumen.DAX30, start = as.Date(Subsequence3.Start), end = as.Date(Subsequence3.Ende))

mean.s1=mean(Subsequence1)
mean.s2=mean(Subsequence2)
mean.s3=mean(Subsequence3)

plot(Trading.Volumen.DAX30, xlab = NA)
par(new=TRUE)
abline(v = as.Date(Subsequence1.Start), col= "red")
abline(v = as.Date(Subsequence2.Start), col= "yellow")
abline(h = mean.s1, col= "blue")
abline(h = mean.s2, col= "green")
abline(h = mean.s3, col= "purple")
legend("topleft",legend = c("2019-09-11", "2020-03-11","MW Abschnitt 1", "MW Abschnitt 2","MW Abschnitt 3"), col = c("yellow","red","purple","green","blue"),lty = 1,cex = 0.7)

plot(Subsequence1, xlab = NA, ylab = "Teilabschnitt 3")
abline(h = mean.s1, col= "blue")
abline(h = mean.s2, col= "green")
abline(h = mean.s3, col= "purple")
legend("topright",legend = c("MW Abschnitt 1", "MW Abschnitt 2","MW Abschnitt 3"), col = c("purple","green","blue"),lty = 1,cex = 0.7)