require(BETS)
require(forecast)
require(TSA)
require(dygraphs)
#selecionando a serie temporal - PIM producao industrial -PF
# índice de base fixa sem ajuste sazonal com base em 2012 = 100
# Série de produção industrial do Brasil, divulgada  pelo IBGE

sidra=BETS.sidra.get(x = c(3653), from = c("200201"), to = c("201710"), territory = "brazil",
                     variable = 3135, sections = c(129316), cl = 544)
st = sidra$serie_3653
#transformando em um objeto ts
st = ts(st$Valor,start = c(2002,01),frequency = 12)


# analise grafica
#plot normal
plot(st)

abline(h = seq(70,110,5), v = seq(2003,2017,1), lty = 3, col = "darkgrey")
#utilizando o dygraph
dygraph(st)
#utiliazndo o monthplot
monthplot(st)


#MA -> ACF
#AR -> PACF

acf(st,lag.max = 48)
#Demora muito a cair, provavelmete nao estacionaria. MAs teremos certeza disso com o teste 
#raiz unitaria
pacf(st,lag.max = 48)

# com difereicniacao


acf(diff(st),lag.max = 48)
#ARIMA(3,1,1)
pacf(diff(st),lag.max = 48)

#parte sazonal
pacf(diff(diff(st,lag = 12)), 48)
acf(diff(diff(st,lag=12)),48)

#(0,1,0)
modelo = Arima(st,order = c(3,1,3), seasonal =  c(1,1,2))
