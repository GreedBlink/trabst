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

# O que da pra notar:






#modelo de holt-winters



#Modelo de Box & Jenkins



#Estatísticas de Aderência ???



