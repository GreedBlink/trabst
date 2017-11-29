#Pacotes necessários
require(BETS)
require(urca)
require(forecast)

dados <-  BETS.sidra.get(x = c(3653), from = c("200201"), to = c("201710"), territory = "brazil",
                         variable = 3135, sections = c(129316), cl = 544)
dados <- ts(dados$serie_3653$Valor,start = c(2002,01),frequency = 12)

#analise exploratoria da serie temporal da producao fisica industrial 

plot.ts(dados)
monthplot(dados)

#tendencia 

#podemos ver um aumento na produção 

#variancia 

#sazonalidade