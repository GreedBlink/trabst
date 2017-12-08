#Carregando os pacotes e os dados
#metodogologia Box & Jenkins
require(BETS)
require(forecast)
require(normtest)
require(TSA)
require(dygraphs)
require(tidyr)
dados <-  BETS.sidra.get(x = c(3653), from = c("200201"), to = c("201710"), territory = "brazil",variable = 3135, sections = c(129316), cl = 544)
dados <- ts(dados$serie_3653$Valor,start = c(2002,01),frequency = 12)
dados <- window(dados,start = c(2002,01),end =c(2017,09),freq=12)

#####Serie
dygraph(dados) %>% dyRangeSelector()


modelo_auto.arima <- auto.arima(dados);modelo_auto.arima

acf(dados,lag.max = 48)

adf.test(dados)

acf(dados,lag.max = 48)


adf.test(dados)


dados_diff <- diff(dados)
acf(dados_diff,lag.max = 48)

adf.test(dados_diff)

dados_diff_seasonal = diff(dados_diff,lag=12)
acf(dados_diff_seasonal,lag=48)

pacf(dados_diff_seasonal,lag=48)


modelo = Arima(dados,order= c(3,1,2),seasonal = c(1,1,1),lambda = 0)
modelo$aic

BETS.t_test(modelo)

modelo = Arima(dados,order= c(3,1,2),seasonal = c(0,1,1),lambda = 0)
modelo$aic

BETS.t_test(modelo)

tsdiag(modelo)

shapiro.test(modelo$residuals)

x = (modelo$residuals - mean(modelo$residuals))/sd(modelo$residuals)
round(x)


#modelo com dummy
dummy <-  BETS.dummy(start = c(2002,01),end = c(2017,09),month =12 ,year = 2008 ,frequency = 12)
dummy

modelo2 <- Arima(dados,order= c(3,1,2),seasonal = c(0,1,1),lambda = 0,xreg=dummy)
modelo2$aic

tsdiag(modelo2)

shapiro.test(modelo2$residuals)

BETS.t_test(modelo2)


modelo3 <- Arima(dados,order = c(2,1,2),seasonal = c(0,1,1),lambda = 0,xreg=dummy)
modelo3$aic

tsdiag(modelo3)

shapiro.test(modelo3$residuals)

modelo2_auto.arima <-  auto.arima(dados,xreg = dummy,lambda = 0)
modelo2_auto.arima$aic

#######Previsao

funcao <- function(h,dados,order,seasonal,lambda){
  #definições premiliminares
  previsoes = c()
  n <- length(dados) 
  i = h
  
  # esse for é para a previsão mês a mês
  for(i in h:1){
    y <- ts(dados[1:(n-i)],start=start(dados),freq=12)
    y_dummy <- ts(dummy[1:(n-i)],start=start(dados),freq=12)
    y_dummy_prev = tail(ts(dummy[1:(n-i+1)],start=start(dados),freq=12),1)
    # para a previsão, precisamos reestimar o modelo, nesse caso, 
    # mês a mês.
    aux2 <- Arima(y,order = order,seasonal = seasonal,lambda = lambda,xreg=y_dummy)
    previsoes[i] <- forecast(aux2,h=1,xreg = y_dummy_prev)$mean
  }
  # daqui para frente, temos a previsão para os meses de um vez só!
  previsoes = ts(previsoes[h:1],end = end(dados),freq = 12)
  
  y <- ts(dados[1:(n-h)],start=start(dados),freq=12)
  y_dummy2 = ts(dummy[1:(n-h)],start=start(dados),freq=12)
  # para a previsão, precisamos reestimar o modelo
  aux1 <- Arima(y,order = order,seasonal = seasonal,lambda = lambda,xreg=y_dummy2)
  
  y_dummy_prev2 <- ts(dummy[(n-h+1):n],end=end(dados),freq=12)
  previsao2 <- forecast(aux1,h=h,xreg = y_dummy_prev2)$mean
  
  ##comparando as previsões junto com os dados originais
  ts.plot(previsoes,previsao2,col=c(1,4),lty = c(1,2))
  lines(dados,col=2,lty = c(3))
  legend("topleft",legend = c("Dados","prev. mes a mes", "previsao normal"),lty = c(1,2,3),col = c(1,2,4),bty="n")
  
  return(list(prev1 = previsoes, prev2 = previsao2))
}




order = c(2,1,2)
seasonal = c(0,1,1)
h=9
prev <- funcao(h=h,dados=dados,order=order,seasonal = seasonal,lambda = 0)




comparacao <-  window(dados, start = c(2017,01),freq=12)
comparacao
prev$prev1
prev$prev2

###Erro médio quadrático
(mean((comparacao - prev$prev1)^2))
(mean((comparacao - prev$prev2)^2))


#Erro absoluto percentual médio
mean(abs(comparacao - prev$prev1)/comparacao)*100
mean(abs(comparacao - prev$prev2)/comparacao)*100