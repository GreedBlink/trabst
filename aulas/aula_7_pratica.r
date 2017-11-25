require(TSA)
require(forecast)

data(robot)
x = as.vector(robot)

data(larain)
x = as.vector(larain)

data(hare)
x = as.vector(hare)

data(oil.price)
x = as.vector(oil.price)
#x = log(x)

par(mfrow=c(1,1))
plot(x,type="l",bty="n",cex.axis=1.4,cex.lab=1.4,xlab="instantes",ylab="x",lwd=1.5)

par(mfrow=c(1,2))
fig=acf(x,plot=F)
plot(fig,ylim=c(-1,1), main="")
pacf(x, main="",ylim=c(-1,1))

adf.test(x) #Teste de Dickey-Fuller
#Não rejeita a hipótese de não estacionariedade
forecast::auto.arima(x) #with drift = com media não nula
#Ou seja,
#W_t = e_t + \phi_1 W_(t-1) - \theta_1 e_(t-2) + \theta_0
#W_t = X_t - X_(t-1)

#phichapeu = 0,4574
#- theta1chapéu = 0,24
#- theta2chapéu = 0,2563
auto.arima(log(x)) #Preferir o com menor AIC

data(color)

#analisando o ajuste

ajuste = auto.arima(color)
ajuste

plot(as.vector(ajuste$residuals),xlab="instantes",ylab="res?duos",bty="n",cex.lab=1.4,cex.axis=1.4)
abline(h=0,lty=3,lwd=2)

qqnorm(ajuste$residuals,bty="n",xlab="quantis te?ricos",ylab="quantis amostrais",lwd=2,cex.lab=1.4,cex.axis=1.4)
qqline(ajuste$residuals,col="red",lwd=2)

fig=acf(ajuste$residual,plot=F)
plot(fig,ylim=c(-1,1),xlab="defasagem",main="ACF dos res?duos",bty="n",cex.lab=1.4,cex.axis=1.4)

tsdiag(ajuste,got=15,omit.initial = F)
#terceiro grafico:
#Ljung-Box teste
#H0 = erros sao nao correlacionados
#H1 = erros sao correlacionados

ks.test(ajuste$residuals,"pnorm",mean(ajuste$residuals),sd(ajuste$residuals))
shapiro.test(ajuste$residuals)


#Fazendo previsao

ajuste=auto.arima(x)
prev=forecast(ajuste,h=5,level=95) #h: instantes para prever
plot(prev,bty="n")
