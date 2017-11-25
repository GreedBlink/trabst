install.packages("forecast")
install.packages("TSA")

library(forecast)
library(TSA)

#ARMA(1,1)
N       = 1000
x       = NULL
x[1]    = 2
sigma2e = 2
e       = rnorm(N,0,sqrt(sigma2e))

phi     = 0.4
theta   = 0.6

#phi     = 0.8
#theta   = 0.4

for(t in 2:N){
  x[t]  = phi*x[t-1] + e[t] -theta*e[t-1]
}

#calculando a correlacao teorica
rho     = NULL
for(k in 1:30){
  rho[k] = (1-theta*phi)*(phi-theta)/(1-2*theta*phi+theta^2)*(phi^(k-1))
}

#calculando a correlacao amostral
fig=acf(x,plot=F)
cbind(fig$acf[1:5], rho[1:5])
boxplot(fig$acf - rho)

#plotando a autocorrelacao e a autocorrelacao parcial
par(mfrow=c(1,2))
plot(fig,ylim=c(-1,1), main="")
points(rho,col="red")
pacf(x, main="",ylim=c(-1,1))

#analisando a autocorrelacao extendida
eacf(x, ar.max = 7, ma.max = 13)

#testando a estacionariedade 
#com o teste de Dickey-Fuller
adf.test(x) 

#estimando as ordens p e q
auto.arima(x)


#############################################
# ARIMA(1,1,1)
#############################################

N       = 1000
x       = NULL
x[1]    = 2
sigma2e = 2
e       = rnorm(N,0,sqrt(sigma2e))

phi     = 0.4
theta   = 0.6

for(t in 2:N){
  x[t]  = phi*x[t-1] + e[t] -theta*e[t-1]
}

t = seq(1,N,by=1)
w = x + 0.2*t

par(mfrow=c(1,2))
plot(x,type="l")
plot(w,type="l")

#plotando a autocorrelacao e a autocorrelacao parcial
par(mfrow=c(2,2))
fig=acf(w,plot=F)
plot(fig,ylim=c(-1,1), main="")
pacf(w, main="",ylim=c(-1,1))
fig=acf(x,plot=F)
plot(fig,ylim=c(-1,1), main="")
pacf(x, main="",ylim=c(-1,1))

#analisando a autocorrelacao extendida
eacf(x, ar.max = 7, ma.max = 13)

#testando a estacionariedade 
#com o teste de Dickey-Fuller
adf.test(w) 
adf.test(diff(w)) 
adf.test(x) 

#estimando as ordens p e q
auto.arima(w)
auto.arima(diff(x))
auto.arima(x)

