# Aula de séries - 17 / 11

######################################################
##EXERCICIOS##
######################################################

#1) Simule um modelo ARMA (1,1) com n=1000 , phi=0.4 , theta=0.6

n <- 1000
e <- rnorm(n)
phi <- 0.4
theta <- 0.6

y <- NULL
y[1] <- 1

for(t in 2:1000){
  y[t] <- phi * y[t-1] +  e[t] - theta * e[t-1]  
}

plot(y, ylab = eypression(y[t]), ylab = "Instantes", type = "o")


#   a)Calcule e desenhe a funcao de autocorrelacao teorica

act=NULL
for(k in 1:n){
  act[k]=((1-theta*phi)*(phi-theta)*phi^(k-1))/(1-2*theta*phi+theta^2)
}

#   b)Desenhe a funcao de autocorrelacao amostral. Adicione 
#     em vermelho nesse mesmo grafico a autocorrelacao teorica.

acf(y, plot=T, type = "correlation")
points(act,col="red")
points(0,1,col="red")

#   c) Faca a autocorrelacao estendida. Essa funcao ajudou a encontrar as ordens
#      p e q do modelo?

# install.packages("TSA")
require(TSA)

# funcao de autocorrelacao eytendida
eacf(y)
# (quase) nunca ajuda a encontrar as ordens p e q do modelo


#   d)Use o teste de Dickey-Fuller para testar a estacionariedade dos dados.

#H0: Dados não estacionários
#H1:  Dados estacionários

adf.test(y)

#Dados estacionários

# e) Use a funcao auto.arima e diga se essa ajudou a encontrar as ordens p e q. (l

#install.packages("forecast")
require(forecast)

auto.arima(y)
# Nem sempre dá certo. Deveria ser ARIMA(1,0,1).
# ar1 = phi, ar2 = theta

#2) Repita os itens anteriores para outros dados:

#   i)  Dado simulado: ARMA (1,1) com phi=0.8 e theta=0.4

n <- 1000
e <- rnorm(n)
phi <- 0.8
theta <- 0.4

y <- NULL
y[1] <- 1

for(t in 2:1000){
  y[t] <- phi * y[t-1] +  e[t] - theta * e[t-1]  
}

#   a)Calcule e desenhe a funcao de autocorrelacao teorica

act=NULL
for(k in 1:n){
  act[k]=((1-theta*phi)*(phi-theta)*phi^(k-1))/(1-2*theta*phi+theta^2)
}

#   b)Desenhe a funcao de autocorrelacao amostral. Adicione 
#     em vermelho nesse mesmo grafico a autocorrelacao teorica.

acf(y, plot=T, type = "correlation")
points(act,col="red")
points(0,1,col="red")

#   c) Faca a autocorrelacao estendida. Essa funcao ajudou a encontrar as ordens
#      p e q do modelo?

# install.packages("TSA")
require(TSA)

# funcao de autocorrelacao eytendida
eacf(y)
# (quase) nunca ajuda a encontrar as ordens p e q do modelo


#   d)Use o teste de Dickey-Fuller para testar a estacionariedade dos dados.

#H0: Dados não estacionários
#H1:  Dados estacionários

adf.test(y)

#Dados estacionários

# e) Use a funcao auto.arima e diga se essa ajudou a encontrar as ordens p e q. (l

#install.packages("forecast")
require(forecast)

auto.arima(y) #Ar1 próximo de theta

#   ii) Faca Wt=Yt+t

w <- NULL

for(t in 1:n){
  w[t] <- y[t] + t
}

act=NULL
for(k in 1:n){
  act[k]=((1-theta*phi)*(phi-theta)*phi^(k-1))/(1-2*theta*phi+theta^2)
}

#   b)Desenhe a funcao de autocorrelacao amostral. Adicione 
#     em vermelho nesse mesmo grafico a autocorrelacao teorica.

acf(w, plot=T, type = "correlation")
points(act,col="red")
points(0,1,col="red")

#   c) Faca a autocorrelacao estendida. Essa funcao ajudou a encontrar as ordens
#      p e q do modelo?

# install.packages("TSA")
require(TSA)

# funcao de autocorrelacao ewtendida
eacf(w)
# (quase) nunca ajuda a encontrar as ordens p e q do modelo

#   d)Use o teste de Dickew-Fuller para testar a estacionariedade dos dados.

#H0: Dados não estacionários
#H1:  Dados estacionários

adf.test(w)

#Dados estacionários

# e) Use a funcao auto.arima e diga se essa ajudou a encontrar as ordens p e q. (l

#install.packages("forecast")
require(forecast)

auto.arima(w)

#  iii) data(robot)  ~> library(TSA)

data(robot)

acf(robot, plot=T, type = "correlation")

#   c) Faca a autocorrelacao estendida. Essa funcao ajudou a encontrar as ordens
#      p e q do modelo?

# install.packages("TSA")
require(TSA)

# funcao de autocorrelacao erobottendida
eacf(robot)
# (quase) nunca ajuda a encontrar as ordens p e q do modelo

#   d)Use o teste de Dickerobot-Fuller para testar a estacionariedade dos dados.

#H0: Dados não estacionários
#H1:  Dados estacionários

adf.test(robot)

#Dados estacionários

# e) Use a funcao auto.arima e diga se essa ajudou a encontrar as ordens p e q. (l

#install.packages("forecast")
require(forecast)

auto.arima(robot)
