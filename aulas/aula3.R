#install.packages("readxl")
require(readxl)
dados = read_excel("poluicao.xls")

#instalando pacotes necessarios
install.packages("forcast")
install.packages("smooth")
install.packages("Mcomp")

#carregando
require(forecast)
require( )
require(Mcomp)

#Criar x1 como a suavizacao pelas medias moveis simples e 
# x2 como a suavizacao pela exponencial simples

x = dados$co[1:120]

r = 2

for(r in 1:120){
  x1[r-1] = NA
  for(i in r:120-r){
    print(i-r+1)
    print(c(i,r))
    x1[i] = sum(x[i:(i-r+1)])/r
  }
}
#medias moveis
x1=NULL

x1[1] = NA
for(i in 2:120){
  x1[i] = sum(x[i:(i-r+1)])/r
}

# Pode ser calculado da seguinte forma

mms = NULL
n=120
for(i in r:n){
  mms[i] = mean(x[(i-r+1):i])  
}

# nessa terceira forma
mms2 = filter(x,rep(1/r,r),sides=1)

#sides = 1 significa so pega os valores passados

#mms==x1
#exponencial
x2 = x[1]
alfa = 0.7
for(i in 2:120){
  x2[i] = alfa*x[i]+(1-alfa)*x2[i-1]
}


#ou do jeito 
x2aux=NULL
x2aux[1] = x[1]
x2aux[2] = alfa*x[2]+(1-alfa)*x[1]

for(i in 3:120){
  x2aux[i] = alfa*x[i-1]+(1-alfa)*x2aux[i-2]  
}

#dessa terceira forma
k = ts(x)
m = HoltWinters(k,alpha = alfa,beta=F,gamma=F,start.periods = 3)

plot(x,type ="l")
lines(m$fitted[,1],col=3)
lines(x2aux,col=6)
lines(x2,col=2)

plot(x-x2);abline(h=0,col=2)  
  
##################

plot(x,type = "l")
lines(mms2,col=2)
lines(filter(x,rep(1/7,7),side=1),col=3)
