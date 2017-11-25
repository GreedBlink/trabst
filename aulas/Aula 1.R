#Séries temporais

#Passeio aleatório

t=100
x=e=NULL

for(i in 1:t){
  e[i]=rnorm(1,0,sqrt(1/2))
  x[i]=sum(e[1:i])
}

#Fazendo vários passeios aleatórios para encontrar a média de x[t]

t=100;n=2000

e=x=matrix(0,t,n)
for(j in 1:n){
  for(i in 1:t){
    e[i,j]=rnorm(1,0,sqrt(1/2))
    x[i,j]=sum(e[1:i,j])
  }
}

(med=apply(x,1,mean)) #Por linha pois queremos a média para cada x[t]
(variancia=apply(x,1,var)) #Por linha pois queremos a média para cada x[t]

cov(x[100,],x[50,]) #t=100, s=50. Cov=min(t,s)*sig_e^2 
cor(x[100,],x[50,]) #t=100, s=50. Cor=sqrt(min(t,s)/max(t,s))

ts.plot(med);abline(h=0,col=3)

plot(variancia,(1:100)*1/2) ; abline(a=0,b=1,col=3)

ts.plot(x[,4]) #No "Ano"
ts.plot(x[4,]) #No "Mês" em diferentes "anos"

plot(x,type="l",xlab="Tempo",ylab="Série",main="Passeio aleatório",cex.lab=1.4,
     cex.main=1.4,cex.axis=1.4,lwd=2)
plot(e,type="l") 

#Médias móveis

e0=rnorm(1,0,sqrt(1/2))
e=rnorm(1,0,sqrt(1/2))
x=(e0+e)/2

for(i in 2:t){
  e[i]=rnorm(1,0,sqrt(1/2))
  x[i]=(e[i]+x[i-1])/2
}
plot(x,type="l",main="Médias móveis")  
plot(x,type="o",pch=19,main="Médias móveis")

#Fazendo replicações para estimar média, var, cov e cor


t=100;r=2000
e=x=matrix(NA,t,r)

for(j in 1:r){
  e0=rnorm(1,0,sqrt(1/2))
  
  e[1,j]=rnorm(1,0,sqrt(1/2))
  x[1,j]=(e0+e[1,j])/2
  for(i in 2:t){
    e[i,j]=rnorm(1,0,sqrt(1/2))
    x[i,j]=(e[i,j]+e[i-1,j])/2
  }
}

(med=apply(x,1,mean)) #Por linha pois queremos a média para cada x[t]
(variancia=apply(x,1,var)) #Por linha pois queremos a média para cada x[t]


ts.plot(med);abline(h=0,col=3)
plot(variancia) ; abline(h=1/4,col=3)

cov(x[39,],x[40,]) #Cov=1/8,se t e s são consecutivos
cor(x[39,],x[40,]) # Cor=1/2 se t e s são consecutivos
