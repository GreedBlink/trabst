#Aula 4

install.packages('forecast', dependencies = TRUE)
install.packages("smooth")
install.packages("Mcomp")

require(smooth)
require(Mcomp)
install.packages("TSA")
library(TSA)
library("randtests")

########################################
#Medias moveis simples
########################################

pasta=getwd()
setwd(paste(pasta,"/dados",sep=""))

dados = read.table("poluicao.txt",header=T)
dados[1,]

x = dados$co[1:120]
#x = dados$no2[1:120]
x
plot(x,type="l",lwd=2,ylab="CO",xlab="instantes",bty="n" ,cex.lab=1.4,cex.axis=1.4)
xp      = dados$co[121:125]
#xp      = dados$no2[121:125]
xxp     = c(x,xp)
n     = length(x)

#medias moveis simples
r     = 7

MMS   = NULL
for(i in r:n){MMS[i] = mean(x[(i-r+1):i])}

#outra forma
MMS2 = filter(x, rep(1/r, r), sides=1)

plot(x,type="l",lwd=2,ylab="CO",xlab="instantes",bty="n" ,cex.lab=1.4,cex.axis=1.4)
lines(MMS,col="red",lwd=2)
lines(MMS2,col="blue",lwd=2)

#o que acontece com o ajuste quando vario r?
plot(x,type="l",lwd=2,ylab="CO",xlab="instantes",bty="n" ,cex.lab=1.4,cex.axis=1.4)
r2=3
lines(filter(x, rep(1/r2, r2), sides=1),col="blue",lwd=2)
r3=10
lines(filter(x, rep(1/r3, r3), sides=1),col="red",lwd=2)

#analisando a diferenca entre o valor observado e o suavizado
res = x - MMS
(EQM = mean(res^2,na.rm=TRUE)) #erro medio quadratico
(EAM = mean(abs(res),na.rm=TRUE)) #erro medio absoluto

#prevendo h passos a frente
  #X_n(h)=MMS_n para todo h
  MMS[n]
  
  #posso tambem atualizar a previsao a medida que ganho novas observacoes
  MMSp    = MMS
  #previsao no instante n+1
  (MMSp[n+1] = MMS[n])
  #previsao no instante n+2
  #Xchapeu_n+1(1) = Xchapeu_n(1) + (X_n+1 - X_n+1-r)/r
  i = n+2
  (MMSp[i] = MMSp[i-1] + (xxp[i-1] - xxp[i-1-r])/r)
  #previsao no instante n+3
  #Xchapeu_n+2(1) = Xchapeu_n+1(1) + (X_n+2 - X_n+2-r)/r
  i = n+3
  (MMSp[i] = MMSp[i-1] + (xxp[i-1] - xxp[i-1-r])/r)
  #previsao no instante n+4
  #Xchapeu_n+3(1) = Xchapeu_n+3(1) + (X_n+3 - X_n+3-r)/r
  i = n+4
  (MMSp[i] = MMSp[i-1] + (xxp[i-1] - xxp[i-1-r])/r)
  #previsao no instante n+5
  #Xchapeu_n+4(1) = Xchapeu_n+4(1) + (X_n+4 - X_n+4-r)/r
  i = n+5
  (MMSp[i] = MMSp[i-1] + (xxp[i-1] - xxp[i-1-r])/r)

  plot(c(x,rep(NA,5)),type="l",lwd=2,ylab="CO",xlab="instantes",bty="n" ,cex.lab=1.4,cex.axis=1.4)
  lines(c(rep(NA,length(x)), MMSp[(n+1):(n+5)]),lwd=2,type="o",col="red")
  
  #supondo que e_t ~ N(0,sigma^2)
  #podemos criar um IC para os valores previstos
  li    = NULL #limite inferior do IC com nivel de confian?a gama da previsao
  ls    = NULL #limite superior do IC com nivel de confian?a gama da previsao
  gama  = 0.95 #nivel de confianca
  alfag = 1-gama #nivel de significancia
  sigmachapeu = sd(res,na.rm=T)
  for(i in 1:5){
    q     = qnorm(1-alfag/2)
    li[i] = MMSp[n+i] - q*sigmachapeu/sqrt(r)
    ls[i] = MMSp[n+i] + q*sigmachapeu/sqrt(r)
  }
  #criando o grafico
  cord.x  = c((n+1):(n+5),(n+5):(n+1))
  cord.y  = c(li,ls[5:1])
  plot(c(x,rep(NA,5)),type="l",lwd=2,ylab="CO",xlab="instantes",bty="n" ,cex.lab=1.4,cex.axis=1.4, ylim=c(min(xxp,li), max(xxp,ls)+4))
  polygon(cord.x,cord.y,col="lightgray",border="lightgray")
  lines(c(rep(NA,length(x)), MMSp[(n+1):(n+5)]),lwd=2,type="o",col="red")
  points((n+1):(n+5),xp,pch="x",lwd=2)
  legend(locator(1),legend=c('valor previsto','Limites do IC de 95%','Valor verdadeiro'),box.lty=0,lwd=c(2,2,2),lty=c(1,1,NA),pch=c(NA,NA,"x"), col=c('red', 'gray','black'),cex=1.2)
  
  #descobrindo o melhor r para a previsao
  EQMp = NULL
  for(r in 1:n){
    MMS   = as.vector(filter(x, rep(1/r, r), sides=1))
    MMSp  = MMS
    #previsao no instante n+1
    MMSp[n+1] = MMS[n]
    for(i in 2:5){
        MMSp[n+i] = MMSp[n+i-1] + (xxp[n+i-1] - xxp[n+i-1-r])/r
    }
    EQMp[r] = mean((xp - MMSp[(n+1):(n+5)])^2)
  }
  plot(EQMp,type="l",xlab="r",bty="n")
  which.min(EQMp)
  

############################################################
#suavizacao exponencial simples
############################################################

alfa    = 0.5502
SES     = NULL
SES[1]  = x[1]
for(t in 2:n){SES[t] = alfa*x[t] + (1-alfa)*SES[t-1]}
SES

#ou
m = HoltWinters(x, alpha=alfa, beta = FALSE, gamma = FALSE)
m$fitted[,1] #m_t = previsao de x_t+1

#ou
ajuste = ses(x, alpha=alfa, initial="simple")
ajuste$fitted

cbind(m$fitted[,1],SES[1:(n-1)],ajuste$fitted[2:n])

#o que acontece quando vario alfa?
plot(x,type="l",lwd=2,ylab="CO",xlab="instantes",bty="n" ,cex.lab=1.4,cex.axis=1.4)
alfa2=0.3
m2 = HoltWinters(x, alpha=alfa2, beta = FALSE, gamma = FALSE)
lines(m2$fitted[,1],col="red",lwd=2)
alfa2=0.5
m2 = HoltWinters(x, alpha=alfa2, beta = FALSE, gamma = FALSE)
lines(m2$fitted[,1],col="blue",lwd=2)
alfa2=0.9
m2 = HoltWinters(x, alpha=alfa2, beta = FALSE, gamma = FALSE)
lines(m2$fitted[,1],col="purple",lwd=2)

#a funcao HoltWinters estima o melhor alfa
HW    = HoltWinters(x, beta = FALSE, gamma = FALSE)
(alfam  = HW$alpha)

#a funcao ses estima o melhor alfa
ajuste = ses(x)
ajuste$model$par[1]

#fazendo previsao para h passos a frente
SES[n]
alfa*x[n] + (1-alfa)*m$fitted[n-1,1]
  #atualizando a previsao a cada nova observacao
  SESp = SES  
  SESp[n+1] = SES[n]
  for(i in 2:5){
    SESp[n+i] = alfa*xp[i-1] + (1-alfa)*SESp[n+i-1]
  }
  SESp[(n+1):(n+5)]
  mean((xp - SESp[(n+1):(n+5)])^2)
  
  #supondo que e_t ~ N(0,sigma^2)
  #podemos criar um IC para os valores previstos
  li    = NULL #limite inferior do IC com nivel de confian?a gama da previsao
  ls    = NULL #limite superior do IC com nivel de confian?a gama da previsao
  gama  = 0.95 #nivel de confianca
  alfag = 1-gama #nivel de significancia
  res   = x - SES 
  sigmachapeu = sd(res,na.rm=T)
  for(i in 1:5){
    q     = qnorm(1-alfag/2)
    li[i] = SESp[n+i] - q*sigmachapeu*sqrt(alfa/(2-alfa))
    ls[i] = SESp[n+i] + q*sigmachapeu*sqrt(alfa/(2-alfa))
  }
  

  #criando o grafico
  cord.x  = c((n+1):(n+5),(n+5):(n+1))
  cord.y  = c(li,ls[5:1])
  plot(c(x,rep(NA,5)),type="l",lwd=2,ylab="CO",xlab="instantes",bty="n" ,cex.lab=1.4,cex.axis=1.4, ylim=c(min(xxp,li), max(xxp,ls)+4))
  polygon(cord.x,cord.y,col="lightgray",border="lightgray")
  lines(c(rep(NA,length(x)), SESp[(n+1):(n+5)]),lwd=2,type="o",col="red")
  points((n+1):(n+5),xp,pch="x",lwd=2)
  legend(locator(1),legend=c('valor previsto','Limites do IC de 95%','Valor verdadeiro'),box.lty=0,lwd=c(2,2,2),lty=c(1,1,NA),pch=c(NA,NA,"x"), col=c('red', 'gray','black'),cex=1.2)
  
#######################################################
#Suavizacao exponencial de Holt
#######################################################
  
dados = read.table("icv.txt",header=T)
x     = dados[1:114,3]
n     = length(x)
plot(x,type="l",lwd=2,ylab="ICV",xlab="instantes",bty="n" ,cex.lab=1.4,cex.axis=1.4)

alfa = 0.9
beta = 0.3
HW    = HoltWinters(x, alpha=alfa, beta=beta, gamma = FALSE)
(alfam  = HW$alpha)
(betam  = HW$beta)
HW$fitted
(HWp=predict(HW, n.ahead = 12, prediction.interval = T))
plot(HW,HWp,bty="n",lwd=2,xlab="instantes",ylab="Observado/Ajustado")

#######################################################
#Suavizacao exponencial de Holt-Winters
#######################################################

data(UKLungDeaths)
m = HoltWinters(ldeaths, seasonal = "addit")
p = predict(m, n.ahead = 12, prediction.interval = T)
plot(m,p,bty="n",lwd=2,xlab="instantes",ylab="Observado/Ajustado")


#outro conjunto de dados
#eletricidade mensal gerada nos EUA
data(electricity)
x           = window(electricity, start=c(1975,1),end=c(2004,12))
xp          = window(electricity, start=c(2005,1),end=c(2005,12))
matrizx     = matrix(x, ncol = 12,byrow=T)
vetorx      = as.vector(x)
vetorxp     = as.vector(xp)
vetorxxp    = c(x,xp)
n           = length(vetorx)
np          = length(vetorxp)
instantes   = 1:n
f           = rep(1:12,30)
anos        = rep(1975:2004,each=12)

plot(x,type="l",lwd=2,ylab="eletricidade",xlab="anos",bty="n" ,cex.lab=1.4,cex.axis=1.4)

#testando se ha indicios de nao haver tendencia
runs.test(vetorx) 
cox.stuart.test(vetorx)
cor.test(vetorx, instantes, method="spearman")

#testando se ha indicios de nao haver sazonalidade
kruskal.test(vetorx,f)
friedman.test(vetorx,f,anos)

#suavizacao de Holt-Winters
ajuste  = HoltWinters(x, seasonal = "addit")
p       = predict(ajuste, n.ahead = 12, prediction.interval = T)
plot(ajuste,p,bty="n",lwd=2,xlab="instantes",ylab="Observado/Ajustado")

plot(1:12,p[,1],bty="n",type="l",lwd=2,xlab="instantes",ylab="previs?o",axes=F,cex.axis=1.2,ylim=c(min(p),max(p)))
axis(1,labels=c("jan","fev","mar","abr","mai","jun","jul","ago", "set","out","nov","dez"),at=c(1:12),cex.axis=1.5,las=1)
axis(2,cex.axis=1.2)
cord.x  = c(1:12,12:1)
cord.y  = c(p[1:12,3],p[12:1,2])
polygon(cord.x,cord.y,col="lightgray",border="lightgray")
lines(1:12,p[,1],lwd=2)
lines(1:12,vetorxp,col="red",lwd=2,type="o")

