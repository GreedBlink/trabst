x = c(84.6,89.9,81.9,95.4,91.2,89.8,89.7,97.9,103.4,107.6,120.4,109.6)
n=length(x)

t=1:n

(betachapeu = (sum(t*x) - n*mean(x)*mean(t))/ (sum(t^2)-n*mean(t)^2))
(muchapeu = mean(x) - betachapeu*mean(t))

(ajuste=lm(x~t))
names(ajuste)

#Coeficientes
ajuste$coefficients[1]
ajuste$coefficients[2]
#Ou 
ajuste$coefficients[1:2]

#X ajustado
ajuste$fitted.values
muchapeu+betachapeu*t

#Resíduos

ajuste$residuals
x-muchapeu-betachapeu*t

#Avaliando resíduos

plot(ajuste$residuals)
abline(h=0)

plot(ajuste$residuals,x)

plot(ajuste$residuals,t)

plot(t,x)
lines(t,ajuste$fitted.values)

hist(ajuste$residuals,prob=T,ylab="Densidade",xlab="Resíduos",main="Histograma")

qqnorm(ajuste$residuals)
qqline(ajuste$residuals)

acf(ajuste$residuals)

#Testes de normalidade
shapiro.test(ajuste$residuals)
ks.test(ajuste$residuals,"pnorm",mean(ajuste$residuals),sd(ajuste$residuals))
#Não rejeito a hipótese de normalidade para e_t

summary(ajuste)
# Em Pr(>|t|) temos o p-valor relacionado ao teste que verifica se o parâmetro
# difere significativamente de 0. Rejeitar a hipótese nula (par = 0) é o que
# desejamos.

######## Gerando novos dados ##########

mu=100
beta=2
t=1:1000

x=mu+beta*t+rnorm(1000,0,sqrt(0.5))

ajuste=lm(x~t)

muchapeu=ajuste$coefficients[1]
betachapeu=ajuste$coefficients[2]


#X ajustado

ajuste$fitted.values
muchapeu+betachapeu*t

#Resíduos

ajuste$residuals
x-muchapeu-betachapeu*t

plot(ajuste$residuals)
abline(h=0,col=3)

plot(x,ajuste$residuals)
plot(t,ajuste$residuals)

plot(t,x)
lines(t,ajuste$fitted.values,col=3)

hist(ajuste$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="Histograma dos resíduos")
curve(dnorm(x,mean(ajuste$residuals),sd(ajuste$residuals)),add=T)

qqnorm(ajuste$residuals)
qqline(ajuste$residuals,col=3)

acf(ajuste$residuals)

#testando normalidade
shapiro.test(ajuste$residuals)
ks.test(ajuste$residuals,"pnorm",mean(ajuste$residuals),sd(ajuste$residuals))

summary(ajuste)


#######Gerando novo conjunto de dados ###########

mu=100
beta=2
n=1000

t=1:n
x=mu+beta*exp(t/1000)+runif(n,-2,2)

ajuste=lm(x~t)

summary(ajuste)

muchapeu=ajuste$coefficients[1]
betachapeu=ajuste$coefficients[2]

#X ajustado

ajuste$fitted.values
muchapeu+betachapeu*t


#Resíduos
ajuste$residuals
x-muchapeu-betachapeu*t

plot(ajuste$residuals)
abline(h=0,col=3)

plot(t,x)
lines(t,ajuste$fitted.values,col=3)

plot(x,ajuste$residuals)
plot(t,ajuste$residuals)

hist(ajuste$residuals,prob=T)
curve(dnorm(x,mean(ajuste$residuals),sd(ajuste$residuals)),add=T)
curve(dunif(x,min(ajuste$residuals),max(ajuste$residuals)),add=T)

shapiro.test(ajuste$residuals)
ks.test(ajuste$residuals,"pnorm",mean(ajuste$residuals),sd(ajuste$residuals))

qqnorm(ajuste$residuals)
qqline(ajuste$residuals,col=3)

acf(ajuste$residuals)

#######Gerando novo conjunto de dados ###########

mu=100
beta=2
n=1000

t=1:n
x=e=NULL

e[1]=runif(1,-0.5,0.5)
x[1]=mu+beta*exp(t[1]/1000)+e[1]
for(i in 2:n){
  e[i]=e[i-1]+runif(1,-2,2)
  x[i]=mu+beta*exp(t[i]/1000)+e[i]
}


ajuste=lm(x~t)

summary(ajuste)

muchapeu=ajuste$coefficients[1]
betachapeu=ajuste$coefficients[2]

#X ajustado

ajuste$fitted.values
muchapeu+betachapeu*t


#Resíduos
ajuste$residuals
x-muchapeu-betachapeu*t

plot(ajuste$residuals)
abline(h=0,col=3)

plot(t,x)
lines(t,ajuste$fitted.values,col=3)

plot(x,ajuste$residuals)
plot(t,ajuste$residuals)

hist(ajuste$residuals,prob=T)
curve(dnorm(x,mean(ajuste$residuals),sd(ajuste$residuals)),add=T)

shapiro.test(ajuste$residuals)
ks.test(ajuste$residuals,"pnorm",mean(ajuste$residuals),sd(ajuste$residuals))

qqnorm(ajuste$residuals)
qqline(ajuste$residuals,col=3)

acf(ajuste$residuals) #Dependência entre os resíduos

