#Aula 5

library(TSA)

#Processo de medias moveis de 1a ordem e 
#ruido branco normalmente distribuido
#x_t = e_t - theta e_t-1
  data(ma1.1.s) #theta=0.9
  x     = ma1.1.s
  theta = 0.9
  
  data(ma1.2.s) #theta=-0.9
  x     = ma1.2.s
  theta = -0.9
  
  e       = rnorm(1001, 0, sqrt(5))
  x       = NULL
  theta   = 0.6
  for(t in 1:1000){    x[t] = e[t+1] - theta * e[t]  }


plot(x, ylab=expression(x[t]),xlab="instantes",type="o")

xd = zlag(x)
plot(xd, x, xlab=expression(x[t-1]), ylab=expression(x[t]),type="p")
cor(xd[2:length(x)], x[2:length(x)])
-theta / (1+theta^2)

xd2 = zlag(x,2)
plot(xd2, x, xlab=expression(x[t-2]), ylab=expression(x[t]),type="p")
cor(xd2[3:length(x)], x[3:length(x)])

xd3 = zlag(x,3)
plot(xd3, x, xlab=expression(x[t-3]), ylab=expression(x[t]),type="p")
cor(xd3[4:length(x)], x[4:length(x)])


#Processo de medias moveis de 2a ordem e 
#ruido branco normalmente distribuido
#x_t = e_t - theta_1 e_t-1 - theta_2 e_t-2

  data(ma2.s)
  x       = ma2.s
  theta1  = 1
  theta2  = -0.6

plot(x, ylab=expression(x[t]),xlab="instantes",type="o")

xd = zlag(x)
plot(xd, x, xlab=expression(x[t-1]), ylab=expression(x[t]),type="p")
cor(xd[2:length(x)], x[2:length(x)])
(-theta1+theta1*theta2) / (1+theta1^2+theta2^2)

xd2 = zlag(x,2)
plot(xd2, x, xlab=expression(x[t-2]), ylab=expression(x[t]),type="p")
cor(xd2[3:length(x)], x[3:length(x)])
(-theta2) / (1+theta1^2+theta2^2)

xd3 = zlag(x,3)
plot(xd3, x, xlab=expression(x[t-3]), ylab=expression(x[t]),type="p")
cor(xd3[4:length(x)], x[4:length(x)])

#Processos autoregressivos

  data(ar1.s)
  x   = ar1.s
  phi = 0.9

  plot(x, ylab=expression(x[t]),xlab="instantes",type="o")
  
  xd = zlag(x)
  plot(xd, x, xlab=expression(x[t-1]), ylab=expression(x[t]),type="p")
  cor(xd[2:length(x)], x[2:length(x)])
  phi
  
  xd2 = zlag(x,2)
  plot(xd2, x, xlab=expression(x[t-2]), ylab=expression(x[t]),type="p")
  cor(xd2[3:length(x)], x[3:length(x)])
  phi^2
  
  xd3 = zlag(x,3)
  plot(xd3, x, xlab=expression(x[t-3]), ylab=expression(x[t]),type="p")
  cor(xd3[4:length(x)], x[4:length(x)])
  phi^3
  