#### Modelos auto regressivos ####

n <- 1000
sig <- 1
e <- rnorm(n,sd = sig)
x <- 1

#### Modelo 1 ####

phi <- 0.8
for(i in 2:(n+1)){
  x[i] <- phi*x[i-1] + e[i-1]
}

plot(x, type="l", main = "AR(1)")
acf(x) #gráfico de auto correlação

#Sabemos que cor(x[1:(n-j)], x[(j+1):n]) = phi^j

c(cor(x[1:(n-1)],x[2:n]), phi)
c(cor(x[1:(n-5)],x[6:n]), phi^5)

#Sabemos que cov(x[1:(n-j)], x[(j+1):n]) = phi^j * sigma^2 / (1-phi^2)
c(cov(x[1:(n-1)],x[2:n]), phi^1 * sig / (1 - phi^2))

#Estimando phi
ar(x,order.max = 1)

#### Modelo 2 ####
phi <- 1
for (i in 2:(n+1)){
  x[i] <- x[i-1] + e[i-1]
}

plot(x, type="l", main = "AR(1)")
acf(x) #gráfico de auto correlação

#Sabemos que cor(x[1:(n-j)], x[(j+1):n]) = phi^j

c(cor(x[1:(n-1)],x[2:n]), phi)
c(cor(x[1:(n-5)],x[6:n]), phi^5)

#Sabemos que cov(x[1:(n-j)], x[(j+1):n]) = phi^j * sigma^2 / (1-phi^2)
c(cov(x[1:(n-1)],x[2:n]), phi^1 * sig / (1 - phi^2))

#Estimando phi
ar(x,order.max = 1)

#### Modelo 3 ####

for (i in 2:(n+1)){
  x[i] <- 5*x[i-1] + e[i-1]
}

plot(x, type="l", main = "AR(1)")
acf(x) #gráfico de auto correlação

#Sabemos que cor(x[1:(n-j)], x[(j+1):n]) = phi^j

c(cor(x[1:(n-1)],x[2:n]), phi)
c(cor(x[1:(n-5)],x[6:n]), phi^5)

#Sabemos que cov(x[1:(n-j)], x[(j+1):n]) = phi^j * sigma^2 / (1-phi^2)
c(cov(x[1:(n-1)],x[2:n]), phi^1 * sig / (1 - phi^2))

#Estimando phi
ar(x,order.max = 1)
