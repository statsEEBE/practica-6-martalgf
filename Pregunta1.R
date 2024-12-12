
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)

xbar <- mean(x)
sigma <- sqrt(25)
n <- length(x)
z0.95 <- qnorm(0.95)

#a)
c(xbar-z0.95*sigma/(sqrt(n)), xbar+z0.95*sigma/(sqrt(n)))

#Instalar libreria
install.packages("BSDA")
library(BSDA)

#Calcular el intervalo de confianza si tenenemos: 
  #la muestra, sigma y el porcentage de confianza
z.test(x, sigma.x = sigma, conf.level = 0.9)

#CRITERIO 2

#Probar la hipotesi de que las cajas tienen un peso diferente a 500g
  # H0: mu=500
  # H1: mu != 500 (mu0=500)
zc <- qnorm(0.95)
zc 
mu0 <- 500 
zobs <- (xbar-mu0)/(sigma/sqrt(n))
zobs
#RECHAZAMOS H0, por lo tanto, aceptamos la alternativa (H1)

z.test(x, sigma.x = sigma, conf.level = 0.9, mu=mu0) #z=zobs
#Si zobs no está (-zc, zc) zc=z_crítico -> rechazamos H0

#CRITERIO 3 (p-valor)
  #Si p-valor < alpha -> rechazamos la hipotesi nula (H0)
#para dos colas
pvalor <- 2*pnorm(-zobs)
pvalor
#para cola superior
z.test(x, sigma.x = sigma, conf.level = 0.9, mu=mu0, alternative = "greater")

#b)
n <- (qnorm(0.975)*sigma)^2
n
z025 <- qnorm(0.975)
c(xbar-z025*sigma/sqrt(n),xbar+z025*sigma/sqrt(n)) #comprobación
504.75-502.75 

#c)
xabar <- mean(x)
n <- length(x)
t005 <- qt(0.995, n-1)
s <- sd(x)

c(xbar-t005*s/sqrt(n), xbar+t005*s/sqrt(n))
t.test(x, conf.level = 0.99)

#Hipotesi
#H0: mu=mu0=500   
#H1: mu!=mu0=500
t.test(x, alternative = "two.sided", conf.level = 0.99, mu=500)
#calcular tobs
#comparar p-valor con alpha