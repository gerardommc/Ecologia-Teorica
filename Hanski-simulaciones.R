coord.cont <- c(0,0)

set.seed(1354)

x = runif(10, 0, 1)
y = runif(10, 0, 1)

coord.islas <- data.frame(x, y)

area.cont <- rnorm(1, 200, 1)

area.is <- (rnorm(10, 2, 2))^2

plot(x, y, xlim = c(0,1), ylim = c(0,1),  pch = 20, cex = area.is/5) #Gráfica de las islas
for(i in 1:10){
      lines(c(0, x[i]), c(0, y[i]), lty = 2, col = "grey")
  } #Pintar las líneas
points(0,0, pch = 20, col = "red", cex = area.cont/10) #Punto de la fuente continental

dist <- sqrt(x^2 + y^2)

## Número de especies continentales

#Función para E
E.p <- function(A, e=0.01, X=1.009){
  Ei <- ifelse(A > e^(1/X), e/(A^X), 1)
  Ei}

x.is <- exp(rnorm(1, mean = 0.1, sd = 0.2))
e <- exp(rnorm(1))

Ei <- E.p(A = area.is,
          e = e,
          X = x.is)

#Función para Si
C.p <- function(d, yprima, alfa, area){
  Sij <- exp(-alfa*d ) * area
  Ci <- 1/(1 + (yprima/Sij)^2)
  Ci
}

yprima <- exp(rnorm(1))
alfa <- exp(rnorm(1))

Ci <- numeric(10)
for(i in 1:10){
  Ci[i] <- C.p(d = dist[i],
               yprima = yprima,
               alfa = alfa,
               area = area.is[i])
}

Ci  

Ji <- Ci/(Ci + Ei)

plot(x,y, pch = 20, cex = Ji*2,
     xlim = c(0,1),
     ylim = c(0,1))

sim.sp <- c()
for(i in 1:10){sim.sp[i] <- rbinom(1, 1, Ji[i])}

s <- c()
for(j in 1:100){
for(i in 1:10){
  s[i] <- rbinom(1, 1, Ji[i])
}
  sim.sp <- rbind(s, sim.sp)
  
  }
sim.sp

for(i in 1:100){
  plot(x, y, cex = sim.sp[i, ], pch = 20)
}
