R <- matrix(0, ncol = 100, nrow = 8)
R[, 1] <- 10

par <- expand.grid(Ix = c(0.75, 1.5), Ex = c(0.75, 1.25), P = c(20, 40))

for(j in 1:nrow(par)){
  Ix <- par$Ix[j]; Ex <- par$Ex[j]; P <- par$P[j]
    for(i in 2:ncol(R)){
      R[j, i] <- R[j, i-1] + Ix - (Ix/P)*R[j, i-1] - Ex/P*R[j, i-1]
    }
  }

matplot(t(R), type = "l")

for(i in 1:8){plot(1:100, R[i, ], type = "l", main = par[i,])}
