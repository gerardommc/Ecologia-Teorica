# Parameters
r <- 0.1       # Intrinsic growth rate
K <- 100       # Carrying capacity
e <- 0.05      # Extinction rate
m <- 0.02      # Migration rate

# Initial population sizes in two patches
N10 <- 20
N20 <- 50

# Time step and total time
dt <- 0.1
total_time <- 100

# Simulation loop
time <- seq(0, total_time, by = dt)
N1 <- numeric(length(time))
N2 <- numeric(length(time))
N1[1] <- N10
N2[1] <- N20

for (i in 2:length(time)) {
  dN1_dt <- r * N1[i - 1] * (1 - N1[i - 1] / K) - e * N1[i - 1] + m * (N2[i - 1] - N1[i - 1])
  dN2_dt <- r * N2[i - 1] * (1 - N2[i - 1] / K) - e * N2[i - 1] + m * (N1[i - 1] - N2[i - 1])
  
  N1[i] <- N1[i - 1] + dN1_dt * dt
  N2[i] <- N2[i - 1] + dN2_dt * dt
  
  # Ensure populations stay within bounds
  N1[i] <- pmax(0, pmin(K, N1[i]))
  N2[i] <- pmax(0, pmin(K, N2[i]))
}

# Plot the results
plot(time, N1, type = "l", xlab = "Tiempo", ylab = "Tamaño de la población", col = "blue", main = "Modelo de Parches Conectados")
lines(time, N2, col = "red")
legend("topright", legend = c("Parche 1", "Parche 2"), col = c("blue", "red"), lty = 1)
