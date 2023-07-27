# Parameters
c <- 0.1     # Intrinsic growth rate
h <- 10
e <- 0.05      # Extinction rate

# Initial population size
P0 <- 5

# Time step and total time
dt <- 0.1
total_time <- 100

# Simulation loop
time <- seq(0, total_time, by = dt)
P <- numeric(length(time))
P[1] <- P0

for (i in 2:length(time)) {
  dP_dt <- c * P[i - 1] * (h - P[i - 1]) - e * P[i - 1]
  P[i] <- P[i - 1] + dP_dt * dt
  
  # Ensure population stays within bounds
  P[i] <- pmax(0, P[i])
}

# Plot the results
plot(time, P, type = "l", xlab = "Tiempo", ylab = "Tamaño poblacional", main = "Modelo simple de Levins")
