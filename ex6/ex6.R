set.seed(1948)

a <- 4
ns <- 30
time <- 90

# Simulated
samples <- matrix(data = rexp(n = 1000 * ns, rate = 1/a), nrow = 1000, ncol = 30)

Ys <- rowSums(samples)

p_simulated <- mean(Ys > time)

# Exact
p_exact <- 1 - pgamma(time, ns, 1/a)

diff_abs <- abs(p_simulated - p_exact)

diff_abs * 100