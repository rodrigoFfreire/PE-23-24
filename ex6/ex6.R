set.seed(1948)

a <- 4
ns <- 30
time <- 90

# Simulated
samples <- matrix(data = rexp(n = 1000 * ns, rate = 1/a), nrow = 1000, ncol = 30)

Ys <- rowSums(samples)

prop_over_limit <- mean(Ys > time)

print(prop_over_limit)

# Exact
p_exact <- 1 - pgamma(time, ns, 1/a)

diff_abs <- abs(prop_over_limit - p_exact)

diff_abs * 100