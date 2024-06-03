set.seed(2126)

n <- 4
r <- 150
m <- 130
l <- -0.9

samples <- matrix(NA, nrow = r, ncol = m)

for (i in 1:r) {
  for (j in 1:m) {
    z <- rnorm(n + 1)
    samples[i, j] <- (sqrt(n) * z[1] / sqrt(sum(z[-1]^2)))
  }
}

proportions <- apply(samples, 1, function(row) mean(row <= l))

estimated_p <- mean(proportions)

direct_p <- pt(l, df = n)

diff_abs <- abs(estimated_p - direct_p) * 100

diff_abs