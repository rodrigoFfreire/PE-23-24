set.seed(2822)

n <- 100
m <- 5000

lambda_0 <- 2.90
lambda_1 <- 3.15

k <- 3.234

error1 <- 0
error2 <- 0

for (i in 1:m) {
  # Amostra sob a hipótese nula
  x0 <- rpois(n = n, lambda = lambda_0)
  
  # Amostra sob a alternativa
  x1 <- rpois(n = n, lambda = lambda_1)
  
  if (mean(x0) > k) {
    error1 <- error1 + 1
  }
  if (mean(x1) <= k) {
    error2 <- error2 + 1
  }
}

p_error1 = error1 / m
p_error2 = error2 / m

print(p_error2 / p_error1)
