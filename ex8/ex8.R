library(pracma)

set.seed(1592)
n <- 12

data <- c(31.8,31.7,35.2,37.1,31.7,36.1,36.3,33.2,34.3,37.5,30.4,34.6,32.4,31.7,30.2,34.3,35.6,34.9,38.9)

samples <- sample(data, n, replace = FALSE)

gamma <- 0.96

a <- qchisq((1 - gamma) / 2, df = n - 1)
b <- qchisq((1 + gamma) / 2, df = n - 1)

s2 <- var(samples)

lower_bound <- (n - 1) * s2 / b
upper_bound <- (n - 1) * s2 / a

F <- function(x) {
  eq1 <- pchisq(x[2], df = n - 1) - pchisq(x[1], df = n - 1) - gamma
  eq2 <- dchisq(x[2], df = n + 3) - dchisq(x[1], df = n + 3)
  
  return(c(eq1, eq2))
}

# Resolver as equações para obter (c, d)
quantis <- fsolve(F, x0 = c(a,b))$x

c <- quantis[1]
d <- quantis[2]

# Calcular o novo intervalo de confiança para sigma^2
lower_bound_new <- (n - 1) * s2 / d
upper_bound_new <- (n - 1) * s2 / c

print(abs((upper_bound - lower_bound) - (upper_bound_new - lower_bound_new)))