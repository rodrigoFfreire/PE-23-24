set.seed(2126)

n <- 4
r <- 150
m <- 130

calc_T <- function(z) {
  sqrt(n) * (z[1] / sqrt(sum(z[-1]^2)))
}

# Gerar as amostras
samples <- matrix(rnorm(r * (m + 1)), ncol = m + 1)

# Calcular T para cada amostra
Ts <- apply(samples, 1, calc_T)

# Calcular a proporção de valores menores ou iguais a -0.9 em cada amostra
proportions <- mean(Ts <= -0.9)

# Calcular a média das proporções
mean_proportion <- mean(proportions)

# Calcular a probabilidade direta usando a função pt do R
direct_prob <- pt(-0.9, df = n)

# Calcular a diferença absoluta
diff_abs <- abs(mean_proportion - direct_prob) * 100

# Arredondar o resultado final
result <- round(diff_abs, 5)
result