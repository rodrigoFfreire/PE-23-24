library(stats4)

dataa <- c(8.54,4.76,5.15,4.96,6.25,7.22,12.9,6.04,8.86,4.88,6.54,4.53,4.7,5.38,5.96,5.17,5.09,5.11)

a <- 4.5

log_likelihood <- function(theta, x) {
  if (x < 4.5) {
    return(0)
  }
  return -(log((theta / x^(theta + 1)) * a^(theta)))
}

mle_result <- mle(log_likelihood, start = list(theta = 4.5), data = dataa)

print(mle_result)