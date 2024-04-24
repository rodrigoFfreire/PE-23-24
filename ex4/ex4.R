set.seed(2255)

n_sims <- 150
beep_code <- 2
off_code <- 1

beeps <- 0
offs <- 0


for (i in 1:n_sims) {
  signals <- sample(1:10, 9, replace = TRUE, prob = (1:10)/55)
  
  if (beep_code %in% signals) {
    beeps <- beeps + 1
  }
  
  if (off_code %in% signals) {
    offs <- offs + 1
  }
}

print(round(beeps / (n_sims - offs), 2))