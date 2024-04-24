set.seed(2255)

n_sims <- 150
beep_code <- 2
off_code <- 1

notoff <- 0
beeps <- 0

for (i in 1:n_sims) {
  signals <- sample(1:10, 9, replace = TRUE, prob = (1:10)/55)
  
  if (!(off_code) %in% signals) {
    notoff <- notoff + 1
    
    if (beep_code %in% signals) {
      beeps <- beeps + 1
    }
  }
}

notoff_prob <- notoff / n_sims
beeps_prob <- beeps / n_sims

print(round(beeps_prob/notoff_prob,2))