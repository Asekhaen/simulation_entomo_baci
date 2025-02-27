
n_sites <- 74

n_intervention <- 51

n_non_intervention <- n_sites - n_intervention

lambda <- 29
reduction_multiplier <- 0.45 # percent reduction factor is 55%

lambda_intervention <- lambda*reduction_multiplier

non_intervertion_count <- rpois(
  n = n_non_intervention,
  lambda = lambda
)

intervention_count <- rpois(
  n = n_intervention,
  lambda = lambda_intervention
)


dat <- tibble(
  intervention = c(
    rep(
      "intervention",
      times = n_intervention
    ),
    rep(
      "non_intervention",
      times = n_non_intervention
    )
  ),
  count = c(
    intervention_count,
    non_intervertion_count
  )
)

write_csv(
  x = dat,
  file = "irs_count_dat.csv"
)
