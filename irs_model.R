# irs model

# read in  and explore data

library(tidyverse)
library(greta)

irs_data <- read_csv("irs_count_dat.csv")

glimpse(irs_data)

count <- as_data(irs_data$count)

hist(irs_data$count)

table(irs_data$intervention)

ggplot(irs_data) +
  geom_bar(
    aes(x = count, fill = intervention)
  )


## model set up

# priors

### intervention priors and 'multiplier'
intervention_percent_reduction <- uniform(
  min = 0,
  max = 100
)

intervention_multiplier <- 1 - intervention_percent_reduction / 100

# create a dummy variable for whether there is an effect of the intervention
intervention_in_place <- as.numeric(
  irs_data$intervention == "intervention"
)

intervention_multiplier_vec <- (1 - intervention_in_place) +
  intervention_in_place * intervention_multiplier

log_irs_vec <- log(intervention_multiplier_vec)

# count prior

# # using lognormal
# log_lambda_average <- lognormal(
#   meanlog = 0.35,
#   sdlog = 0.4
#   #meanlog = log(30),
#   #sdlog = log(20)
# )
# lambda_average <- exp(log_lambda_average)

# > log(30)
# [1] 3.401197
# > log(20)
# [1] 2.995732

# # using normal then logging
lambda_average <- normal(
  mean = 30,
  sd = 20,
  truncation = c(0, Inf)
)
log_lambda_average <- log(lambda_average)


log_lambda_final <- log_lambda_average + log_irs_vec

lambda_final <- exp(log_lambda_final)


distribution(count) <- poisson(lambda = lambda_final)

m <- model(lambda_average, intervention_percent_reduction)

plot(m)

# prior predictive checks

prior_sims <- calculate(
  count,
  nsim = 1000
)

dim(prior_sims$count)
which_row <- 12

irs_data_point_prior_sims <- prior_sims$count[,which_row,1]
irs_data_point_truth <- irs_data$count[which_row]
mean(irs_data_point_prior_sims < irs_data_point_truth)

hist(
  irs_data_point_prior_sims,
  #xlim = c(0, 100),
  breaks = 100
)
abline(v = irs_data_point_truth)

hist(log(irs_data_point_prior_sims))
abline(v = log(irs_data_point_truth))


bayesplot::ppc_dens_overlay(
  y = irs_data$count,
  yrep = prior_sims$count[,,1]
)


bayesplot::ppc_ecdf_overlay(
  y = irs_data$count,
  yrep = prior_sims$count[,,1]
)



# model fitting
draws <- mcmc(
  model = m,
  n_samples = 1000,
  warmup = 1000,
  chains = 8
)

# model checking

# convergence
library(bayesplot)
mcmc_trace(draws)

coda::gelman.diag(
  x = draws,
  autoburnin = FALSE
)

# model fit to data
### randomised quantile residuals

counts_sim <- calculate(
  count,
  values = draws,
  nsim = 100
)

dim(counts_sim$count)

library(DHARMa)
resids <- createDHARMa(
  simulatedResponse = t(counts_sim$count[,,1]),
  observedResponse = irs_data$count
)

plot(resids)


# can also convert to expected normality
res <- resids$scaledResiduals
hist(res, breaks = 100)

res_normal <- qnorm(res)
hist(res_normal)





# result
summary(draws)


