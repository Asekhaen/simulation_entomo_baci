library(greta)
library(tidyverse)



prior_hist <- function(x){
  sims <- calculate(x, nsim = 1000)
  hist(sims$x, breaks = 100)
}

#Parameters
intervention_percent_reduction <- uniform(
  min = 0,
  max = 100
)
prior_hist(intervention_percent_reduction)


study_avg_wt <- normal(
  mean = 4000,
  sd = 3000,
  truncation = c(0, Inf)
)

# study_avg_wt <- uniform(
#   min = 0,
#   max = 10000
# )

prior_hist(study_avg_wt)

inter_village_sd <- normal(
  mean = 0,
  sd = 0.5,
  truncation = c(0, Inf)
)
prior_hist(inter_village_sd)

inter_household_sd <- normal(
  mean = 0,
  sd = 0.5,
  truncation = c(0, Inf)
)
prior_hist(inter_household_sd)

inter_month_sd  <- normal(
  mean = 0,
  sd = 0.5,
  truncation = c(0, Inf)
)
prior_hist(inter_month_sd)

n_villages <- 8
n_households_per_village <- 11

data_template <- expand_grid(
  village = seq_len(n_villages),
  household = seq_len(n_households_per_village),
  month = c(1:3, 13:15),
  weight = NA
) %>%
  mutate(
    before_after = case_when(
      month %in% 1:3 ~ "before",
      month %in% 13:15 ~ "after"
    ),
    arm = case_when(
      village <= round(n_villages / 2) ~ "control",
      .default = "intervention"
    ),
    .before = "weight"
  ) %>%
  mutate(
    # make unique combinations of households and villages
    household_village_combination = paste(village,
                                          household,
                                          sep = "-"),
    unique_household = match(household_village_combination,
                             unique(household_village_combination)),
    .after = "household"
  ) %>%
  select(
    -household_village_combination
  )


village_avg_wt_diffs <- normal(
  mean = 0,
  sd = inter_village_sd,
  dim = n_villages
)


# # hierarchical decentralisation
# village_avg_wt_diffs_raw <- normal(
#   mean = 0,
#   sd = 1,
#   dim = n_villages
# )
# 
# village_avg_wt_diffs <- village_avg_wt_diffs_raw * inter_village_sd


unique_household_avg_wt_diffs <- normal(
  mean = 0,
  sd = inter_household_sd,
  dim = n_households_per_village*n_villages
)


# # hierarchical decentralisation
# unique_household_avg_wt_diffs_raw <- normal(
#   mean = 0,
#   sd = 1,
#   dim = n_households_per_village*n_villages
# )
# 
# unique_household_avg_wt_diffs <- unique_household_avg_wt_diffs_raw * inter_household_sd


# # this is the version that does not end with a probability distribution
# 
# n_months <- 6
# month_household_avg_wt_diffs <- normal(
#   mean = 0,
#   sd = inter_month_sd,
#   dim = n_months*n_villages*n_households_per_village
# )
# 
# 
# nonintervention_log_weights <- log(study_avg_wt) +
#   village_avg_wt_diffs[data_template$village] +
#   unique_household_avg_wt_diffs[data_template$unique_household] +
#   month_household_avg_wt_diffs
# 
# sims_old_way <- calculate(nonintervention_log_weights[50],
#                           nsim = 10000)
# hist(sims_old_way$`nonintervention_log_weights[50]`,
#      breaks = 100)

intervention_multiplier <- 1 - intervention_percent_reduction / 100
# log_intervention_multiplier <- log(intervention_multiplier)

# create a dummy variable for whether there is an effect of the intervention
intervention_in_place <- as.numeric(
  data_template$before_after == "after" &
    data_template$arm == "intervention"
)

intervention_multiplier_vec <- (1 - intervention_in_place) +
  intervention_in_place * intervention_multiplier

log_intervention_multiplier_vec <- log(intervention_multiplier_vec)

# calculate(log_intervention_multiplier_vec,
#           values = list(
#             intervention_percent_reduction = 10))

# this is the version that ends with a probability distribution (so that we can
# define a likelihood over some data)
nonintervention_log_weights_household <- log(study_avg_wt) +
  village_avg_wt_diffs[data_template$village] +
  unique_household_avg_wt_diffs[data_template$unique_household]

intervention_log_weights_household <- nonintervention_log_weights_household + 
  log_intervention_multiplier_vec

# sims_new_way <- calculate(nonintervention_log_weights_observations[50],
#                           nsim = 10000)
# hist(sims_new_way$`nonintervention_log_weights_observations[50]`,
#      breaks = 100)


# this is useful for simulating data, but once we want to fit to data
# it confuses out dag and is hanging superfulously onto our model, so we 
# remove it for now
# log_weights_observations <- normal(
#   mean = intervention_log_weights_household,
#   sd = inter_month_sd
# )
# 
# 
# m <- model(log_weights_observations)
# 
# plot(m)
# 
# simulated_log_weights <- calculate(log_weights_observations, nsim = 1)
# 
# 
# sim_weights <- exp(unlist(simulated_log_weights))
# 
# sim_data <- data_template |>
#   mutate(
#     weight = sim_weights
#   )
# 
# 
# sim_data %>%
#   mutate(
#     intervened = arm == "intervention" & before_after == "after",
#     before_after = factor(before_after,
#                           levels = c("before", "after"))
#   ) %>%
#   ggplot(
#     aes(
#       x = weight,
#       fill = intervened
#     )
#   ) +
#   geom_histogram() +
#   facet_grid(arm ~ before_after) +
#   coord_flip()

# read in real (simulated) data
ento_baci_data <- read_csv(
  file = "ento_baci_data.csv"
)

plot_baci_data <- function(x){
  x %>%
    mutate(
      intervened = arm == "intervention" & before_after == "after",
      before_after = factor(before_after,
                            levels = c("before", "after"))
    ) %>%
    ggplot(
      aes(
        x = weight,
        fill = intervened
      )
    ) +
    geom_histogram() +
    facet_grid(arm ~ before_after) +
    coord_flip()
}

plot_baci_data(ento_baci_data)


data <- ento_baci_data |>
  mutate(
    log_weights = log(weight)
  )


# here we created a greta node for simulation
# log_weights_observations <- normal(
#   mean = intervention_log_weights_household,
#   sd = inter_month_sd
# )

log_weights <- as_data(data$log_weights)

# here we have our data being drawn from model distrubutions
distribution(log_weights) <- normal(
  mean = intervention_log_weights_household,
  #mean = nonintervention_log_weights_household,
  sd = inter_month_sd
)

# alternative ways of feeding in the weights
# distribution(log(data$weights)) <- normal(
#   mean = intervention_log_weights_household,
#   sd = inter_month_sd
# )

# distribution(data$weights) <- lognormal(
#   mean = intervention_log_weights_household,
#   sd = inter_month_sd
# )

log_weights

prior_sims <- calculate(
  log_weights,
  nsim = 1000
)

dim(prior_sims$log_weights)
which_row <- 227

data_point_prior_sims <- prior_sims$log_weights[,which_row,1]
data_point_truth <- data$log_weights[which_row]
mean(data_point_prior_sims < data_point_truth)

hist(data_point_prior_sims)
abline(v = data_point_truth)


bayesplot::ppc_dens_overlay(
  y = data$log_weights,
  yrep = prior_sims$log_weights[,,1]
)


bayesplot::ppc_ecdf_overlay(
  y = data$log_weights,
  yrep = prior_sims$log_weights[,,1]
)


# create our model object
baci_model <- model(
  #intervention_log_weights_household,
  inter_month_sd,
  inter_household_sd,
  inter_village_sd,
  intervention_percent_reduction,
  study_avg_wt
)

# plot(baci_model)

# # 
# inits <- initials(
#   inter_month_sd = 10#,
#   #inter_village_sd = 0.3,
#   #inter_village_sd = 0.3,
#   #study_avg_wt = 4000,
#   #intervention_percent_reduction = 27
# )


draws <- mcmc(
  model = baci_model,
  n_samples = 1000,
  warmup = 10000,
  chains = 8#,
  #initial_values = inits
)



library(bayesplot)
mcmc_trace(
  x = draws,
  pars = c(
    "study_avg_wt",
    "intervention_percent_reduction",
    "inter_month_sd",
    "inter_household_sd",
    "inter_village_sd"
  )
)

# Rhat / Gelman-Rubin diagnostic / Potential scale reduction factor
library(coda)
gelman.diag(
  x = draws,
  autoburnin = FALSE
)

# fit summary
summary(draws)

# maximum likelihood
baci_maxlik <- opt(
  model = baci_model
)

baci_maxlik

### randomised quantile residuals

log_weights

log_weights_sim <- calculate(
  log_weights,
  values = draws,
  nsim = 100
)


library(DHARMa)
resids <- createDHARMa(
  simulatedResponse = t(log_weights_sim$log_weights[,,1]),
  observedResponse = data$log_weights
)

plot(resids)


# can also convert to expected normality
res <- resids$scaledResiduals
hist(res, breaks = 100)

res_normal <- qnorm(res)
hist(res_normal)


