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

inter_month_sd  <- normal(
  mean = 0,
  sd = 0.5,
  truncation = c(0, Inf)
)

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

unique_household_avg_wt_diffs <- normal(
  mean = 0,
  sd = inter_household_sd,
  dim = n_households_per_village*n_villages
)

n_months <- 6
month_household_avg_wt_diffs <- normal(
  mean = 0,
  sd = inter_month_sd,
  dim = n_months*n_villages*n_households_per_village
)


# # this is the version that does not end with a probability distribution
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
intervention_in_place <- as.numeric(data_template$before_after == "after" &
                                      data_template$arm == "intervention")

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

log_weights_observations <- normal(
  mean = intervention_log_weights_household,
  sd = inter_month_sd
)

# distribution(data$log_weights) <- normal(
#   mean = intervention_log_weights_household,
#   sd = inter_month_sd
# )

# distribution(log(data$weights)) <- normal(
#   mean = intervention_log_weights_household,
#   sd = inter_month_sd
# )

# distribution(data$weights) <- lognormal(
#   mean = intervention_log_weights_household,
#   sd = inter_month_sd
# )

# sims_new_way <- calculate(nonintervention_log_weights_observations[50],
#                           nsim = 10000)
# hist(sims_new_way$`nonintervention_log_weights_observations[50]`,
#      breaks = 100)






nonintervention_weights <- exp(nonintervention_log_weights)

m <- model(study_avg_wt, village_avg_wt_diffs)

plot(m)
prior_hist(nonintervention_weights[1])

# check the prior density over one of the simulations

sims <- calculate(nonintervention_weights[1], nsim = 10000)
# hist(sims$`nonintervention_weights[1]`, breaks = 100,
#      xlim = c(0, 50000))
# mean(sims$`nonintervention_weights[1]` < 50000)

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

# 8x villages (difference from study average)
village_avg_wt_diffs <- rnorm(n_villages,
                              mean = 0,
                              sd = inter_village_sd)

# 64x households (difference from village average)
unique_household_avg_wt_diffs <- rnorm(n_households_per_village * n_villages,
                                       mean = 0,
                                       sd = inter_household_sd)

# 384 household-month trap records
month_household_avg_wt_diffs <- rnorm(6 * n_households_per_village * n_villages,
                                      mean = 0,
                                      sd = inter_month_sd)

# study average + correct village difference + correct household difference +
# correct month difference

nonintervention_log_weights <- log(study_avg_wt) +
  village_avg_wt_diffs[data_template$village] +
  unique_household_avg_wt_diffs[data_template$unique_household] +
  month_household_avg_wt_diffs

# apply convert percentage reduction
intervention_multiplier <- 1 - intervention_percent_reduction / 100

# create a dummy variable for whether there is an effect of the intervention
intervention_in_place <- as.numeric(data_template$before_after == "after" &
                                      data_template$arm == "intervention")

intervention_multiplier_vec <- (1 - intervention_in_place) +
  intervention_in_place * intervention_multiplier

log_weights <- nonintervention_log_weights + log(intervention_multiplier_vec)

weights <- exp(log_weights)

data_template$weight <- weights