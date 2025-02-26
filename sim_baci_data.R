# data simulation code
library(greta)
library(tidyverse)


#Parameters
intervention_percent_reduction <- 27


study_avg_wt <- 2000

inter_village_sd <- 0.3

inter_household_sd <- 0.3

inter_month_sd  <- 0.3

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

# # this is the version that does not end with a probability distribution
# 
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

log_weights_observations <- normal(
  mean = intervention_log_weights_household,
  sd = inter_month_sd
)


m <- model(log_weights_observations)

plot(m )

simulated_log_weights <- calculate(log_weights_observations, nsim = 1)

simulated_weights <- exp(unlist(simulated_log_weights))

simdat <- data_template |>
  mutate(
    weight = simulated_weights
  )


simdat %>%
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

write_csv(
  x = simdat,
  file = "ento_baci_data.csv"
)






