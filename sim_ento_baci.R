library(tidyverse)

n_villages <- 8
n_households_per_village <- 8

# Create a tibble with the structure of the sampling set up, and an empty column
# for the weight of the mosquitoes collected. Each row is one sampling unit: one
# trapping effort, over the course of a month, in a single household.
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

view(data_template)

# what parameters do we need?

# intervention effect (% reduction)
intervention_percent_reduction <- 27 # percent

# WITHOUT intervention effect:
# study average weight (one month total, in one household)
study_avg_wt <- 4000 #mg
inter_village_sd <- 500
inter_household_sd <- 300
inter_month_sd <- 500

# # we could do the simulations with for-loops
# 
# # all 8 villages first
# village_avg_wt <- rnorm(n_villages,
#                         mean = study_avg_wt,
#                         sd = inter_village_sd)
# 
# # then loop through the villages, each time generating 8 households
# 
# # e.g. for the first village:
# household_village1_avg_wt <- rnorm(n_households_per_village,
#                                    mean = village_avg_wt[1],
#                                    sd = inter_household_sd)
# 
# # then loop through the households, each time generating 6 months
# 
# # e.g. for the first village, first household:
# month_household1_village1_wt <- rnorm(6,
#                                       mean = household_village1_avg_wt[1],
#                                       sd = inter_month_sd)

# or we can simulate them all at once, with vectorised code. This makes use of
# the fact that we can simulate zero-mean random normals, and then add on the
# mean. It also makes use of integer indexing in R.

# integer indexing:

# names <- c("John", "Vic", "Sylviane")
# indices <- c(1, 1, 1, 2, 2, 3, 3, 3, 3)
# names[indices]

# set up the random difference values (ignoring intervention effects)

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

nonintervention_weights <- study_avg_wt +
  village_avg_wt_diffs[data_template$village] +
  unique_household_avg_wt_diffs[data_template$unique_household] +
  month_household_avg_wt_diffs

hist(nonintervention_weights)

# apply convert percentage reduction
intervention_multiplier <- 1 - intervention_percent_reduction / 100

# create a dummy variable for whether there is an effect of the intervention
intervention_in_place <- as.numeric(data_template$before_after == "after" &
                                      data_template$arm == "intervention")

intervention_multiplier_vec <- (1 - intervention_in_place) +
  intervention_in_place * intervention_multiplier

weights <- nonintervention_weights * intervention_multiplier_vec

data_template$weight <- weights

view(data_template)

data_template %>%
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
