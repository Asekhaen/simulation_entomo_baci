# greta intro

x <- iris$Petal.Length
y <- iris$Sepal.Length
library(greta)

int <- normal(0, 5)
coef <- normal(0, 3)
sd <- lognormal(0, 3)

mean <- int + coef * x


# prior simulation
sims <- calculate(mean[1], nsim = 1000)
hist(sims$`mean[1]`)



distribution(y) <- normal(mean, sd)

m <- model(int, coef, sd)

plot(m)

draws <- mcmc(m, n_samples = 1000, chains = 4)

bayesplot::mcmc_trace(draws)

library(bayesplot)
mcmc_intervals(draws)

mcmc_dens_chains(draws)
