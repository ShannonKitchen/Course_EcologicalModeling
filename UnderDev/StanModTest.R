# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, cowplot, palmerpenguins, performance, bslib, shiny,
      ggExtra, here, rstan, bayesplot, loo,
      quiet = TRUE)

# # Here is the link to the R Package with the penguins data:
# https://allisonhorst.github.io/palmerpenguins/

# Read in a data file
data(package = 'palmerpenguins', verbose = FALSE)

# Make list for stan
penguins <- na.omit(penguins)
peng_dat <- 
  list(N = nrow(penguins),
       y = penguins$body_mass_g,
       n_island = length(unique(as.numeric(as.factor(penguins$island)))),
       island = as.numeric(as.factor(penguins$island)),
       n_species = length(unique(as.numeric(as.factor(penguins$species)))),
       species = as.numeric(as.factor(penguins$species)),
       n_sex = 2,
       sex = as.numeric(as.factor(penguins$sex)),
       billLength = with(penguins, (bill_length_mm - mean(bill_length_mm)) / sd(bill_length_mm)),
       flipperLength = with(penguins, (flipper_length_mm - mean(flipper_length_mm)) / sd(flipper_length_mm))
       )
# Stan fit
fit <- stan(file = here("UnderDev/Penguin.stan"),
            model_name = "PenguinTest",
            data = peng_dat,
            chains = 3,
            iter = 4000,
            cores = 3)
print(fit)

# 
plot(fit)

# 
set.seed(1856)
yrep1 <- extract(fit)[[""]]
samp100 <- sample(nrow(yrep1), 100)
ppc_dens_overlay(y, yrep1[samp100, ])  