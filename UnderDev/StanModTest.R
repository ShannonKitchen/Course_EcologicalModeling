# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, cowplot, palmerpenguins, performance, bslib, shiny,
      ggExtra, here, rstan, bayesplot, loo, brms,
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

#====================================================
## brms
scaleFun <- 
  function(x) {
    (x - mean(x)) / sd(x)
  }

penguins <- 
  penguins %>%
  mutate(flipper_len_scaled = scaleFun(flipper_length_mm),
         bill_len_scaled = scaleFun(bill_length_mm),
         bill_len_scaled_2 = bill_len_scaled^2,
         flipper_len_scaled_2 = flipper_len_scaled^2)

# Fit with brms
fit_brms <- 
  brm(formula = body_mass_g ~ flipper_len_scaled + flipper_len_scaled_2 + bill_len_scaled + bill_len_scaled_2 + (species | sex),
      data = penguins, 
      family = lognormal(),
      iter = 4000, 
      warmup = 2000,
      chains = 3,
      seed = 4405,
      # threads = 3,
      cores = 5,
      prior = c(set_prior("normal(0, 10)", class = "b", coef = "flipper_len_scaled"),
                set_prior("normal(0, 10)", class = "b", coef = "bill_len_scaled")
                # ,
                # set_prior("normal(0, 100)", class = "sd", coef = "sexfemale")
      )
  )
fit_brms
# Predict
nd <- 
  with(penguins, 
       expand.grid(flipper_len_scaled = (seq(min(flipper_length_mm), max(flipper_length_mm), 10) - mean(flipper_length_mm)) / sd(flipper_length_mm),
                   # bill_len_scaled = (seq(min(bill_length_mm), max(bill_length_mm), 10) - mean(bill_length_mm)) / sd(bill_length_mm),
                   bill_len_scaled = 0,
                   bill_len_scaled_2 = 0,
                   species = unique(species),
                   sex = unique(sex))
  ) %>%
  mutate(flipper_len_scaled_2 = flipper_len_scaled^2)
nd0 <- 
  with(penguins, 
       expand.grid(flipper_length_mm = seq(min(flipper_length_mm), max(flipper_length_mm), 10))
  )
pred_brms <- 
  predict(fit_brms,
          newdata = nd,
          cores = 5,
          probs = c(0.025, 0.10, 0.90, 0.975))
pred_df <- 
  cbind(pred_brms, nd, nd0 %>% dplyr::select(c("flipper_length_mm")))
ggplot() +
  geom_point(data = penguins, 
             aes(x = flipper_length_mm, y = body_mass_g, color = sex),
             alpha = 0.5) +
  geom_ribbon(data = pred_df,
              mapping = aes(x = flipper_length_mm, 
                            y = Estimate, 
                            ymin = Q10,
                            ymax = Q90,
                            fill = sex, 
                            color = species),
              alpha = 0.3) +
  geom_line(data = pred_df,
            mapping = aes(x = flipper_length_mm, y = Estimate, color = sex, linetype = species),
            linewidth = 1.5)
