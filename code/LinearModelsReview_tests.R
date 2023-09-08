# List of packages necessary to run this script:
require(librarian, quietly = TRUE)
shelf(tidyverse, performance, quiet = TRUE)

# Load Roberts et al. (2022) data:
bleach <- 
  read.csv("https://github.com/LivingLandscapes/Course_EcologicalModeling/raw/master/data/bleach.csv")

#
Fit_lm <- lm(LOGBLEACH ~ SST * as.factor(MASS), data = bleach)
summary(Fit_lm)
plot(Fit_lm)

#
Fit_binom <- glm(MASS ~ SST, family = binomial, data = bleach)
summary(Fit_binom)

predFit <- 
  data.frame(SST = seq(min(bleach$SST), max(bleach$SST), 0.05),
             as.data.frame(predict(Fit_binom, 
                                   data.frame(SST = seq(min(bleach$SST), max(bleach$SST), 0.05)),
                                   se.fit = TRUE)))
predFit <- 
  predFit %>%
  mutate(predFit_reponse = plogis(fit),
         upr = plogis(fit + qnorm(0.05)*se.fit),
         lwr = plogis(fit + qnorm(0.95)*se.fit))
ggplot() +
  geom_ribbon(data = predFit,
              mapping = aes(x = SST, 
                            ymin = lwr,
                            ymax = upr),
              color = "gray90",
              alpha = 0.6) +
  geom_line(data = predFit,
            mapping = aes(SST, predFit_reponse),
            linewidth = 1.5) +
    xlab("Sea Surface Temperature") +
    ylab("Probability of\nMass Bleaching Event")
  

tst <- 
  data.frame(SST = seq(min(bleach$SST), max(bleach$SST), 0.05),
             as.data.frame(predict(Fit, 
                                   data.frame(SST = seq(min(bleach$SST), max(bleach$SST), 0.05)),
                                   se.fit = TRUE, 
                                   type = "response")))
