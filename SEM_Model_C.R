
library(brms)
library(tidyverse)
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/all.data.rda")


#### Model C   #####
#### Gulf Stream North Wall Index 


################### Model 1 for SEM C
#### eg.abund.C ####
################### This model hypothesizes that right whale abundance is 
################### influenced by population size, patchiness, and regional zpl


## this was run on the clusters
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/eg.abund.C.rda")           
#eg.abund.C.prior <- c(set_prior("cauchy(50, 50)", class = "Intercept"),
#                      set_prior("normal(.32, .16)", class = "b", coef = "pop.est"),
#                      set_prior("normal(8.5, 4.25)", class = "b", coef = "mtz.patchiness"),
#                      set_prior("normal(0, 0.00045)", class = "b", coef = "RegZpl"),
#                      set_prior("normal(42, 42)", class = "b", coef = "CCBStrat"),
#                      set_prior("normal(0.15, 0.07)", class = "b", dpar = "hu", coef = "CCBStrat"),
#                      set_prior("normal(0.025, 0.0125)", class = "b", dpar = "hu", coef = "mtz.patchiness"),
#                      set_prior("normal(0.001, 0.0005)", class = "b", dpar = "hu", coef = "pop.est"))

## in this model "RegZpl" is doing the job of Regional habitat quality
#model_formula = eg.abund ~ CCBStrat + SpringDate + pop.est + mtz.patchiness + RegZpl + (1|Year)
#model_formula_hu = update(model_formula,  hu ~ . )
#model_formula = bf(model_formula, model_formula_hu)

#model_formula = bf(model_formula, model_formula_hu)

#eg.abund.C <- brm(formula = model_formula,
#                  family = "hurdle_gamma", prior = eg.abund.C.prior,
#                  warmup = 1000, inits = 0,
#                  iter = 4000, chains = 2, cores = 2,
#                  control = list(stepsize = 0.01, max_treedepth = 15,
#                                 adapt_delta=0.99), data = all.data)





eg.abund.C.summary <- summary(eg.abund.C, waic = TRUE)
plot(eg.abund.C, waic = TRUE)
pp_check(eg.abund.C)
plot(marginal_effects(eg.abund.C), points = TRUE)
bayes_R2(eg.abund.C)

p <- plot(marginal_effects(eg.abund.C), points = TRUE)

## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef.  the hurdle part needs to be exp. 
fixed_eff = data.frame(eg.abund.C.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`Odds ratio` =c(NA, exp(fixed_eff$Estimate[2]),
                          rep(NA, 5),
                          exp(fixed_eff$Estimate[8:12]))
fixed_eff$`OR low 95%` = c(NA, exp(fixed_eff$`l-95% CI`[2]),
                           rep(NA,5),
                           exp(fixed_eff$`l-95% CI`[8:12]))
fixed_eff$`OR high 95%` = c(NA, exp(fixed_eff$`u-95% CI`[2]),
                            rep(NA,5),
                            exp(fixed_eff$Estimate[8:12]))
fixed_eff$`backtransformed est` = c(1/(fixed_eff$Estimate[1]), 
                                    NA,
                                    1/fixed_eff$Estimate[3:7],
                                    rep(NA, 5))
fixed_eff$`backtransformed low 95%` = c(1/(fixed_eff$`l-95% CI`[1]), 
                                        NA,
                                        1/fixed_eff$`l-95% CI`[3:7],
                                        rep(NA, 5))
fixed_eff$`backtransformed high 95%` = c(1/(fixed_eff$`u-95% CI`[1]), 
                                         NA,
                                         1/fixed_eff$`u-95% CI`[3:7],
                                         rep(NA, 5))

fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`,
                `Odds ratio`, `OR low 95%`, 
                `OR high 95%`, `backtransformed est`, 
                `backtransformed low 95%`,
                `backtransformed high 95%`)
 pander::pander(fixed_eff)

## standardize coefficients after transformation. 
## We now think the non-transformed coefs are what are
## supposed to be standardized
# fixed_eff$`standardized coefficients` =c(NA, NA,
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$CCBStrat)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[4]*sd(all.data$SpringDate)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[5]*sd(all.data$pop.est)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[6]*sd(all.data$mtz.patchiness)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[7]*sd(all.data$RegZpl)/sd(all.data$eg.abund),
#                                          rep(NA, 5))

 ## This standardizes the non-transformed coefs by std
 ## deviations
  fixed_eff$`standardized coefficients` =c(NA, NA,
                                           fixed_eff$`Estimate`[3]*sd(all.data$CCBStrat)/sd(all.data$eg.abund),
                                           fixed_eff$`Estimate`[4]*sd(all.data$SpringDate)/sd(all.data$eg.abund),
                                           fixed_eff$`Estimate`[5]*sd(all.data$pop.est)/sd(all.data$eg.abund),
                                           fixed_eff$`Estimate`[6]*sd(all.data$mtz.patchiness)/sd(all.data$eg.abund),
                                           fixed_eff$`Estimate`[7]*sd(all.data$RegZpl)/sd(all.data$eg.abund),
                                          rep(NA, 5))


## standardize binary coefficients
# Extract predicted values on the link scale
preds <- predict(eg.abund.C, type = "link")

# Compute sd of error variance using theoretical variances
sd.y.LT <- sqrt(var(preds) + pi^2/3)

# Compute sd of x
sd.pop.est <- sd(all.data$pop.est)
sd.patchiness <- sd(all.data$mtz.patchiness)
sd.SpringDate <- sd(all.data$SpringDate)
sd.CCBStrat <- sd(all.data$CCBStrat)
sd.RegZpl <- sd(all.data$RegZpl)

# extract beta coefs
Beta.glm.pop <- fixed_eff[10,1]
Beta.glm.patch <- fixed_eff[11,1]
Beta.glm.springDate <- fixed_eff[9, 1]
Beta.glm.strat <- fixed_eff[8, 1]
Beta.glm.regzpl <- fixed_eff[12, 1]


## range standardized coefficients after transformation
# fixed_eff$`range standardized coefficients` =c(NA, NA,
#                                          fixed_eff$`backtransformed est`[3]*diff(range(all.data$CCBStrat))/diff(range(all.data$eg.abund)),
#                                          fixed_eff$`backtransformed est`[4]*diff(range(all.data$SpringDate))/diff(range(all.data$eg.abund)),
#                                          fixed_eff$`backtransformed est`[5]*diff(range(all.data$pop.est))/diff(range(all.data$eg.abund)),
#                                          fixed_eff$`backtransformed est`[6]*diff(range(all.data$mtz.patchiness))/diff(range(all.data$eg.abund)),
#                                          fixed_eff$`backtransformed est`[7]*diff(range(all.data$RegZpl))/diff(range(all.data$eg.abund)),
#                                          Beta.glm.strat * sd.CCBStrat / sd.y.LT[1],
#                                          Beta.glm.springDate * sd.SpringDate / sd.y.LT[1],
#                                          Beta.glm.pop * sd.pop.est / sd.y.LT[1],
#                                          Beta.glm.patch * sd.patchiness / sd.y.LT[1],
#                                          Beta.glm.regzpl * sd.RegZpl / sd.y.LT[1])

## range standardized coefficients 
## with out transformation
fixed_eff$`range standardized coefficients` =c(NA, NA,
                                         fixed_eff$`Estimate`[3]*diff(range(all.data$CCBStrat))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`Estimate`[4]*diff(range(all.data$SpringDate))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`Estimate`[5]*diff(range(all.data$pop.est))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`Estimate`[6]*diff(range(all.data$mtz.patchiness))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`Estimate`[7]*diff(range(all.data$RegZpl))/diff(range(all.data$eg.abund)),
                                         Beta.glm.strat * sd.CCBStrat / sd.y.LT[1],
                                         Beta.glm.springDate * sd.SpringDate / sd.y.LT[1],
                                         Beta.glm.pop * sd.pop.est / sd.y.LT[1],
                                         Beta.glm.patch * sd.patchiness / sd.y.LT[1],
                                         Beta.glm.regzpl * sd.RegZpl / sd.y.LT[1])





## try plotting with bayesplot
post <- posterior_samples(eg.abund.C)

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
mcmc_areas(
  post[, 3:12], 
  prob = 0.9, # 90% intervals thick parts
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )


################### Model 2 for SEM C
#### Model C MTZ.patchiness ####
################### this model hypothesizes that mtz.patchiness is influenced by
################### stratification, sst anomaly, and caloric value of zpl.

# patchy.C.prior <- c(set_prior("normal(2.8, 1.4)", class = "b", coef = "CCBStrat"),
#                     set_prior("normal()", class = "b", coef = "CaloricVal"))
# 
# patchy.C <- brm(formula = mtz.patchiness ~ SpringDate + CCBStrat + CaloricVal + (1|Year),
#                 family = "gamma", prior = patchy.C.prior,
#                 warmup = 1000, inits = 0,
#                 iter = 2000, chains = 2,
#                 control = list(stepsize = 0.01, max_treedepth = 15,
#                                adapt_delta=0.99), data = all.data)
# 
# 
# patchy.C.summary <- summary(patchy.C, waic = TRUE)
# plot(patchy.C.summary, waic = TRUE)
# pp_check(patchy.C.summary)
# plot(marginal_effects(patchy.C.summary), points = TRUE)
# save(patchy.C.summary, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/patchy.C.rda")
# load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/patchy.C.rda")
#  
## Just in case Stormy isnt' finished with his Caloric values
## stuff in time, we're going to run this model with MTZ in the place
## of caloric values as well

# patchy2.C.prior <- c(set_prior("normal(2.8, 1.4)", class = "b", coef = "CCBStrat"),
#                     set_prior("normal(0.0001, 0.00007)", class = "b", coef = "MTZ"))
## thsi model runs quickly
# patchy2.C <- brm(formula = mtz.patchiness ~ CCBStrat + MTZ + (1|Year),
#                 family = "gamma", prior = patchy2.C.prior,
#                 warmup = 1000, inits = 0,
#                 iter = 2000, chains = 2,
#                 control = list(stepsize = 0.01, max_treedepth = 15,
#                                adapt_delta=0.99), data = all.data)


load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/patchy2.C.rda")
patchy2.C.summary <- summary(patchy2.C, waic = TRUE)
plot(patchy2.C, waic = TRUE)
pp_check(patchy2.C)
plot(marginal_effects(patchy2.C), points = TRUE)
save(patchy2.C, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/patchy2.C.rda")


## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef
fixed_eff = data.frame(patchy2.C.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI` , `u-95% CI`, 
                `backtransformed est`, 
                `backtransformed low 95%`,
                `backtransformed high 95%`)
strat.transformed.table <- pander::pander(fixed_eff)

## standardize coefficients after back transformation
# fixed_eff$`standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*sd(all.data$CCBStrat)/sd(all.data$mtz.patchiness),
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$MTZ)/sd(all.data$mtz.patchiness))
# 

## range standardize coefficients after back transformation
# fixed_eff$`range standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*diff(range(all.data$CCBStrat))/diff(range(all.data$mtz.patchiness)),
#                                          fixed_eff$`backtransformed est`[3]*diff(range(all.data$MTZ))/diff(range(all.data$mtz.patchiness)))
# 

## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA,
                                         fixed_eff$`Estimate`[2]*sd(all.data$CCBStrat)/sd(all.data$mtz.patchiness),
                                         fixed_eff$`Estimate`[3]*sd(all.data$MTZ)/sd(all.data$mtz.patchiness))


## range standardize coefficients 
fixed_eff$`range standardized coefficients` =c(NA, 
                                          fixed_eff$`Estimate`[2]*diff(range(all.data$CCBStrat))/diff(range(all.data$mtz.patchiness)),
                                          fixed_eff$`Estimate`[3]*diff(range(all.data$MTZ))/diff(range(all.data$mtz.patchiness)))
 
 

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate

post <- posterior_samples(patchy2.C)
mcmc_areas(
  post[,2:4], 
  prob = 0.9, # 90% intervals thick parts
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(patchy2.C)


#### Model C MTZ.patchiness w/month ####
 patchy2.C.prior <- c(set_prior("normal(2.8, 1.4)", class = "b", coef = "CCBStrat"),
                     set_prior("normal(0.0001, 0.00007)", class = "b", coef = "MTZ"))

patchy.C.month <- brm(mtz.patchiness ~ Month + CCBStrat + MTZ + (1|Year),
                 family = "gamma", prior = patchy2.C.prior,
                 warmup = 1000, inits = 0,
                 iter = 2000, chains = 2,
                 control = list(stepsize = 0.01, max_treedepth = 15,
                                adapt_delta=0.99), data = all.data)


load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/patchy.C.month.rda")
patchy.C.month.summary <- summary(patchy.C.month, waic = TRUE)
plot(patchy.C.month, waic = TRUE)
pp_check(patchy.C.month)
plot(marginal_effects(patchy.C.month), points = TRUE)
save(patchy.C.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/patchy.C.month.rda")


conditions <- data.frame(Month = c("1", "2", "3", "4", "5"))
p <- plot(marginal_effects(patchy.C.month, 
                           conditions = conditions),
          points = TRUE)

p[[2]] +
  xlab("Stratification") +
  ylab("Zooplankton patchiness")

p[[3]] +
  xlab("Cape Cod Bay Mean Total Zooplankton") +
  ylab("Zooplankton patchiness")

p2 <- plot(marginal_effects(patchy.C.month),
          points = TRUE)

p2[[1]] +
  xlab("Month") +
  ylab("Zooplankton patchiness")

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
post <- posterior_samples(patchy.C.month)
mcmc_areas(
  post, pars = c("b_CCBStrat"), 
  prob = 0.9, # 90% intervals thick parts
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(patchy.C.month)

## This was done with emmeans because we are only 
## trying to get the contrast.  This tells us
## there are differences between the effect of
## some months on patchiness.
patchy.C.month %>% 
  emmeans( ~ Month) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


## This tells us that all months have a (+)
## effect on patchiness.THis was also done iwth 
## emmeans because we are only trying to get the contrast.
## change the HPD to 90% (the default is 95%) to match
## the rest of my analysis
plot(hpd.summary(prob = 90, emmeans(patchy.C.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
patch.em <- emmeans(patchy.C.month, ~ Month)

acf(residuals(patchy.C.month)[,1])



## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

Patchy.C.coefs <- data.frame(patchy.C.month.summary$fixed, check.names = F) %>%
  select(Estimate) %>%
  
  ## first pull out the est.
  ## coefs from the summary table
  ## that don't deal with the contrast
  slice(6:7) %>%
  
  ## exponentiate the coefs
  ## that don't deal with contrasts
  mutate(exp(Estimate))


## this will calculate the coefficients
## when there are contrasts.
Patchy.C.coefs.contrasts <- post %>%
  mutate(Jan = b_Intercept,
         Feb = b_Intercept + b_Month2,
         March = b_Intercept + b_Month3,
         April = b_Intercept + b_Month4,
         May = b_Intercept + b_Month5) %>% 
  as.data.frame() %>%
  summarize(mean.Jan = mean(Jan),
            mean.Feb = mean(Feb),
            mean.March = mean(March),
            mean.April = mean(April),
            mean.May = mean(May)) %>%
  mutate(exp.Jan = exp(mean.Jan),
         exp.Feb = exp(mean.Feb),
         exp.March = exp(mean.March),
         exp.April = exp(mean.April),
         exp.May = exp(mean.May))


################### Model 3 for SEM C
################### this model hypothesizes that CaloricValue is influenced by
################### Regional Zooplankton adn chlorophyll

# CaloricVal.C.prior <- c(set_prior("normal()", class = "Intercept"),
#                         set_prior("normal()", class = "b", coef = "RegZpl"),
#                         set_prior("normal()", class = "b", coef = "chlorophyll"))
# 
# CaloricVal.C <- brm(CaloricVal ~ SpringDate + RegZpl + chlorophyll + (1|Year),
#                     family = "", prior = CaloricVal.C.prior,
#                     warmup = 1000, inits = 0,
#                     iter = 2000, chains = 2,
#                     control = list(stepsize = 0.01, max_treedepth = 15,
#                                    adapt_delta=0.99), data = all.data)
# 
# 
# CaloricVal.C.summary <- summary(CaloricVal.C, waic = TRUE)
# plot(CaloricVal.C.summary, waic = TRUE)
# pp_check(CaloricVal.C.summary)
# plot(marginal_effects(CaloricVal.C.summary), points = TRUE)
# save(CaloricVal.C.summary, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CaloricVal.C.rda")
# load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CaloricVal.C.rda")




## JUst in case Stormy isn't comfortable iwth releasing the caloric
## value data yet I'm also working this model with MTZ
## in the place of Caloric values.

## this was run on the clusters
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/mtz.C.rda")

## THIS MODEL DOESN"T CONVERGE
# mtz.C.prior <- c(set_prior("cauchy(374, 465)", class = "Intercept"),
#                         set_prior("normal(2.5, 1.25)", class = "b", coef = "RegZpl"))
# #                        set_prior("normal(29388, 15000)", class = "b", coef = "chlorophyll"))
# 
# mtz.C <- brm(MTZ ~ SpringDate + RegZpl + chlorophyll (1|Year),
#                    family = "gamma", prior = mtz.C.prior,
#                    warmup = 1000, inits = 0,
#                    iter = 4000, chains = 2,
#                    control = list(stepsize = 0.01, max_treedepth = 15,
#                                   adapt_delta=0.99), data = all.data)


# MTZ.C.summary <- summary(mtz.C, waic = TRUE)
# plot(mtz.C, waic = TRUE)
# pp_check(mtz.C)
# plot(marginal_effects(mtz.C), points = TRUE)
# ##save(mtz.C, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/MTZ.C.rda")
# ##load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/MTZ.C.rda")
# 
# ## try plotting with bayesplot
# post <- posterior_samples(mtz.C)
# 
# ## this plots the posterior distribution with 90% uncertainty intervals
# ## shaded and a line for the mean point estimate
# mcmc_areas(
#   post[, 2:4], 
#   prob = 0.9, # 90% intervals thick parts
#   point_est = "mean"
# ) +
#   ggplot2::labs(
#     title = "Posterior distributions",
#     subtitle = "with means and 90% intervals"
#   )

################### Model 4 for SEM C  *** WE DON"T USE THIS MODEL ***
################### this model hypothesizes that Regional Zpl is influenced by
################### chlorophyll concentration and CCBStrat

#RegZpl.C.prior <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
#                    set_prior("normal(0, 53852)", class = "b", coef = "CCBStrat"),
#                    set_prior("normal(150786, 75393)", class = "b", coef = "chlorophyll"))
## thsi runs quickly
#RegZpl.C <- brm(formula = RegZpl ~ chlorophyll + CCBStrat + (1|Year),
##                family = "gamma", prior = RegZpl.C.prior,
#                warmup = 1000, inits = 0,
#                iter = 2000, chains = 2,
#                control = list(stepsize = 0.01, max_treedepth = 15,
#                               adapt_delta=0.99), data = all.data)


#RegZpl.C.summary <- summary(RegZpl.C, waic = TRUE)
#plot(RegZpl.C, waic = TRUE)
#pp_check(RegZpl.C.summary)
#plot(marginal_effects(RegZpl.C), points = TRUE)
#save(RegZpl.C, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.C.rda")
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.C.rda")

## try plotting with bayesplot
#post <- posterior_samples(RegZpl.C)

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
#mcmc_areas(
#  post[, 2:3], 
#  prob = 0.9, # 90% intervals thick parts
#  point_est = "mean"
#) +
#  ggplot2::labs(
#    title = "Posterior distributions",
#    subtitle = "with means and 90% intervals"
#  )


################### Model 5 for SEM C *** WE DON"T USE THIS MODEL
################### this model hypothesizes that chlorophyll concentration
###################  is influenced by Spring transition date and CCBStrat.

#chl.C.prior <- c(set_prior("normal(2.5, 1.12)", class = "Intercept"),
#                 set_prior("normal(0.23, 0.11)", class = "b", coef = "CCBStrat"),
#                 set_prior("normal(-0.02, 0.01)", class = "b", coef = "SpringDate"))

## this runs quickly
#chl.C <- brm(formula = chlorophyll ~ SpringDate + CCBStrat + (1|Year),
#             family = "gamma", prior = chl.C.prior,
#             warmup = 1000, inits = 0,
#             iter = 2000, chains = 2,
#             control = list(stepsize = 0.01, max_treedepth = 15,
#                            adapt_delta=0.99), data = all.data)


#chl.C.summary <- summary(chl.C, waic = TRUE)
#plot(chl.C, waic = TRUE)
##pp_check(chl.C)
#plot(marginal_effects(chl.C), points = TRUE)
#save(chl.C, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/chl.C.rda")
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/chl.C.rda")


## try plotting with bayesplot
#post <- posterior_samples(chl.C)

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
#mcmc_areas(
##  post[, 2:3], 
#  prob = 0.9, # 90% intervals thick parts
#  point_est = "mean"
#) +
#  ggplot2::labs(
#    title = "Posterior distributions",
#    subtitle = "with means and 90% intervals"
#  )


################### Model 6 for SEM C *** WE don't use this model ****
################### this model hypothesizes that Spring Transistion date
###################  is influenced by sst anomalies and the GUlf Stream North Wall

#SpringDate.C.prior <- c(set_prior("normal(152, 6.3)", class = "Intercept"),
#                        set_prior("normal()", class = "b", coef = "gsnw2yrLag"))

#SpringDate.C <- brm(formula = SpringDate ~ gsnw2yrLag + (1|Year),
#                    family = "normal", prior = SpringDate.C.prior,
#                    warmup = 500, inits = 0,
#                    iter = 1000, chains = 2,
#                    control = list(stepsize = 0.01, max_treedepth = 15,
#                                   adapt_delta=0.99), data = all.data)


#SpringDate.C.summary <- summary(SpringDate.C, waic = TRUE)
#plot(SpringDate.C.summary, waic = TRUE)
#pp_check(SpringDate.C.summary)
#plot(marginal_effects(SpringDate.C.summary), points = TRUE)
#save(SpringDate.C.summary, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/SpringDate.C.rda")
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/SpringDate.C.rda")



################### Model 7 for SEM C **** We don't use this model ****
################### this model hypothesizes that Stratification is influenced
################### by sst anomalies, Gulf Stream North Wall and Spring transition Date

#CCBStrat.C.prior <- c(set_prior("cauchy()", class = "Intercept"),
#                      set_prior("normal()", class = "b", coef = "gsnw2yrLag"),
#                      set_prior("normal()", class = "b", coef = "SpringDate"))

#CCBStrat.C <- brm(formula = CCBStrat ~ SpringDate + gsnw2yrLag + (1|Year),
#                  family = "", prior = CCBStrat.C.prior,
#                  warmup = 1000, inits = 0,
#                  iter = 2000, chains = 2,
#                  control = list(stepsize = 0.01, max_treedepth = 15,
#                                 adapt_delta=0.99), data = all.data)


#CCBStrat.C.summary <- summary(CCBStrat.C, waic = TRUE)
#plot(CCBStrat.C.summary, waic = TRUE)
#pp_check(CCBStrat.C.summary)
#plot(marginal_effects(CCBStrat.C.summary), points = TRUE)
#save(CCBStrat.C.summary, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.C.rda")
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.C.rda")


################### Model 8 for SEM C
################### this model hypothesizes that SST anomalies are influenced
################### by the gulf stream north wall index

#SSTanom.C.prior <- c(set_prior("cauchy()", class = "Intercept"),
#                      set_prior("normal()", class = "b", coef = "gulf.stream.north.wall"))


#SSTanom.C <- brm(formula = SSTanom ~ gsnw2yrLag + (1|Year),
#                  family = "", prior = SSTanom.C.prior,
#                  warmup = 1000, inits = 0,
#                  iter = 2000, chains = 2,
#                  control = list(stepsize = 0.01, max_treedepth = 15,
#                                 adapt_delta=0.99), data = all.data)


#SSTanom.C.summary <- summary(SSTanom.C, waic = TRUE)
#plot(SSTanom.C.summary, waic = TRUE)
#pp_check(SSTanom.C.summary)
#plot(marginal_effects(SSTanom.C.summary), points = TRUE)
##save(SSTanom.C.summary, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/SSTanom.C.rda")
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/SSTanom.C.rda")



################### Model 9 for SEM C
################### this model hypothesizes that the Gulf Stream North wall with a 2 yr lag
################### is influenced by the NAO with a 4 yr lag

## ran on clusters
## this has "C2" in the title but that was a type-o
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/gsnw.C2.2yrlag.rda")


#gsnw.C.prior <- c(set_prior("normal(-0.14, 0.86)", class = "Intercept"),
#                  set_prior("normal(0.12, 0.06)", class = "b", coef = "NAO4yrlag"))

## this doesn't even come close to converging at warumup = 1000
## and iterations =4000 with treedepth 20.  Up it all!
#gsnw.C.2yrlag <- brm(formula = gsnw2yrLag ~ NAO4yrlag + (1|Year),
#                     family = "normal", prior = gsnw.C.prior,
#                     warmup = 1000, inits = 0,
#                     iter = 8000, chains = 4, cores = 4,
#                     control = list(stepsize = 0.01, max_treedepth = 25,
#                                    adapt_delta=0.99), data = all.data)


#gsnw.C.summary <- summary(gsnw.C.2yrlag, waic = TRUE)
#plot(gsnw.C.2yrlag, waic = TRUE)
#pp_check(gsnw.C.2yrlag)
#plot(marginal_effects(gsnw.C.2yrlag), points = TRUE)






#### Model C2   ####
######################## Change the time lags from model C 
####################### and RegZpl is influenced by NAO 2yrlag 

#### Model MTZ.C2 ####
## we never got model MTZ.C to converge.  So we tried this instead, which removes
## the influence of chlorophyll

## we ran this on the clusters this does not converge
## but tries to est the intercept
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/mtz.C2.rda")

#mtz.C2.prior <- c(#set_prior("cauchy(374, 465)", class = "Intercept"),
 #                 set_prior("normal(2.5, 1.25)", class = "b", coef = "RegZpl"))
# 
# mtz.C2 <- brm(MTZ ~ SpringDate + RegZpl + (1|Year),
#               family = "gamma", prior = mtz.C2.prior,
#               warmup = 1000, inits = 0,
#               iter = 10000, chains = 2,
#               control = list(stepsize = 0.01, max_treedepth = 20,
#                              adapt_delta=0.99), data = all.data)

## Solange suggested to set the intercept to 0 since
## we can't get the above model to converge.
mtz.C2 <- brm(MTZ ~ 0 + SpringDate + RegZpl + (1|Year),
              family = "gamma", prior = mtz.C2.prior,
              warmup = 1000, inits = 0,
              iter = 4000, chains = 2,
              control = list(stepsize = 0.01, max_treedepth = 20,
                             adapt_delta=0.99), data = all.data)

mtz.C2.summary <- summary(mtz.C2, waic = TRUE)
plot(mtz.C2, waic = TRUE)
pp_check(mtz.C2)
plot(marginal_effects(mtz.C2), points = TRUE)
bayes_R2(mtz.C2)
save(mtz.C2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/mtz.C2.noint.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/mtz.C2.noint.rda")


## try plotting with bayesplot
post <- posterior_samples(mtz.C2)

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
mcmc_areas(post,
           regex_pars= "b_RegZpl",
  prob = 0.9, # 90% intervals thick parts
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

mtz.C2.coefs <- data.frame(mtz.C2.summary$fixed, check.names = F) %>%
  select(Estimate) %>%
  
  ## exponentiate the coefs
  ## that don't deal with contrasts
  mutate(exp(Estimate))

################### Model C2 CCBStrat ####
################### this model hypothesizes that Stratification is influenced
################### by sst anomalies, Gulf Stream North Wall and Spring transition Date

CCBStrat.C2.prior <- c(set_prior("cauchy(0.19, 0.03)", class = "Intercept"),
                       set_prior("normal(-0.75, 0.37)", class = "b", coef = "gulf.stream.north.wall"),
                       set_prior("normal(-0.06, 0.03)", class = "b", coef = "SpringDate"))

CCBStrat.C2 <- brm(formula = CCBStrat ~ SpringDate + gulf.stream.north.wall + (1|Year),
                   family = "gamma", prior = CCBStrat.C2.prior,
                   warmup = 1000, inits = 0,
                   iter = 4000, chains = 2,
                   control = list(stepsize = 0.01, max_treedepth = 15,
                                  adapt_delta=0.99), data = all.data)

load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.C2.rda")
CCBStrat.C2.summary <- summary(CCBStrat.C2, waic = TRUE)
plot(CCBStrat.C2, waic = TRUE)
pp_check(CCBStrat.C2)
plot(marginal_effects(CCBStrat.C2), points = TRUE)
save(CCBStrat.C2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.C2.rda")
bayes_R2(CCBStrat.C2)

## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef
fixed_eff = data.frame(CCBStrat.C2.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`,
                `backtransformed est`, `backtransformed low 95%`,
                `backtransformed high 95%`)
strat.transformed.table <- pander::pander(fixed_eff)

# ## standardize coefficients after back transforming
# fixed_eff$`standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*sd(all.data$SpringDate)/sd(all.data$CCBStrat),
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$gulf.stream.north.wall)/sd(all.data$CCBStrat))
# 
# ## range standardize coefficients after back transforming
# fixed_eff$`range standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*diff(range(all.data$SpringDate))/diff(range(all.data$CCBStrat)),
#                                          fixed_eff$`backtransformed est`[3]*diff(range(all.data$gulf.stream.north.wall))/diff(range(all.data$CCBStrat)))
# 


## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA, 
                                         fixed_eff$`Estimate`[2]*sd(all.data$SpringDate)/sd(all.data$CCBStrat),
                                         fixed_eff$`Estimate`[3]*sd(all.data$gulf.stream.north.wall)/sd(all.data$CCBStrat))

## range standardize coefficients 
fixed_eff$`range standardized coefficients` =c(NA, 
                                               fixed_eff$`Estimate`[2]*diff(range(all.data$SpringDate))/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[3]*diff(range(all.data$gulf.stream.north.wall))/diff(range(all.data$CCBStrat)))


## try plotting with bayesplot
post <- posterior_samples(CCBStrat.C2)

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
mcmc_areas(
  post[, 2:3], 
  prob = 0.9, # 90% intervals thick parts
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )



################### Model C2 CCBStrat w/month ####
CCBStrat.C2.prior <- c(set_prior("cauchy(0.19, 0.03)", class = "Intercept"),
                       set_prior("normal(-0.75, 0.37)", class = "b", coef = "gulf.stream.north.wall"),
                       set_prior("normal(-0.06, 0.03)", class = "b", coef = "SpringDate"))

CCBStrat.C2.month <- brm(CCBStrat ~ Month + SpringDate + gulf.stream.north.wall + (1|Year),
                   family = "gamma", prior = CCBStrat.C2.prior,
                   warmup = 1000, inits = 0,
                   iter = 4000, chains = 2,
                   control = list(stepsize = 0.01, max_treedepth = 15,
                                  adapt_delta=0.99), data = all.data)

load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.C2.month.rda")
CCBStrat.C2.summary <- summary(CCBStrat.C2.month, waic = TRUE)
plot(CCBStrat.C2.month, waic = TRUE)
pp_check(CCBStrat.C2.month)
save(CCBStrat.C2.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.C2.month.rda")
bayes_R2(CCBStrat.C2.month)

p <- plot(marginal_effects(CCBStrat.C2.month, 
                      conditions = conditions), points = TRUE)

p[[2]] +
  xlab("Spring Transition Date") +
  ylab("Stratification")

p[[3]] +
  xlab("Gulf Stream North Wall Index") +
  ylab("Stratification")

p2 <- plot(marginal_effects(CCBStrat.C2.month), points = TRUE)
p2[[1]] +
  ylab("Stratification")


## This was done with emmeans because we are only 
## trying to get the contrast.  This tells us
## there are differences between the effect of
## some months on Stratification.
CCBStrat.C2.month %>% 
  emmeans( ~ Month) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

## This tells us that all months have a negative
## effect on stratification.THis was also done iwth 
## emmeans because we are only trying to get the contrast.
## change the HPD to 90% (the default is 95%) to match
## the rest of my analysis
plot(hpd.summary(prob = 90, emmeans(CCBStrat.C2.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
strat.em <- emmeans(CCBStrat.C2.month, ~ Month)


## try plotting with bayesplot
post <- posterior_samples(CCBStrat.C2.month)

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
mcmc_areas(
  post[, 6:7], 
  prob = 0.9, # 90% intervals thick parts
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

acf(residuals(CCBStrat.C2.month)[,1])



## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

CCBStrat.C2.coefs <- data.frame(CCBStrat.C2.summary$fixed, check.names = F) %>%
  select(Estimate) %>%
  
  ## first pull out the est.
  ## coefs from the summary table
  ## that don't deal with the contrast
  slice(6:7) %>%
  
  ## exponentiate the coefs
  ## that don't deal with contrasts
  mutate(exp(Estimate))


## this will calculate the coefficients
## when there are contrasts.
CCBStrat.C2.coefs.contrasts <- post %>%
  mutate(Jan = b_Intercept,
         Feb = b_Intercept + b_Month2,
         March = b_Intercept + b_Month3,
         April = b_Intercept + b_Month4,
         May = b_Intercept + b_Month5) %>% 
  as.data.frame() %>%
  summarize(mean.Jan = mean(Jan),
            mean.Feb = mean(Feb),
            mean.March = mean(March),
            mean.April = mean(April),
            mean.May = mean(May)) %>%
  mutate(exp.Jan = exp(mean.Jan),
         exp.Feb = exp(mean.Feb),
         exp.March = exp(mean.March),
         exp.April = exp(mean.April),
         exp.May = exp(mean.May))


################### Model C2 *** WE DON"T USE THIS MODEL
################### this model hypothesizes that Spring Transistion date
###################  is influenced by the GUlf Stream North Wall

## THis ran on the clusters
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/Spring.Date.C2.rda")


#SpringDate.C2.prior <- c(set_prior("normal(152, 6.3)", class = "Intercept"),
#                        set_prior("normal(-9.05, 2.25)", class = "b", coef = "gulf.stream.north.wall"))

## this did not converge with 8000 iterations, so I made the prior for gsnw stronger
## by cutting the standard deviation from 4.5 to 2.25 and upped the iterations
## gsnw rhat = 1822
#SpringDate.C2 <- brm(formula = SpringDate ~ gulf.stream.north.wall + (1|Year),
#                    family = "normal", prior = SpringDate.C2.prior,
#                    warmup = 1000, inits = 0,
#                    iter = 20000, chains = 2,
#                    control = list(stepsize = 0.01, max_treedepth = 15,
#                                   adapt_delta=0.999), data = all.data)


#SpringDate.C2.summary <- summary(SpringDate.C2, waic = TRUE)
#plot(SpringDate.C2, waic = TRUE)
#pp_check(SpringDate.C2)
#plot(marginal_effects(SpringDate.C2), points = TRUE)

## This is the same model but with wider priors
## this still doesn't fit.  got worse with wider priors. gsnw rhat = 5614
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/Spring.Date.C2.new.prior.rda")
#summary(SpringDate.C2.new.prior)
#plot(SpringDate.C2.new.prior)
#plot(marginal_effects(SpringDate.C2.new.prior))


## This is the same model but with springdate and gsnw centered
## with flat priors the rhat is 1681 which is worse than when I set wide priors
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/Spring.Date.c.C2.rda")
#summary(SpringDate.c.C2)
#plot(SpringDate.c.C2)
#plot(marginal_effects(SpringDate.c.C2))

################### Model C2  ** WE DON"T USE THIS MODEL ***
################### this model hypothesizes that the Gulf Stream North wall 
################### is influenced by the NAO with a 2 yr lag

## This model was run ont he clusters
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/gsnw.C2.rda")


#gsnw.C2.prior <- c(set_prior("normal(-0.14, 0.86)", class = "Intercept"),
#                  set_prior("normal(0.12, 0.06)", class = "b", coef = "NAO2yrlag"))

## this doesn't even come close to converging at warumup = 1000
## and iterations =6000 with treedepth 20.  Up it all!
#gsnw.C2 <- brm(formula = gulf.stream.north.wall ~ NAO2yrlag + (1|Year),
#                     family = "normal", prior = gsnw.C2.prior,
#                     warmup = 1000, inits = 0,
#                     iter = 6000, chains = 2, cores = 2,
#                     control = list(stepsize = 0.01, max_treedepth = 25,
#                                    adapt_delta=0.99), data = all.data)


#gsnw.C2.summary <- summary(gsnw.C2, waic = TRUE)
#plot(gsnw.C2, waic = TRUE)
#pp_check(gsnw.C2)
#plot(marginal_effects(gsnw.C2), points = TRUE)
#save(gsnw.C2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/gsnw.C2.rda")




################### Model C2 RegZpl ####
################### this model hypothesizes that RegZpl is influenced
################### by CCBStrat, chlorophyll, and NAO2yrlag


regzpl.C2.prior <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
                     set_prior("normal(18615, 9307.5)", class = "b", coef = "NAO2yrlag"),
                     set_prior("normal(0, 53852)", class = "b", coef = "CCBStrat"),
                     set_prior("normal(150786, 75393)", class = "b", coef = "chlorophyll"))

## this model runs quickly :)
RegZpl.C2 <- brm(formula = RegZpl ~ CCBStrat + chlorophyll + NAO2yrlag + (1|Year),
                 family = "gamma", prior = regzpl.C2.prior,
                 warmup = 1000, inits = 0,
                 iter = 4000, chains = 4, cores = 4,
                 control = list(stepsize = 0.01, max_treedepth = 15,
                                adapt_delta=0.99), data = all.data)


RegZpl.C2.summary <- summary(RegZpl.C2, waic = TRUE)
plot(RegZpl.C2, waic = TRUE)
pp_check(RegZpl.C2)
plot(marginal_effects(RegZpl.C2), points = TRUE)
save(RegZpl.C2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.C2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.C2.rda")
bayes_R2(RegZpl.C2)


## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef
fixed_eff = data.frame(RegZpl.C2.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
                `backtransformed est`, `backtransformed low 95%`,
                `backtransformed high 95%`)
strat.transformed.table <- pander::pander(fixed_eff)

# ## standardize coefficients after backtransforming
# fixed_eff$`standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*sd(all.data$CCBStrat)/sd(all.data$RegZpl),
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$chlorophyll)/sd(all.data$RegZpl),
#                                          fixed_eff$`backtransformed est`[4]*sd(all.data$NAO2yrlag)/sd(all.data$RegZpl))
# 
# ## range standardize coefficients after backtransforming
# fixed_eff$`range standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*diff(range(all.data$CCBStrat))/diff(range(all.data$RegZpl)),
#                                          fixed_eff$`backtransformed est`[3]*diff(range(all.data$chlorophyll))/diff(range(all.data$RegZpl)),
#                                          fixed_eff$`backtransformed est`[4]*diff(range(all.data$NAO2yrlag))/diff(range(all.data$RegZpl)))


## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA, 
                                         fixed_eff$`Estimate`[2]*sd(all.data$CCBStrat)/sd(all.data$RegZpl),
                                         fixed_eff$`Estimate`[3]*sd(all.data$chlorophyll)/sd(all.data$RegZpl),
                                         fixed_eff$`Estimate`[4]*sd(all.data$NAO2yrlag)/sd(all.data$RegZpl))

## range standardize coefficients 
fixed_eff$`range standardized coefficients` =c(NA, 
                                               fixed_eff$`Estimate`[2]*diff(range(all.data$CCBStrat))/diff(range(all.data$RegZpl)),
                                               fixed_eff$`Estimate`[3]*diff(range(all.data$chlorophyll))/diff(range(all.data$RegZpl)),
                                               fixed_eff$`Estimate`[4]*diff(range(all.data$NAO2yrlag))/diff(range(all.data$RegZpl)))


## try plotting with bayesplot
post <- posterior_samples(RegZpl.C2)


mcmc_areas(
  post[, 2:4], 
  prob = 0.9, # 80% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )


################### Model C2 RegZpl w/month ####

regzpl.C2.prior <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
                     set_prior("normal(18615, 9307.5)", class = "b", coef = "NAO2yrlag"),
                     set_prior("normal(0, 53852)", class = "b", coef = "CCBStrat"),
                     set_prior("normal(150786, 75393)", class = "b", coef = "chlorophyll"))

## this model runs quickly :)
RegZpl.C2.month <- brm(formula = RegZpl ~ Month + CCBStrat + chlorophyll + NAO2yrlag + (1|Year),
                 family = "gamma", prior = regzpl.C2.prior,
                 warmup = 1000, inits = 0,
                 iter = 4000, chains = 4, cores = 4,
                 control = list(stepsize = 0.01, max_treedepth = 15,
                                adapt_delta=0.99), data = all.data)


RegZpl.C2.summary <- summary(RegZpl.C2.month, waic = TRUE)
plot(RegZpl.C2.month, waic = TRUE)
pp_check(RegZpl.C2.month)
conditions <- data.frame(Month = c("1", "2", "3", "4", "5"))
p <- plot(marginal_effects(RegZpl.C2.month, conditions = conditions),
     points = TRUE)
save(RegZpl.C2.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.C2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.C2.month.rda")
bayes_R2(RegZpl.C2.month)


p[[3]] +
  xlab("Regional chlorophyll") +
  ylab("Regional Calanus finmarchicus")

p[[4]] +
  xlab("NAO 2 yr lag") +
  ylab("Regional Calanus finmarchicus")

p1 <- plot(marginal_effects(RegZpl.C2.month),
          points = TRUE)

p1[[1]] +
  ylab("Regional Calanus finmarchicus")
## This was done with emmeans because we are only 
## trying to get the contrast.  This tells us
## there are differences between the effect of
## some months on RegZpl.
RegZpl.C2.month %>% 
  emmeans( ~ Month) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


## This tells us that all months have a negative
## effect on RegZpl. This was also done iwth 
## emmeans because we are only trying to get the contrast.
## change the HPD to 90% (the default is 95%) to match
## the rest of my analysis
plot(hpd.summary(prob = 90, emmeans(RegZpl.C2.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
regzpl.em <- emmeans(RegZpl.C2.month, ~ Month)

## try plotting with bayesplot
post <- posterior_samples(RegZpl.C2.month)


mcmc_areas(
  post[, 6:8], 
  prob = 0.9, # 80% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

acf(residuals(RegZpl.C2.month)[,1])

## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

RegZpl.C2.coefs <- data.frame(RegZpl.C2.summary$fixed, check.names = F) %>%
  select(Estimate) %>%
  
  ## first pull out the est.
  ## coefs from the summary table
  ## that don't deal with the contrast
  slice(6:8) %>%
  
  ## exponentiate the coefs
  ## that don't deal with contrasts
  mutate(exp(Estimate))

## this will calculate the coefficients
## when there are contrasts.
RegZpl.C2.coefs.contrasts <- post %>%
  mutate(Jan = b_Intercept,
         Feb = b_Intercept + b_Month2,
         March = b_Intercept + b_Month3,
         April = b_Intercept + b_Month4,
         May = b_Intercept + b_Month5) %>% 
  as.data.frame() %>%
  summarize(mean.Jan = mean(Jan),
            mean.Feb = mean(Feb),
            mean.March = mean(March),
            mean.April = mean(April),
            mean.May = mean(May)) %>%
  mutate(exp.Jan = exp(mean.Jan),
         exp.Feb = exp(mean.Feb),
         exp.March = exp(mean.March),
         exp.April = exp(mean.April),
         exp.May = exp(mean.May))



################### Model C2 CHL ####
################### this model hypothesizes that Chlorophyll is influenced
################### by CCBStrat, SpringDate, and NAO2yrlag


chl.C2.prior <- c(set_prior("normal(2.5, 1.12)", class = "Intercept"),
                  set_prior("normal(0.23, 0.11)", class = "b", coef = "CCBStrat"),
                  set_prior("normal(-0.02, 0.01)", class = "b", coef = "SpringDate"))

## this runs quickly
chl.C2 <- brm(formula = chlorophyll ~ NAO2yrlag + SpringDate + CCBStrat + (1|Year),
              family = "gamma", prior = chl.C2.prior,
              warmup = 1000, inits = 0,
              iter = 4000, chains = 2,
              control = list(stepsize = 0.01, max_treedepth = 15,
                             adapt_delta=0.99), data = all.data)


chl.C2.summary <- summary(chl.C2, waic = TRUE)
plot(chl.C2, waic = TRUE)
pp_check(chl.C2)
plot(marginal_effects(chl.C2), points = TRUE)
save(chl.C2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/chl.C2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/chl.C2.rda")
bayes_R2(chl.C2)

## try changing the g in gamma from lower case to capital
## to determine if that changes the default link from log
## to inverse.  This did not make a difference.
# chl.C3 <- brm(formula = chlorophyll ~ NAO2yrlag + SpringDate + CCBStrat + (1|Year),
#               family = "Gamma", prior = chl.C2.prior,
#               warmup = 1000, inits = 0,
#               iter = 4000, chains = 2,
#               control = list(stepsize = 0.01, max_treedepth = 15,
#                              adapt_delta=0.99), data = all.data)






## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef
fixed_eff = data.frame(chl.C2.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
                `backtransformed est`, `backtransformed low 95%`,
                `backtransformed high 95%`)
strat.transformed.table <- pander::pander(fixed_eff)

## standardize coefficients after back transforming
# fixed_eff$`standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*sd(all.data$NAO2yrlag)/sd(all.data$chlorophyll),
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$SpringDate)/sd(all.data$chlorophyll),
#                                          fixed_eff$`backtransformed est`[4]*sd(all.data$CCBStrat)/sd(all.data$chlorophyll))
# 
 ## range standardize coefficients after backtrnasforming
# fixed_eff$`range standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*diff(range(all.data$NAO2yrlag))/diff(range(all.data$chlorophyll)),
#                                          fixed_eff$`backtransformed est`[3]*diff(range(all.data$SpringDate))/diff(range(all.data$chlorophyll)),
#                                          fixed_eff$`backtransformed est`[4]*diff(range(all.data$CCBStrat))/diff(range(all.data$chlorophyll)))
# 

## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA, 
                                         fixed_eff$`Estimate`[2]*sd(all.data$NAO2yrlag)/sd(all.data$chlorophyll),
                                         fixed_eff$`Estimate`[3]*sd(all.data$SpringDate)/sd(all.data$chlorophyll),
                                         fixed_eff$`Estimate`[4]*sd(all.data$CCBStrat)/sd(all.data$chlorophyll))

## range standardize coefficients 
fixed_eff$`range standardized coefficients` =c(NA, 
                                               fixed_eff$`Estimate`[2]*diff(range(all.data$NAO2yrlag))/diff(range(all.data$chlorophyll)),
                                               fixed_eff$`Estimate`[3]*diff(range(all.data$SpringDate))/diff(range(all.data$chlorophyll)),
                                               fixed_eff$`Estimate`[4]*diff(range(all.data$CCBStrat))/diff(range(all.data$chlorophyll)))




## try plotting with bayesplot
post <- posterior_samples(chl.C2)

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
mcmc_areas(
  post[, 2:4], 
  prob = 0.9, # 90% intervals thick parts
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

#### Model C2 CHL w/month ####
chl.C2.prior <- c(set_prior("normal(2.5, 1.12)", class = "Intercept"),
                  set_prior("normal(0.23, 0.11)", class = "b", coef = "CCBStrat"),
                  set_prior("normal(-0.02, 0.01)", class = "b", coef = "SpringDate"))

## this runs quickly
chl.C2.month <- brm(chlorophyll ~ Month + NAO2yrlag + SpringDate + CCBStrat + (1|Year),
              family = "gamma", prior = chl.C2.prior,
              warmup = 1000, inits = 0,
              iter = 4000, chains = 2,
              control = list(stepsize = 0.01, max_treedepth = 15,
                             adapt_delta=0.99), data = all.data)


chl.C2.summary <- summary(chl.C2.month, waic = TRUE)
plot(chl.C2.month, waic = TRUE)
pp_check(chl.C2.month)
conditions <- data.frame(Month = c("1", "2", "3", "4", "5"))
p <- plot(marginal_effects(chl.C2.month, conditions = conditions), points = TRUE)
save(chl.C2.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/chl.C2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/chl.C2.month.rda")
bayes_R2(chl.C2.month)

p[[1]] +
  ylab("Regional chlorophyll")

p[[2]] +
  xlab("NAO 2 yr lag") +
  ylab("Regional chlorophyll")

p[[3]] +
  xlab("Spring Transition Date") +
  ylab("Regional chlorophyll")

p[[4]] +
  xlab("Stratification") +
  ylab("Regional chlorophyll")

## This was done with emmeans because we are only 
## trying to get the contrast.  This tells us
## there are differences between the effect of
## some months on chl.
chl.C2.month %>% 
  emmeans( ~ Month) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


## This tells us that some months have a negative
## effect on CHL.THis was also done iwth 
## emmeans because we are only trying to get the contrast.
## change the HPD to 90% (the default is 95%) to match
## the rest of my analysis
plot(hpd.summary(prob = 90, emmeans(chl.C2.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
strat.em <- emmeans(chl.C2.month, ~ Month)


## try plotting with bayesplot
post <- posterior_samples(chl.C2.month)

## this plots the posterior distribution with 90% uncertainty intervals
## shaded and a line for the mean point estimate
mcmc_areas(
  post, pars = c("b_CCBStrat"), 
  prob = 0.9, # 90% intervals thick parts
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

acf(residuals(chl.C2.month)[,1])


## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

chl.C2.coefs <- data.frame(chl.C2.summary$fixed, check.names = F) %>%
  select(Estimate) %>%
  
  ## first pull out the est.
  ## coefs from the summary table
  ## that don't deal with the contrast
  slice(6:8) %>%
  
  ## exponentiate the coefs
  ## that don't deal with contrasts
  mutate(exp(Estimate))


## this will calculate the coefficients
## when there are contrasts.
chl.C2.coefs.contrasts <- post %>%
  mutate(Jan = b_Intercept,
         Feb = b_Intercept + b_Month2,
         March = b_Intercept + b_Month3,
         April = b_Intercept + b_Month4,
         May = b_Intercept + b_Month5) %>% 
  as.data.frame() %>%
  summarize(mean.Jan = mean(Jan),
            mean.Feb = mean(Feb),
            mean.March = mean(March),
            mean.April = mean(April),
            mean.May = mean(May)) %>%
  mutate(exp.Jan = exp(mean.Jan),
         exp.Feb = exp(mean.Feb),
         exp.March = exp(mean.March),
         exp.April = exp(mean.April),
         exp.May = exp(mean.May))
