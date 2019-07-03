
library(brms)
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/all.data.rda")

########################           MODEL B           ##############################
################### Model 1 for SEM B
################### Original hypothesis: right whale abundance is directly influenced by 
################### the size of the right whale population, Calanus finmarchicus, Pseudocalanus,
################### and Centropages monthly mean abundance in CCB, the interaction between 
################### Calanus and CCB stratification and a random effect of year. 
################### The D-sep test indicated that regional SST was also a direct influencer of
################### right whale abundance in CCB. 


# prior.b <- c(set_prior("cauchy(50,50)", class = "Intercept"),
#              set_prior("normal(.32, .16)", class = "b", coef = "pop.est"), 
#              #set_prior("", class = "b", coef = "CalCCB*mtz.patchiness"),
#              set_prior("normal(0.003, 0.0015)", class = "b", coef = "CalCCB"),
#              set_prior("normal(8.5, 4.25)", class = "b", coef = "mtz.patchiness"),
#              set_prior("normal(0, 12)", class = "b", coef = "SST"),
#              set_prior("normal(0, 0.00045)", class = "b", coef = "RegZpl"),
#              set_prior("normal(0.025, 0.0125)", class = "b", dpar = "hu", coef = "mtz.patchiness"),
#              set_prior("normal(-0.04, 0.02)", class = "b", dpar = "hu", coef = "SST"),
#              set_prior("normal(0.00001, 0.000005)", class = "b", dpar = "hu", coef = "CalCCB"),
#              set_prior("normal(0.001, 0.0005)", class = "b", dpar = "hu", coef = "pop.est"))
# 
# 
# model_formula_b = eg.abund ~ SST + pop.est + mtz.patchiness*CalCCB + RegZpl + (1|Year)
# model_formula_hu_b = update(model_formula_b,  hu ~ . )
# model_formula_b = bf(model_formula_b, model_formula_hu_b)
# 
# abund.gamma.hurd.B <- brm(model_formula_b, 
#                           family = 'hurdle_gamma', prior = prior.b,
#                           warmup = 1000, inits = 0, cores = 4, 
#                           iter = 2000, chains = 2, 
#                           control = list(stepsize = 0.01, max_treedepth = 15,
#                                          adapt_delta=0.99), data = all.data)
# 
# abund.b.summary <- summary(abund.gamma.hurd.B, waic = TRUE)
# plot(abund.gamma.hurd.B, waic = TRUE)
# pp_check(abund.gamma.hurd.B)
# plot(marginal_effects(abund.gamma.hurd.B), points = TRUE)
# save(abund.gamma.hurd.B, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/abund.gamma.hurd.B.rda")
# load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/abund.gamma.hurd.B.rda")
# 
# ## show the marginal effects separately for each year
# conditions <- data.frame(Year = unique(all.data$Year))
# rownames(conditions) <- unique(all.data$Year)
# me_abund.gamma.hurd.B <- marginal_effects(
#   abund.gamma.hurd.B, conditions = conditions, 
#   re_formula = NULL, method = "predict")
# plot(me_abund.gamma.hurd.B, ncol = 5, points = TRUE)
# 
# 
# ## back transform results from gamma
# ## BRMS uses inverse link for gamma family so to 
# ## back transform 1/coef.  the hurdle part needs to be exp. 
# fixed_eff = data.frame(abund.b.summary$fixed, check.names = F)
# fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
# fixed_eff$`Odds ratio` =c(NA, exp(fixed_eff$Estimate[2]),
#                           rep(NA, 6),
#                           exp(fixed_eff$Estimate[9:14]))
# fixed_eff$`OR low 95%` = c(NA, exp(fixed_eff$`l-95% CI`[2]),
#                            rep(NA,6),
#                            exp(fixed_eff$`l-95% CI`[9:14]))
# fixed_eff$`OR high 95%` = c(NA, exp(fixed_eff$`u-95% CI`[2]),
#                             rep(NA,6),
#                             exp(fixed_eff$Estimate[9:14]))
# fixed_eff$`backtransformed est` = c(1/(fixed_eff$Estimate[1]), 
#                                     NA,
#                                     1/fixed_eff$Estimate[3:8],
#                                     rep(NA, 6))
# 
# fixed_eff$`backtransformed low 95%` = c(1/(fixed_eff$`l-95% CI`[1]), 
#                                         NA,
#                                         1/fixed_eff$`l-95% CI`[3:8],
#                                         rep(NA, 6))
# fixed_eff$`backtransformed high 95%` = c(1/(fixed_eff$`u-95% CI`[1]), 
#                                          NA,
#                                          1/fixed_eff$`u-95% CI`[3:8],
#                                          rep(NA, 6))
# 
# fixed_eff = fixed_eff %>% 
#   dplyr::select(`Odds ratio`, `OR low 95%`, `OR high 95%`, `backtransformed est`, 
#                 `backtransformed low 95%`,
#                 `backtransformed high 95%`)
# abund.b.table <- pander::pander(fixed_eff)
# 
# ## standardize coefficients after transformation
# fixed_eff$`standardized coefficients` =c(NA, NA,
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$SST)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[4]*sd(all.data$pop.est)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[5]*sd(all.data$mtz.patchiness)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[6]*sd(all.data$CalCCB)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[7]*sd(all.data$RegZpl)/sd(all.data$eg.abund),
#                                          rep(NA, 7))


################### Model 2 for SEM B CCBStrat ####
################### Original hypothesis: CCBStratification is directly influenced by
################### the interaction between local wind speed and local wind direction and SST
################### and a random effect of year.

# strat.prior.b <- c(set_prior("cauchy(0.19, 0.03)", class = "Intercept"),
#                    set_prior("normal(0.19, 0.09)", class = "b", coef = "SST"),
#                    set_prior("normal(0, 0.175)", class = "b", coef = "LocalWindSpd"))
# #set_prior("normal(0, 1.6)", class = "b", coef = "CompLocalWind"),
# #set_prior("normal(0, 0.175)", class = "b", coef = "LocalWindSpd*CompLocalWind"))
# 
# 
# CCBStrat.b <- brm(CCBStrat ~ SST + LocalWindSpd*CompLocalWind + (1|Year), 
#                   family = "gamma", prior = strat.prior.b,
#                   warmup = 1000, inits = 0,
#                   iter = 2500, chains = 2, 
#                   control = list(stepsize = 0.01, max_treedepth = 15,
#                                  adapt_delta=0.99), 
#                   data = all.data)
# 
# CCBStrat.b.summary <- summary(CCBStrat.b, waic = TRUE)
# plot(CCBStrat.b, waic = TRUE)
# pp_check(CCBStrat.b)
# plot(marginal_effects(CCBStrat.b), points = TRUE)
# save(CCBStrat.b, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.b.rda")
# load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.b.rda")
# 
# 
# ## show the marginal effects separately for each year
# conditions <- data.frame(Year = unique(all.data$Year))
# rownames(conditions) <- unique(all.data$Year)
# me_CCBStrat.b <- marginal_effects(
#   CCBStrat.b, conditions = conditions, 
#   re_formula = NULL, method = "predict")
# plot(me_CCBStrat.b, ncol = 5, points = TRUE)
# 
# ## back transform results from gamma
# ## BRMS uses inverse link for gamma family so to 
# ## back transform 1/coef. 
# fixed_eff = data.frame(CCBStrat.b.summary$fixed, check.names = F)
# fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
# fixed_eff$`backtransformed est` =c(NA, 1/(fixed_eff$Estimate[2:9]))
# fixed_eff$`backtransformed low 95%` = c(NA, 1/(fixed_eff$`l-95% CI`[2:9]))
# fixed_eff$`backtransformed high 95%` = c(NA, 1/(fixed_eff$`u-95% CI`[2:9]))
# 
# fixed_eff = fixed_eff %>% 
#   dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
#                 `backtransformed est`,
#                 `backtransformed low 95%`,
#                 `backtransformed high 95%`)
# CCBStrat.b.table <- pander::pander(fixed_eff)
# 
# ## standardize coefficients after transformation
# # fixed_eff$`standardized coefficients` =c(NA,
# #                                          fixed_eff$`backtransformed est`[2]*sd(all.data$SST)/sd(all.data$CCBStrat),
# #                                          fixed_eff$`backtransformed est`[3]*sd(all.data$LocalWindSpd)/sd(all.data$CCBStrat),
# #                                          rep(NA, 6))
# 
# 
# ## standardize coefficients 
# fixed_eff$`standardized coefficients` =c(NA,
#                                          fixed_eff$`Estimate`[2]*sd(all.data$SST)/sd(all.data$CCBStrat),
#                                          fixed_eff$`Estimate`[3]*sd(all.data$LocalWindSpd)/sd(all.data$CCBStrat),
#                                          rep(NA, 6))
# 
# 

################### Model 3 for SEM B PSEUDO ####
################### This model hypothesizes that Pseudocalanus in CCB is a function of
################### SST and a random effect of year.

Pseudo.prior.b <- c(set_prior("cauchy(43, 82)", class = "Intercept"),
                    set_prior("normal(0, 2490)", class = "b", coef = "SST"))


Pseudo.b <- brm(Pseudo ~ SST + (1|Year),
                family = "gamma", prior = Pseudo.prior.b, 
                warmup = 1000, inits = 0, cores = 4, 
                iter = 2000, chains = 2, 
                control = list(stepsize = 0.01, max_treedepth = 15,
                               adapt_delta=0.99), 
                data = all.data)

pseudo.b.summary <- summary(Pseudo.b, waic = TRUE)
plot(Pseudo.b, waic = TRUE)
pp_check(Pseudo.b)
p <- plot(marginal_effects(Pseudo.b), points = TRUE) 
save(Pseudo.b, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/Pseudo.b.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/Pseudo.b.rda")

p[[1]] +
  xlab("Sea Surface Temperature") +
  ylab("Cape Cod Bay Pseudocalanus spp.")

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_Pseudo.b <- marginal_effects(
  Pseudo.b, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_Pseudo.b, ncol = 5, points = TRUE)



## back transform results from gamma
## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

pseudo.coefs <- data.frame(pseudo.b.summary$fixed, check.names = F) %>%
  select(Estimate) %>%
  
  ## exponentiate the coefs
  ## that don't deal with contrasts
  mutate(exp(Estimate))

## try plotting with bayesplot
post <- posterior_samples(Pseudo.b)


mcmc_areas(
  post, pars=c("b_SST"), 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

################### Model 4 for SEM B
################### This model hypothesizes that Regional Zooplankton (regional habitat quality)
################### is directly influenced by Regional Zooplankton with a one month lag.
################### and a random effect of year. 

## this ran on the clusters

load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/RegZpl.b.rda")

#RegZpl.prior.b <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
#                    set_prior("normal(0.5, 0.125)", class = "b", coef = "RegZplLag1"),
#                    set_prior("normal(0, 26926)", class = "b", coef = "CCBStrat"))


## this model did not converge at iterations = 6000
#RegZpl.b <- brm(RegZpl ~ RegZplLag1 + CCBStrat + (1|Year), 
##                family = "gamma", prior = RegZpl.prior.b, 
#                warmup = 1000, inits = 0, cores = 4, 
##                iter = 6000, chains = 4, 
#                control = list(stepsize = 0.01, max_treedepth = 25,
#                               adapt_delta=0.99), 
#                data = all.data)



summary(RegZpl.b, waic = TRUE)
plot(RegZpl.b, waic = TRUE)
pp_check(RegZpl.b)
plot(marginal_effects(RegZpl.b), points = TRUE)


## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_RegZpl.b <- marginal_effects(
  RegZpl.b, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_RegZpl.b, ncol = 5, points = TRUE)




################### Model 6 for SEM B
################### This model hypothesizes that Calanus in CCB is a function of
################### RegWindDirec interacting with RegWindSpd and a random
################### effect of year.

#prior.cal <- c(set_prior("cauchy(4, 9.68)", class = "Intercept"),
#               set_prior("normal(2578, 1289)", class = "b", coef = "LocalWindSpd"), 
               # set_prior("", class = "b", coef = "CompLocalWind"),
 #              set_prior("normal(0.075, 0.03)", class = "b", coef = "RegZplLag1"), 
               #  set_prior("", class = "b", coef = "CompLocalWind:LocalWindSpd"),
  #             set_prior("normal(8287, 4143)", class = "b", coef = "CCBStrat"),
  #             set_prior("normal(0.075, 0.03)", class = "b", coef = "RegZpl"))

## this runs in less than 24 hours on clusters with 8 cores
#cal.gamma.hurd.B <- brm(CalCCB ~ CCBStrat + RegZpl + RegZplLag1 + LocalWindSpd*CompLocalWind + (1|Year), 
#                        family = "gamma", prior = prior.cal,
#                        warmup = 1000, inits = 0, 
#                        iter = 6000, chains = 2, 
#                        control = list(stepsize = 0.01, max_treedepth = 20,
#                                       adapt_delta=0.99), data = all.data)




#CalCCB.b.summary <- summary(cal.gamma.hurd.B, waic = TRUE)
#plot(cal.gamma.hurd.B, waic = TRUE)
##pp_check(cal.gamma.hurd.B)
#p <- plot(marginal_effects(cal.gamma.hurd.B), points = TRUE)
#save(cal.gamma.hurd.B, file = "cal.gamma.hurd.B.rda")
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/cal.gamma.hurd.B.rda")





################### Model 7 for SEM B
################### This model hypothesizes Regional Zooplankton abundance with a 
################### 1 month lag is directly impacted by regional SST with a 2 yr lag
################### and the NAO with a 4 yr lag, and a random effect of year

regzpl.prior.b <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
                    set_prior("normal(13753, 6976)", class = "b", coef = "SST2yrLag"),
                    set_prior("normal(1807, 903)", class = "b", coef = "NAO4yrlag"))


regzpllag.b <- brm(RegZplLag1 ~ SST2yrLag + NAO4yrlag + (1|Year),
                   family = "gamma" , prior = regzpl.prior.b,
                   warmup = 1000, inits = 0, 
                   iter = 4000, chains = 2, 
                   control = list(stepsize = 0.01, max_treedepth = 15,
                                  adapt_delta=0.99), data = all.data)



regzpllag.b.summary <- summary(regzpllag.b, waic = TRUE)
plot(regzpllag.b, waic = TRUE)
pp_check(regzpllag.b)
plot(marginal_effects(regzpllag.b), points = TRUE)
save(regzpllag.b, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpllag.b.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpllag.b.rda")

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_regzpllag.b <- marginal_effects(
  regzpllag.b, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_regzpllag.b, ncol = 5, points = TRUE)


## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef. 
fixed_eff = data.frame(regzpllag.b.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =c(1/(fixed_eff$Estimate))
fixed_eff$`backtransformed low 95%` = c(1/(fixed_eff$`l-95% CI`))
fixed_eff$`backtransformed high 95%` = c(1/(fixed_eff$`u-95% CI`))

fixed_eff = fixed_eff %>% 
  dplyr::select(`backtransformed est`,
                `backtransformed low 95%`,
                `backtransformed high 95%`)
regzpl.lag.b.table <- pander::pander(fixed_eff)

## standardize coefficients after transformation
fixed_eff$`standardized coefficients` =c(NA,
                                         fixed_eff$`backtransformed est`[2]*sd(all.data$SST2yrLag)/sd(all.data$RegZplLag1),
                                         fixed_eff$`backtransformed est`[3]*sd(all.data$NAO4yrlag)/sd(all.data$RegZplLag1))

################### Model 8 for SEM B
################### This model hypothesizes that SST at a 2 yr lag is caused by
################### the NAO wiht a 4 yr lag and a random effect of year.
sst.prior.b <- c(set_prior("normal(6,4)", class = "Intercept"),
                 set_prior("normal(-0.65, 0.32)", class = "b", coef = "NAO4yrlag"))


sst.lag.b <- brm(SST2yrLag ~ NAO4yrlag + (1|Year),
                 family = "normal", prior = sst.prior.b,
                 warmup = 1000, inits = 0, cores = 4, 
                 iter = 4000, chains = 2, 
                 control = list(stepsize = 0.01, max_treedepth = 15,
                                adapt_delta=0.99), data = all.data)


sst.lag.b.summary <- summary(sst.lag.b, waic = TRUE)
plot(sst.lag.b, waic = TRUE)
pp_check(sst.lag.b)
plot(marginal_effects(sst.lag.b), points = TRUE)
save(sst.lag.b, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.lag.b.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.lag.b.rda")

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_sst.lag.b <- marginal_effects(
  sst.lag.b, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_sst.lag.b, ncol = 5, points = TRUE)

## Gaussian doesn't need back transforming
fixed_eff = data.frame(sst.lag.b.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL

## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA,
                                         fixed_eff$`Estimate`[2]*sd(all.data$NAO4yrlag)/sd(all.data$SST2yrLag))


################### Model 9 for SEM B
################### This model hypothesizes that SST is caused by SST at a 2 yrlag

sst.nolag.prior.b <- c(set_prior("normal(6, 4)", class = "Intercept"),
                       set_prior("normal(0.5, 0.25)", class = "b", coef = "SST2yrLag"))


sst.b <- brm(SST ~ SST2yrLag + (1|Year),
             family = "normal", prior = sst.nolag.prior.b,
             warmup = 1000, inits = 0, 
             iter = 4000, chains = 2, 
             control = list(stepsize = 0.01, max_treedepth = 15,
                            adapt_delta=0.99), data = all.data)


sst.b.summary <- summary(sst.b, waic = TRUE)
plot(sst.b, waic = TRUE)
pp_check(sst.b)
plot(marginal_effects(sst.b), points = TRUE)
save(sst.b, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.b.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.b.rda")


## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_sst.b <- marginal_effects(
  sst.b, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_sst.b, ncol = 5, points = TRUE)

## Gaussian doesn't need back transforming
fixed_eff = data.frame(sst.b.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL

## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA,
                                         fixed_eff$`Estimate`[2]*sd(all.data$SST2yrLag)/sd(all.data$SST))



###### Model B2   ####
##### Model B2 eg abundance ####


prior.b2 <- c(set_prior("cauchy(50,50)", class = "Intercept"),
              set_prior("normal(.32, .16)", class = "b", coef = "pop.est"), 
              set_prior("normal(0, 0.00045)", class = "b", coef = "RegZpl"),
              set_prior("normal(8.5, 4.25)", class = "b", coef = "mtz.patchiness"),
              set_prior("normal(0, 12)", class = "b", coef = "SST"),
              set_prior("normal(0.025, 0.0125)", class = "b", dpar = "hu", coef = "mtz.patchiness"),
              set_prior("normal(-0.04, 0.02)", class = "b", dpar = "hu", coef = "SST"),
              set_prior("normal(0.001, 0.0005)", class = "b", dpar = "hu", coef = "pop.est"))


model_formula_b2 = eg.abund ~ SST + pop.est + RegZpl + mtz.patchiness + (1|Year)
model_formula_hu_b2 = update(model_formula_b2,  hu ~ . )
model_formula_b2 = bf(model_formula_b2, model_formula_hu_b2)

## this takes about 2 hours to run
abund.gamma.hurd.B2 <- brm(model_formula_b2, 
                           family = 'hurdle_gamma', prior = prior.b2,
                           warmup = 1000, inits = 0, cores = 4, 
                           iter = 4000, chains = 2, 
                           control = list(stepsize = 0.01, max_treedepth = 15,
                                          adapt_delta=0.99), data = all.data)

abund.b2.summary <- summary(abund.gamma.hurd.B2, waic = TRUE)
plot(abund.gamma.hurd.B2, waic = TRUE)
pp_check(abund.gamma.hurd.B2)
plot(marginal_effects(abund.gamma.hurd.B2), points = TRUE)
save(abund.gamma.hurd.B2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/abund.gamma.hurd.B2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/abund.gamma.hurd.B2.rda")



## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef.  the hurdle part needs to be exp. 
fixed_eff = data.frame(abund.b2.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`Odds ratio` =c(NA, exp(fixed_eff$Estimate[2]),
                          rep(NA, 4),
                          exp(fixed_eff$Estimate[7:10]))
fixed_eff$`OR low 95%` = c(NA, exp(fixed_eff$`l-95% CI`[2]),
                           rep(NA,4),
                           exp(fixed_eff$`l-95% CI`[7:10]))
fixed_eff$`OR high 95%` = c(NA, exp(fixed_eff$`u-95% CI`[2]),
                            rep(NA,4),
                            exp(fixed_eff$Estimate[7:10]))
fixed_eff$`backtransformed est` = c(1/(fixed_eff$Estimate[1]), 
                                    NA,
                                    1/fixed_eff$Estimate[3:6],
                                    rep(NA, 4))
fixed_eff$`backtransformed low 95%` = c(1/(fixed_eff$`l-95% CI`[1]), 
                                        NA,
                                        1/fixed_eff$`l-95% CI`[3:6],
                                        rep(NA, 4))
fixed_eff$`backtransformed high 95%` = c(1/(fixed_eff$`u-95% CI`[1]), 
                                         NA,
                                         1/fixed_eff$`u-95% CI`[3:6],
                                         rep(NA, 4))

fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
                `Odds ratio`, `OR low 95%`, 
                `OR high 95%`, `backtransformed est`, 
                `backtransformed low 95%`,
                `backtransformed high 95%`)
abund.b.table <- pander::pander(fixed_eff)

## standardize coefficients after transformation
# fixed_eff$`standardized coefficients` =c(NA, NA,
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$SST)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[4]*sd(all.data$pop.est)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[5]*sd(all.data$RegZpl)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[6]*sd(all.data$mtz.patchiness)/sd(all.data$eg.abund),
#                                          rep(NA, 4))


fixed_eff$`standardized coefficients` =c(NA, NA,
                                         fixed_eff$`Estimate`[3]*sd(all.data$SST)/sd(all.data$eg.abund),
                                         fixed_eff$`Estimate`[4]*sd(all.data$pop.est)/sd(all.data$eg.abund),
                                         fixed_eff$`Estimate`[5]*sd(all.data$RegZpl)/sd(all.data$eg.abund),
                                         fixed_eff$`Estimate`[6]*sd(all.data$mtz.patchiness)/sd(all.data$eg.abund),
                                         rep(NA, 4))

## standardize binary coefficients
# Extract predicted values on the link scale
preds <- predict(abund.gamma.hurd.B2, type = "link")

# Compute sd of error variance using theoretical variances
sd.y.LT <- sqrt(var(preds) + pi^2/3)

# Compute sd of x
sd.pop.est <- sd(all.data$pop.est)
sd.SST <- sd(all.data$SST)
sd.patchy <- sd(all.data$mtz.patchiness)
sd.RegZpl <- sd(all.data$RegZpl)

# extract beta coefs
Beta.glm.pop <- fixed_eff[8,1]
Beta.glm.sst <- fixed_eff[7,1]
Beta.glm.mtz.patchiness <- fixed_eff[10, 1]
Beta.glm.regzpl <- fixed_eff[9, 1]

## range standardized after transformation
# fixed_eff$`range standardized coefficients` =c(NA, NA,
#                                          fixed_eff$`backtransformed est`[3]*diff(range(all.data$SST))/diff(range(all.data$eg.abund)),
#                                          fixed_eff$`backtransformed est`[4]*diff(range(all.data$pop.est))/diff(range(all.data$eg.abund)),
#                                          fixed_eff$`backtransformed est`[5]*diff(range(all.data$RegZpl))/diff(range(all.data$eg.abund)),
#                                          fixed_eff$`backtransformed est`[6]*diff(range(all.data$mtz.patchiness))/diff(range(all.data$eg.abund)),
#                                          Beta.glm.sst * sd.SST / sd.y.LT[1],
#                                          Beta.glm.pop * sd.pop.est / sd.y.LT[1],
#                                          Beta.glm.mtz.patchiness * sd.patchy / sd.y.LT[1],
#                                          Beta.glm.regzpl * sd.RegZpl / sd.y.LT[1])

## range standardized 
fixed_eff$`range standardized coefficients` =c(NA, NA,
                                               fixed_eff$`Estimate`[3]*diff(range(all.data$SST))/diff(range(all.data$eg.abund)),
                                               fixed_eff$`Estimate`[4]*diff(range(all.data$pop.est))/diff(range(all.data$eg.abund)),
                                               fixed_eff$`Estimate`[5]*diff(range(all.data$RegZpl))/diff(range(all.data$eg.abund)),
                                               fixed_eff$`Estimate`[6]*diff(range(all.data$mtz.patchiness))/diff(range(all.data$eg.abund)),
                                               Beta.glm.sst * sd.SST / sd.y.LT[1],
                                               Beta.glm.pop * sd.pop.est / sd.y.LT[1],
                                               Beta.glm.mtz.patchiness * sd.patchy / sd.y.LT[1],
                                               Beta.glm.regzpl * sd.RegZpl / sd.y.LT[1])

## try plotting with bayesplot
post <- posterior_samples(abund.gamma.hurd.B2)


mcmc_areas(
  post[, 3:10], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(abund.gamma.hurd.B2)


################### Model B2 CCBStrat ####

strat.prior.b2 <- c(set_prior("cauchy(0.19, 0.03)", class = "Intercept"),
                    set_prior("normal(0.19, 0.09)", class = "b", coef = "SST"),
                    set_prior("normal(0, 0.175)", class = "b", coef = "LocalWindSpd"),
                    set_prior("normal(0.4, 0.2)", class = "b", coef = "AO1yrLag"))

CCBStrat.b2 <- brm(CCBStrat ~ AO1yrLag + SST + LocalWindSpd*CompLocalWind + (1|Year), 
                   family = "gamma", prior = strat.prior.b2,
                   warmup = 1000, inits = 0,
                   iter = 4000, chains = 2, 
                   control = list(stepsize = 0.01, max_treedepth = 15,
                                  adapt_delta=0.99), 
                   data = all.data)

conditions <- data.frame(CompLocalWind = c("NE", "S", "NW", "W"))
CCBStrat.b2.summary <- summary(CCBStrat.b2, waic = TRUE)
plot(CCBStrat.b2, waic = TRUE)
pp_check(CCBStrat.b2)
plot(marginal_effects(CCBStrat.b2), points = TRUE)
plot(marginal_effects(CCBStrat.b2, conditions = conditions,
                      "AO1yrLag"), points = TRUE)
save(CCBStrat.b2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.b2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.b2.rda")


## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef
fixed_eff = data.frame(CCBStrat.b2.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
                `backtransformed est`, `backtransformed low 95%`,
                `backtransformed high 95%`)
strat.transformed.table <- pander::pander(fixed_eff)

## standardize coefficients after transformation
## to standardize categorical coefficiants multiply the coefficient by
## .5/range(response variable)
# fixed_eff$`standardized coefficients by std` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*sd(all.data$AO1yrLag)/sd(all.data$CCBStrat),
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$SST)/sd(all.data$CCBStrat),
#                                          fixed_eff$`backtransformed est`[4]*sd(all.data$LocalWindSpd)/sd(all.data$CCBStrat),
#                                          rep(NA,6))

## range standardize after transformation
# fixed_eff$`range standardized coeficients` = c(NA,
#                                                fixed_eff$`backtransformed est`[2]*diff(range(all.data$AO1yrLag))/diff(range(all.data$CCBStrat)),
#                                                fixed_eff$`backtransformed est`[3]*diff(range(all.data$SST))/diff(range(all.data$CCBStrat)),
#                                                fixed_eff$`backtransformed est`[4]*diff(range(all.data$LocalWindSpd))/diff(range(all.data$CCBStrat)),
#                                                fixed_eff$`backtransformed est`[5]*.5/diff(range(all.data$CCBStrat)),
#                                                fixed_eff$`backtransformed est`[6]*.5/diff(range(all.data$CCBStrat)),
#                                                fixed_eff$`backtransformed est`[7]*.5/diff(range(all.data$CCBStrat)),
#                                                rep(NA,3))
# 
# 


## standardize coefficients 
fixed_eff$`standardized coefficients by std` =c(NA, 
                                                fixed_eff$`Estimate`[2]*sd(all.data$AO1yrLag)/sd(all.data$CCBStrat),
                                                fixed_eff$`Estimate`[3]*sd(all.data$SST)/sd(all.data$CCBStrat),
                                                fixed_eff$`Estimate`[4]*sd(all.data$LocalWindSpd)/sd(all.data$CCBStrat),
                                                rep(NA,6))

## range standardize
fixed_eff$`range standardized coeficients` = c(NA,
                                               fixed_eff$`Estimate`[2]*diff(range(all.data$AO1yrLag))/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[3]*diff(range(all.data$SST))/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[4]*diff(range(all.data$LocalWindSpd))/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[5]*.5/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[6]*.5/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[7]*.5/diff(range(all.data$CCBStrat)),
                                               rep(NA,3))

## try plotting with bayesplot
post <- posterior_samples(CCBStrat.b2)


mcmc_areas(
  post[, 2:10], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

## how much variation did we explain?
bayes_R2(CCBStrat.b2)



## does Local wind have an effect on strat
## and regional wind direction have an effect on strat.
## compute averages for each category and 
## I think the NE part should be more than just the intercept
post$mu_CompLocalWindNE <- post$b_Intercept
post$mu_CompLocalWindNW <- post$b_Intercept + post$b_CompLocalWindNW
post$mu_CompLocalWindS <- post$b_Intercept + post$b_CompLocalWindS
post$mu_CompLocalWindW  <- post$b_Intercept + post$b_CompLocalWindW

post %>%
  transmute(CompLocalWindNE = b_Intercept,
            CompLocalWindNW = b_Intercept + `b_CompLocalWindNW`,
            CompLocalWindS = b_Intercept + `b_CompLocalWindS`,
            CompLocalWindW = b_Intercept + `b_CompLocalWindW`)%>%
  gather(key, value) %>%
  ggplot(aes(x = value, group = key, color = key, fill = key)) +
  geom_density(alpha = 1/4) +
  scale_x_continuous() +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Local Wind Direction on Stratification") +
  theme(text = element_text(family = "Times"))

## If the proportion of the difference between two
## wind directions is less than 5% than it is extremely likely
## to be different because that means that the 90% Percentile
## Intervals of the difference between the two directions
## don't overlap 0.  

post %>%
  mutate(CompLocalWindNE = b_Intercept,
         CompLocalWindNW = b_Intercept + `b_CompLocalWindNW`,
         CompLocalWindS = b_Intercept + `b_CompLocalWindS`,
         CompLocalWindW = b_Intercept + `b_CompLocalWindW`) %>% 
  mutate(diffNE.NW = CompLocalWindNW - CompLocalWindNE,
         diffNE.S = CompLocalWindNE - CompLocalWindS,
         diffNE.W = CompLocalWindW - CompLocalWindNE, 
         diffNW.S = CompLocalWindNW - CompLocalWindS, 
         diffNW.W = CompLocalWindW - CompLocalWindNW, 
         diffS.W = CompLocalWindW - CompLocalWindS) %>%
  summarise(Proportion_of_the_difference_below_0NE.NW = sum(diffNE.NW < 0) / length(diffNE.NW),
            Proportion_of_the_difference_below_0NE.S = sum(diffNE.S < 0) / length(diffNE.S),
            Proportion_of_the_difference_below_0NE.W = sum(diffNE.W < 0) / length(diffNE.W),
            Proportion_of_the_difference_below_0NW.S = sum(diffNW.S < 0) / length(diffNW.S),
            Proportion_of_the_difference_below_0NW.W = sum(diffNW.W < 0) / length(diffNW.W),
            Proportion_of_the_difference_below_0S.W = sum(diffS.W < 0) / length(diffS.W))


## does the interaction between local wind spd and direc
## have an effect on strat.
## compute averages for each category and 
## I think the NE part should be more than just the intercept
post$mu_CompLocalWindNE.spd <- post$b_Intercept
post$mu_CompLocalWindNW.spd <- post$b_Intercept + post$"b_LocalWindSpd:CompLocalWindNW"
post$mu_CompLocalWindS.spd <- post$b_Intercept + post$"b_LocalWindSpd:CompLocalWindS"
post$mu_CompLocalWindW.spd  <- post$b_Intercept + post$"b_LocalWindSpd:CompLocalWindW"

## use emmeans to work on the interaction between
## local wind direction and speed on stratification
emmeans(CCBStrat.b2, ~ CompLocalWind | LocalWindSpd, cov.reduce = FALSE)
emmeans(CCBStrat.b2, ~ CompLocalWind | LocalWindSpd)

## this gives the 90% quantile
CCBStrat.b2 %>%
  emmeans( ~ LocalWindSpd * CompLocalWind) %>%
  gather_emmeans_draws() %>%
  median_qi(.width = .90)

CCBStrat.b2 %>%
  emmeans( ~ CompLocalWind*LocalWindSpd) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


#### Model B2 CCBStrat w/month ####
strat.prior.b2 <- c(set_prior("cauchy(0.19, 0.03)", class = "Intercept"),
                    set_prior("normal(0.19, 0.09)", class = "b", coef = "SST"),
                    set_prior("normal(0, 0.175)", class = "b", coef = "LocalWindSpd"),
                    set_prior("normal(0.4, 0.2)", class = "b", coef = "AO1yrLag"))

CCBStrat.b2.month <- brm(CCBStrat ~ Month + AO1yrLag + SST + LocalWindSpd*CompLocalWind + (1|Year), 
                   family = "gamma", prior = strat.prior.b2,
                   warmup = 1000, inits = 0,
                   iter = 4000, chains = 2, 
                   control = list(stepsize = 0.01, max_treedepth = 15,
                                  adapt_delta=0.99), 
                   data = all.data)


CCBStrat.b2.summary <- summary(CCBStrat.b2.month, waic = TRUE)
plot(CCBStrat.b2.month, waic = TRUE)
pp_check(CCBStrat.b2.month)
p <- plot(marginal_effects(CCBStrat.b2.month),
     points = TRUE)
conditions <- data.frame(CompLocalWind = c("NE", "S", "NW", "W"))
plot(marginal_effects(CCBStrat.b2.month, conditions = conditions,
                      "AO1yrLag"), points = TRUE)
save(CCBStrat.b2.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.b2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.b2.month.rda")


conditions <- data.frame(Month = c("1", "2", "3", "4", "5"))
plot(marginal_effects(CCBStrat.b2.month, conditions = conditions,
                      "LocalWindSpd"), points = TRUE)



p[[6]] +
  facet_wrap(~CompLocalWind)

## try plotting with bayesplot
post <- posterior_samples(CCBStrat.b2.month)


mcmc_areas(
  post[, 1:5], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

## how much variation did we explain?
bayes_R2(CCBStrat.b2.month)

## Does the interaction between wind spd and
## direction influence CCBStrat?
## Yes.
CCBStrat.b2.month %>%
  emtrends(~LocalWindSpd*CompLocalWind, var="LocalWindSpd",
           at=list(LocalWindSpd = c(2, 12))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


emtrends(CCBStrat.b2.month, ~LocalWindSpd | CompLocalWind, 
         var = "LocalWindSpd", at = list(LocalWindSpd = c(2,12)))


## This was done with emmeans because we are only 
## trying to get the contrast.  This tells us
## there are differences between the effect of
## some months on stratification.
CCBStrat.b2.month %>% 
  emmeans( ~ Month) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  geom_vline(xintercept = 0, color = "red", lty=2)


## This tells us that all months have a negative
## effect on stratification.THis was also done iwth 
## emmeans because we are only trying to get the contrast.
## change the HPD to 90% (the default is 95%) to match
## the rest of my analysis
plot(hpd.summary(prob = 90, emmeans(CCBStrat.b2.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
strat.em <- emmeans(CCBStrat.b2.month, ~ Month)


## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## There is no effect of wind direction on its own.
CCBStrat.b2.month %>% 
  emtrends(~CompLocalWind, var = "LocalWindSpd", 
           at=list(LocalWindSpd = c(2, 12))) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## BEWARE this uses prob = 95% HPD 
## the rest of my analysis is at 90%
strat.trend <- emtrends(CCBStrat.b2.month, 
                        pairwise ~ CompLocalWind, 
                        var = "LocalWindSpd")

## This shows the relationship of each
## wind direction at 90% HPD
CCBStrat.b2.month %>%
  emtrends(~CompLocalWind, var = "LocalWindSpd") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = CompLocalWind)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of of windspd*direction
## to do a sanity check on the slopes being the same.
emmip(CCBStrat.b2.month, CompLocalWind ~ LocalWindSpd, 
      cov.reduce = range)



## This gives the main effect of local wind speed on its own
## This averages over the values of wind direction.
## The interaction matters in thsi model so 
## we can't trust the results from this.
emmeans(CCBStrat.b2.month,  ~ LocalWindSpd)

plot(hpd.summary(prob = 90, emmeans(CCBStrat.b2.month, ~ LocalWindSpd))) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## This gives teh main effect of local win didrection when all
## teh wind speeds are averaged. 
## The interaction matters in this model so we can't
## trust the results from this.
emmeans(CCBStrat.b2.month,  ~ CompLocalWind)

plot(hpd.summary(prob = 90, emmeans(CCBStrat.b2.month, ~ CompLocalWind))) +
  geom_vline(xintercept = 0, color = "red", lty=2)



acf(residuals(CCBStrat.b2.month)[,1])

################### Model B2 RegZpl ####

RegZpl.prior.b2 <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
                     set_prior("normal(0, 26926)", class = "b", coef = "CCBStrat"))


RegZpl.b2 <- brm(RegZpl ~ CCBStrat + (1|Year), 
                 family = "gamma", prior = RegZpl.prior.b2, 
                 warmup = 1000, inits = 0, cores = 4, 
                 iter = 4000, chains = 4, 
                 control = list(stepsize = 0.01, max_treedepth = 25,
                                adapt_delta=0.99), 
                 data = all.data)



regzpl.b2.summary <- summary(RegZpl.b2, waic = TRUE)
plot(RegZpl.b2, waic = TRUE)
pp_check(RegZpl.b2)

plot(marginal_effects(RegZpl.b2), points = TRUE)
save(RegZpl.b2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.b2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.b2.rda")

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_RegZpl.b <- marginal_effects(
  RegZpl.b, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_RegZpl.b, ncol = 5, points = TRUE)


## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef
fixed_eff = data.frame(regzpl.b2.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
                `backtransformed est`, `backtransformed low 95%`,
                `backtransformed high 95%`)
strat.transformed.table <- pander::pander(fixed_eff)

## standardize coefficients after transformation
# fixed_eff$`standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*sd(all.data$CCBStrat)/sd(all.data$RegZpl))


## range standardize coefficients after transformation
# fixed_eff$`range standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*diff(range(all.data$CCBStrat))/diff(range(all.data$RegZpl)))


## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA, 
                                         fixed_eff$`Estimate`[2]*sd(all.data$CCBStrat)/sd(all.data$RegZpl))


## range standardize coefficients 
fixed_eff$`range standardized coefficients` =c(NA, 
                                               fixed_eff$`Estimate`[2]*diff(range(all.data$CCBStrat))/diff(range(all.data$RegZpl)))


## try plotting with bayesplot
post <- posterior_samples(RegZpl.b2)


mcmc_areas(
  post, pars =c("b_CCBStrat"), 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(RegZpl.b2)



################### Model B2 RegZpl w/month ####

RegZpl.prior.b2 <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
                     set_prior("normal(0, 26926)", class = "b", coef = "CCBStrat"))


RegZpl.b2.month <- brm(RegZpl ~ CCBStrat + Month + (1|Year), 
                 family = "gamma", prior = RegZpl.prior.b2, 
                 warmup = 1000, inits = 0, cores = 4, 
                 iter = 4000, chains = 4, 
                 control = list(stepsize = 0.01, max_treedepth = 25,
                                adapt_delta=0.99), 
                 data = all.data)



regzpl.b2.month.summary <- summary(RegZpl.b2.month, waic = TRUE)
plot(RegZpl.b2.month, waic = TRUE)
pp_check(RegZpl.b2.month)
plot(marginal_effects(RegZpl.b2.month), points = TRUE)
save(RegZpl.b2.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.b2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.b2.month.rda")

bayes_R2(RegZpl.b2.month)

## try plotting with bayesplot
post <- posterior_samples(RegZpl.b2.month)


mcmc_areas(
  post[,2:6], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

## This was done with emmeans because we are only 
## trying to get the contrast.  This tells us
## there are differences between the effect of
## some months on RegZpl.
RegZpl.b2.month %>%
  emmeans( ~ Month) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

## This tells us that all months have a positive
## effect on regzpl. This was also done iwth 
## emmeans because we are only trying to get the contrast.
## change the HPD to 90% (the default is 95%) to match
## the rest of my analysis
plot(hpd.summary(prob = 90, emmeans(RegZpl.b2.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
zpl.em <- emmeans(RegZpl.b2.month, ~ Month)


acf(residuals(RegZpl.b2.month)[,1])


## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

RegZpl.B2.coefs <- data.frame(regzpl.b2.month.summary$fixed, check.names = F) %>%
  select(Estimate) %>%
  
  ## first pull out the est.
  ## coefs from the summary table
  ## that don't deal with the contrast
  slice(2) %>%
  
  ## exponentiate the coefs
  ## that don't deal with contrasts
  mutate(exp(Estimate))


## this will calculate the coefficients
## when there are contrasts.
RegZpl.B2.coefs.contrasts <- post %>%
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

#### Model B2  Calanus ####

## THis was run on the clusters
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/cal.gamma.hurd.B2.rda")



#prior.cal.b2 <- c(set_prior("cauchy(4, 9.68)", class = "Intercept"),
#               set_prior("normal(2578, 1289)", class = "b", coef = "LocalWindSpd"), 
# set_prior("", class = "b", coef = "CompLocalWind"),
#  set_prior("", class = "b", coef = "CompLocalWind:LocalWindSpd"),
#              set_prior("normal(8287, 4143)", class = "b", coef = "CCBStrat"),
#               set_prior("normal(0.075, 0.03)", class = "b", coef = "RegZpl"))

## this runs in less than 24 hours on clusters with 8 cores
#cal.gamma.hurd.B2 <- brm(CalCCB ~ CCBStrat + RegZpl + LocalWindSpd*CompLocalWind + (1|Year), 
#                        family = "gamma", prior = prior.cal.b2,
#                        warmup = 1000, inits = 0, 
#                        iter = 4000, chains = 2, 
#                        control = list(stepsize = 0.01, max_treedepth = 20,
#                                       adapt_delta=0.99), data = all.data)



cal.b2.summary <- summary(cal.gamma.hurd.B2, waic = TRUE)
plot(cal.gamma.hurd.B2, waic = TRUE)
pp_check(cal.gamma.hurd.B2)
cal.plot <- plot(marginal_effects(cal.gamma.hurd.B2), points = TRUE) +
  ylim(0, 30000)
conditions <- data.frame(CompLocalWind = c("NE", "S", "NW", "W"))
p <- plot(marginal_effects(cal.gamma.hurd.B2, 
                      conditions = conditions), points = TRUE) 

p[[1]] +
  xlab("Stratification") +
  ylab("Cape Cod Bay Calanus finmarchicus")

p[[2]] +
  xlab("Regional Calanus finmarchicus") +
  ylab("Cape Cod Bay Calanus finmarchicus") +
  ylim(0, 1e+06)

p[[3]] +
  xlab("Local Wind Speed") +
  ylab("Cape Cod Bay Calanus finmarchicus") +
  ylim(0, 1e+06)

p1 <- plot(marginal_effects(cal.gamma.hurd.B2), points = TRUE)

p1[[4]] +
  xlab("Local Wind Direction") +
  ylab("Cape Cod Bay Calanus finmarchicus")

p1[[5]] +
  xlab("Local Wind Speed") +
  ylab("Cape Cod Bay Calanus finmarchicus") +
  ylim(0, 5e+05)

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_cal.b2 <- marginal_effects(
  cal.gamma.hurd.B2, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_cal.b2, ncol = 5, points = TRUE)


## try plotting with bayesplot
post <- posterior_samples(cal.gamma.hurd.B2)


mcmc_areas(
  post[, 2:10], 
  prob = 0.9, # 80% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )


bayes_R2(cal.gamma.hurd.B2)



## Does the interaction between wind spd and
## direction influence Calanus?
## Yes
cal.gamma.hurd.B2 %>%
  emtrends(~LocalWindSpd*CompLocalWind, var="LocalWindSpd",
           at=list(LocalWindSpd = c(2, 11))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## There is an effect of wind direction on its own.
cal.gamma.hurd.B2 %>% 
  emtrends(~CompLocalWind, var = "LocalWindSpd") %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  geom_vline(xintercept = 0, color = "red", lty=2)


## This shows the relationship of each
## wind direction at 90% HPD
cal.gamma.hurd.B2 %>%
  emtrends(~CompLocalWind, var = "LocalWindSpd", 
           at=list(LocalWindSpd = c(2, 11))) %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = CompLocalWind)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)


## plot a linear version of of windspd*direction
## to do a sanity check on the slopes being the same.
emmip(cal.gamma.hurd.B2, CompLocalWind ~ LocalWindSpd, 
      cov.reduce = range)


## This gives the main effect of wind speed on its own
## This averages over the values of wind direction
emmeans(cal.gamma.hurd.B2,  ~ LocalWindSpd)

plot(hpd.summary(prob = 90, emmeans(cal.gamma.hurd.B2, ~ LocalWindSpd))) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## This gives teh main effect of wind direction when all
## teh speeds are averaged.  
emmeans(cal.gamma.hurd.B2,  ~ CompLocalWind)

plot(hpd.summary(prob = 90, emmeans(cal.gamma.hurd.B2, ~ CompLocalWind))) +
  geom_vline(xintercept = 0, color = "red", lty=2)


################### Model B2 Patchy ####

## this was run on the clusters
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/mtz.patchy.b.rda")



#mtz.patchy.prior.b <- c(##set_prior("", class = "Intercept"),
#  set_prior("normal(2.8, 0.47)", class = "b", coef = "CCBStrat"),
#  set_prior("normal(0.0002, 0.0001)", class = "b", coef = "Centro"),
#  set_prior("normal(-0.95, 0.47)", class = "b", coef = "LocalWindSpd"),
  #set_prior("", coef = "b", class = "LocalWindDirec"),
  #  set_prior("", coef = "b", class = "LocalWindSpd*LocalWindDirec"),
 # set_prior("normal(0.00015, 0.00007)", class = "b", coef = "CalCCB"),
#  set_prior("normal(0, 0.95)", class = "b", coef = "SST"),
#  set_prior("normal(0.00015, 0.00007)", class = "b", coef = "Pseudo"))

## I ran this on the cluster.  it took about 70 minutes.
#mtz.patchy.b <- brm(mtz.patchiness ~ SST + Pseudo + CalCCB + CCBStrat + Centro + LocalWindSpd*CompLocalWind + (1|Year),
#                    family = "gamma", prior = mtz.patchy.prior.b, 
#                    warmup = 1000, inits = 0, cores = 4, 
#                    iter = 3000, chains = 2, 
#                    control = list(stepsize = 0.01, max_treedepth = 15,
#                                  adapt_delta=0.99), 
#                    data = all.data)



mtz.patchy.b.summary <- summary(mtz.patchy.b, waic = TRUE)
plot(mtz.patchy.b, waic = TRUE)
pp_check(mtz.patchy.b)
plot(marginal_effects(mtz.patchy.b, "CompLocalWind:LocalWindSpd"), points = TRUE)





## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_mtz.patchy.b <- marginal_effects(
  mtz.patchy.b, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_mtz.patchy.b, ncol = 5, points = TRUE)



## try plotting with bayesplot
post <- posterior_samples(mtz.patchy.b)


mcmc_areas(
  post[, 2:13],
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(mtz.patchy.b)


## Does the interaction between wind spd and
## direction influence patchiness?
## Yes
mtz.patchy.b %>%
  emtrends(~LocalWindSpd*CompLocalWind, var="LocalWindSpd",
           at=list(LocalWindSpd = c(2, 11))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

emtrends(mtz.patchy.b, ~LocalWindSpd | CompLocalWind, 
         var = "LocalWindSpd", at = list(LocalWindSpd = c(2,11)))

## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## There is a difference between some wind directions
## on its own on patchiness
mtz.patchy.b %>% 
  emtrends(~CompLocalWind, var = "LocalWindSpd") %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## BEWARE this uses prob = 95% HPD 
## the rest of my analysis is at 90%
patchy.trend <- emtrends(mtz.patchy.b, 
                        pairwise ~ CompLocalWind, 
                        var = "LocalWindSpd")

## This shows the relationship of each
## wind direction at 90% HPD
mtz.patchy.b %>%
  emtrends(~CompLocalWind, var = "LocalWindSpd") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = CompLocalWind)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of of windspd*direction
## to do a sanity check on the slopes being the same.
emmip(mtz.patchy.b, CompLocalWind ~ LocalWindSpd, 
      cov.reduce = range)


## This gives the main effect of wind spd on its own
## This averages over the values of wind direction
emmeans(mtz.patchy.b,  ~ LocalWindSpd)

plot(hpd.summary(prob = 90, emmeans(mtz.patchy.b, ~ LocalWindSpd))) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## This gives teh main effect of wind direction when all
## teh wind speeds are averaged  
emmeans(mtz.patchy.b,  ~ CompLocalWind)

plot(hpd.summary(prob = 90, emmeans(mtz.patchy.b, ~ CompLocalWind))) +
  geom_vline(xintercept = 0, color = "red", lty=2)

