##### SEM A in BRMS ####
## After trying and failing to fit glm's and glmer's with the proper distribution
## that would also work in psem() Jarrett and I decided we needed
## to try a gamma hurdle in brms.  A gamma hurdle is needed because the abundance
## data are continuous, zero inflated, but gamma shaped.  So, the 
## gamma-hurdle model is a joint modle that will first model the 0's
## and non-0's, then model the non-0's as a gamma distribution.  
## gamma hurdle models can be used in an SEM in brms.

## After we realized gamma hurdle was not the way to go
## because gamma hurdle assumes you have no TRUE zeros which
## we very well may have.  So, we tried the abundance models
## with a zero-inflated binomial distribution.  This can be
## found in the zinb.R file.


## load in the data
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/all.data.rda")

####load libraries ####
library(brms)
library(tidyverse)
library(emmeans)
library(tidybayes)

###################  SEM A eg.abund ####
########### Original hypoth: Monthly right whale abundance is conditional upon mean total
########### zooplankton, the population size, the interaction between mean total zooplankton
########### and stratification, with a random effect of year.  The test of d-separation for this
########### model when this model was treated as linear (which it is not) indicated regional
########### SST is also directly causes right whale abundance.


## this was run on the clusters
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/abund.gamma.hurd.a.rda")


#prior <- c(set_prior("normal(406,10)", class = "b", coef = "pop.est"), 
#           ## this is a normal prior centered around 406 with 95% of the 
           ## probability between 406 +/- 20
 #          set_prior("normal(12100,100)", class = "b", coef = "MTZ"), 
           ## this is a normal prior centered around 12100 with 95% of the 
           ## probability between 12100 +/- 200
  #         set_prior("normal(5, 1.5)", class = "b", coef = "sst"),
   #        set_prior("normal(.66, 2)", class = "b", coef = "CCBStrat"))
## this is a normal prior centered around .66 with 95% of the 
## probability between .66 +/-4.  This prior could probably
## be half cauchy.

#prior2 <- c(set_prior("normal(406,30)", class = "b", coef = "pop.est"), 
            ## this is a normal prior centered around 406 with 95% of the 
            ## probability between 406 +/- 60
 #           set_prior("normal(12100, 100)", class = "b", coef = "pop.est"), 
            ## should this be a gamma distribution?
            ## shape = 1.9, scale = 10000 looks like it matches these data
            ## right now this is a normal prior centered around 12100 with 95% of the 
            ## probability between 12100 +/- 200
  #          set_prior("cauchy(.66, 2)", class = "b", coef = "CCBStrat"), 
            ## shape = 3, scale = .2 looks like it matches these data
   #         set_prior("normal(5, 3)", class = "b", coef = "sst")) 

#priorCauch <- c(set_prior("normal(406,120)", class = "b", coef = "pop.est"), 
            ## this is a normal prior centered around 406 with 95% of the 
            ## probability between 406 +/- 120
 #           set_prior("cauchy(500, 10000)", class = "b", coef = "MTZ"), 
            ## should this be a gamma distribution? Jarrett said cauchy is fine.
            ## gamma distribution throws an error "log prob at initial value exception: gamma_lapdf inverse scale param is inf, but must be finite"
  #          set_prior("cauchy(0.25, .3)", class = "b", coef = "CCBStrat"), 
   #         set_prior("normal(5, 3)", class = "b", coef = "SST")) 

## these are the priors that are explained in the SEM_table.xlsx
#prior3 <- c(set_prior("normal(406,120)", class = "b", coef = "pop.est"), 
            ## this is a normal prior centered around 406 with 95% of the 
            ## probability between 406 +/- 120
 #           set_prior("cauchy(2109, 2570)", class = "b", coef = "MTZ"), 
  #          set_prior("cauchy(.34,.16)", class = "b", coef = "CCBStrat"), #location = .34, scale = .16 this was estimated using fitdistr() of the raw data before we took the means
   #         set_prior("normal(10, 5)", class = "b", coef = "SST")) 

## these are hte priors explained in teh blue notebook
#prior4 <- c(set_prior("cauchy(50, 50)", class = "Intercept"),
 #           set_prior("normal(0.0025, 0.001 )", class = "b", coef = "MTZ"), 
  #          set_prior("normal(.32, .16)", class = "b", coef = "pop.est"),
   #         set_prior("normal(0, 12)", class = "b", coef = "SST"), 
    #        set_prior("normal(120, 60)", class = "b", coef = "MTZ:CCBStrat"),
     #       set_prior("normal(42, 42)", class = "b", coef = "CCBStrat"), 
#            set_prior("normal(0,0.00045)", class = "b", coef = "RegZpl"),
#            set_prior("normal(-0.04, 0.02)", class = "b",  dpar = "hu", coef = "SST"),
#            set_prior("normal(0.001, 0.0005)", class = "b",  dpar = "hu", coef = "pop.est"),
#            set_prior("normal(0.000008, 0.000004)", class = "b",  dpar = "hu", coef = "MTZ"),
#            set_prior("normal(0.15, 0.07)", class = "b",  dpar = "hu", coef = "CCBStrat"))

## in this model "RegZpl" is doing the job of Regional habitat quality
#model_formula = eg.abund ~ pop.est + SST + MTZ + MTZ * CCBStrat + RegZpl + (1|Year)
#model_formula_hu = update(model_formula,  hu ~ . )
#model_formula = bf(model_formula, model_formula_hu)

#model_formula = bf(model_formula, model_formula_hu)
#abund.gamma.hurd <- brm(formula = model_formula, 
#                        family=hurdle_gamma,  prior = prior4, 
#                        warmup = 1000, inits = 0, cores = 4,
#                        iter = 3000, chains = 4, sample_prior = TRUE,
#                        control = list(stepsize = 0.01, max_treedepth = 15, 
#                                       adapt_delta = 0.99), data = all.data)


abund.summary <- summary(abund.gamma.hurd, waic = TRUE)
plot(abund.gamma.hurd, waic = TRUE)
pp_check(abund.gamma.hurd) ## distribution of observations in blue (eg.abund)
## each line is a predicted distribution of abundance from simulation of posterior.
## the shading and the lines should look similar, otherwise something is wrong. 
plot(marginal_effects(abund.gamma.hurd), points = TRUE)


## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_abund.gamma.hurd <- marginal_effects(
  abund.gamma.hurd, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_abund.gamma.hurd, ncol = 5, points = TRUE)

## plot the prior and posterior distributions
#plot(hypothesis(abund.gamma.hurd, "MTZ > 0"))
#plot(hypothesis(abund.gamma.hurd, "SST = 0"))
#plot(hypothesis(abund.gamma.hurd, "MTZ:CCBStrat >0"))
#plot(hypothesis(abund.gamma.hurd, "CCBStrat > 10"))



## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef.  the hurdle part needs to be exp. 
fixed_eff = data.frame(abund.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`Odds ratio` =c(NA, exp(fixed_eff$Estimate[2]),
                          rep(NA, 6),
                          exp(fixed_eff$Estimate[9:14]))
fixed_eff$`OR low 95%` = c(NA, exp(fixed_eff$`l-95% CI`[2]),
                           rep(NA,6),
                           exp(fixed_eff$`l-95% CI`[9:14]))
fixed_eff$`OR high 95%` = c(NA, exp(fixed_eff$`u-95% CI`[2]),
                            rep(NA,6),
                            exp(fixed_eff$Estimate[9:14]))
fixed_eff$`backtransformed est` = c(1/(fixed_eff$Estimate[1]), 
                                    NA,
                                    1/fixed_eff$Estimate[3:8],
                                    rep(NA, 6))
fixed_eff$`backtransformed low 95%` = c(1/(fixed_eff$`l-95% CI`[1]), 
                                        NA,
                                        1/fixed_eff$`l-95% CI`[3:8],
                                        rep(NA, 6))
fixed_eff$`backtransformed high 95%` = c(1/(fixed_eff$`u-95% CI`[1]), 
                                         NA,
                                         1/fixed_eff$`u-95% CI`[3:8],
                                         rep(NA, 6))

fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
                `Odds ratio`, `OR low 95%`, `OR high 95%`, 
                `backtransformed est`, 
                `backtransformed low 95%`,
                `backtransformed high 95%`)
abund.b.table <- pander::pander(fixed_eff)

## standardize coefficients after transformation
# fixed_eff$`standardized coefficients` =c(NA, NA,
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$pop.est)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[4]*sd(all.data$SST)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[5]*sd(all.data$MTZ)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[6]*sd(all.data$CCBStrat)/sd(all.data$eg.abund),
#                                          fixed_eff$`backtransformed est`[7]*sd(all.data$RegZpl)/sd(all.data$eg.abund),
#                                          rep(NA, 7))
# 


## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA, NA,
                                         fixed_eff$`Estimate`[3]*sd(all.data$pop.est)/sd(all.data$eg.abund),
                                         fixed_eff$`Estimate`[4]*sd(all.data$SST)/sd(all.data$eg.abund),
                                         fixed_eff$`Estimate`[5]*sd(all.data$MTZ)/sd(all.data$eg.abund),
                                         fixed_eff$`Estimate`[6]*sd(all.data$CCBStrat)/sd(all.data$eg.abund),
                                         fixed_eff$`Estimate`[7]*sd(all.data$RegZpl)/sd(all.data$eg.abund),
                                         rep(NA, 7))



## standardize binary coefficients
# Extract predicted values on the link scale
preds <- predict(abund.gamma.hurd, type = "link")

# Compute sd of error variance using theoretical variances
sd.y.LT <- sqrt(var(preds) + pi^2/3)

# Compute sd of x
sd.pop.est <- sd(all.data$pop.est)
sd.SST <- sd(all.data$SST)
sd.MTZ <- sd(all.data$MTZ)
sd.CCBStrat <- sd(all.data$CCBStrat)
sd.RegZpl <- sd(all.data$RegZpl)

# extract beta coefs
Beta.glm.pop <- fixed_eff[9,1]
Beta.glm.sst <- fixed_eff[10,1]
Beta.glm.mtz <- fixed_eff[11, 1]
Beta.glm.strat <- fixed_eff[12, 1]
Beta.glm.regzpl <- fixed_eff[13, 1]




## range standardize coefficients after transformation
## include binomial responses that were standardized
## but it does not seem like they need to be back transformed
fixed_eff$`range standardized coefficients` =c(NA, NA,
                                         fixed_eff$`Estimate`[3]*diff(range(all.data$pop.est))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`Estimate`[4]*diff(range(all.data$SST))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`Estimate`[5]*diff(range(all.data$MTZ))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`Estimate`[6]*diff(range(all.data$CCBStrat))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`Estimate`[7]*diff(range(all.data$RegZpl))/diff(range(all.data$eg.abund)),
                                        NA,
                                         Beta.glm.pop * sd.pop.est / sd.y.LT[1],
                                         Beta.glm.sst * sd.SST / sd.y.LT[1],
                                         Beta.glm.mtz * sd.MTZ / sd.y.LT[1],
                                         Beta.glm.strat * sd.CCBStrat / sd.y.LT[1],
                                         Beta.glm.regzpl * sd.RegZpl / sd.y.LT[1],
                                          NA)


## try plotting with bayesplot
post <- posterior_samples(abund.gamma.hurd)


mcmc_areas(
  post[, 3:14], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(abund.gamma.hurd)

## use emmeans to work on the interaction between
## MTZ and stratification on eg abund
## this gives the 90% quantile
emmeans(abund.gamma.hurd, ~ CCBStrat | MTZ)

abund.gamma.hurd %>%
  emmeans( ~ MTZ * CCBStrat) %>%
  gather_emmeans_draws() %>%
  median_qi(.width = .90)

abund.gamma.hurd %>%
  emmeans( ~ MTZ*CCBStrat) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))



################### Model A CCBStrat ####
########### Original hypoth: Stratification in CCB is a function of regional SST, and the
########### interaction between regional wind speed and regional wind direction
########### with a random effect of year.



##prior.strat.rad <- c(set_prior("normal(0,.5)", class = "b", coef = "sinRegWindDirecRad"), 
##                 set_prior("normal(0,.6)", class = "b", coef = "cosRegWindDirecRad"),
#                 set_prior("normal(8, 2)", class = "b", coef = "RegWindSpd"),
#                 set_prior("normal(5, 3)", class = "b", coef = "SST"))

#prior.3 <- c(#set_prior("normal(0,.5)", class = "b", coef = "sinRegWindDirecRad"), 
#             #set_prior("normal(0,.6)", class = "b", coef = "cosRegWindDirecRad"),
#             set_prior("normal(8, 4)", class = "b", coef = "RegWindSpd"),
#             set_prior("normal(10, 5)", class = "b", coef = "SST"))


#model_formula = CCBStrat ~ SST + sinRegWindDirecRad * cosRegWindDirecRad * RegWindSpd + (1|Year)
#model_formula_hu = update(model_formula,  hu ~ . )
#model_formula = bf(model_formula, model_formula_hu)


#strat.gamma.hurd.rad <- brm(formula = model_formula,
#                        family =  'hurdle_gamma', prior = prior.strat.rad,
#                        warmup = 1000, inits = 0, cores = 4,
##                        iter = 3000, chains = 2, 
  #                      control = list(stepsize = 0.01, 
  #                                     max_treedepth = 15, adapt_delta = 0.99), data = all.data)


#strat.gamma.hurd.form <- bf(CCBStrat ~ SST + RegWindDirec * RegWindSpd + (1|Year), 
#                            family =  hurdle_gamma)



#strat.summary <- summary(strat.gamma.hurd.rad, waic = TRUE)
#plot(strat.gamma.hurd.rad, waic = TRUE)
#pp_check(strat.gamma.hurd.rad)
#plot(marginal_effects(strat.gamma.hurd.rad), points = TRUE)



## construct a model with wind direction as a factor variable, not sin and cos
## components

## these are the priors explained in the SEM_table.xlsx.  I'm not sure
## about the prior for the categorical variable (CompRegWind)
      prior.strat.3 <- c(set_prior("normal(0,10)", class = "b"), ## this throws an error when I specify the coef = "CompRegWind" argument...?
            set_prior("normal(8, 4)", class = "b", coef = "RegWindSpd"),
              set_prior("normal(10, 5)", class = "b", coef = "SST"))

prior.strat.4 <- c(set_prior("cauchy(.19, .03)", class = "Intercept"), ## setting this as a gamma distribution throws an error "Rejecting initial value: Gradient evaluated at the initial value is not finite."
                set_prior("normal(0.19, 0.09)", class = "b", coef = "SST"),
                   set_prior("normal(0, 0.175)", class = "b"), 
                   set_prior("normal(0, 0.175)", class = "b", coef = "RegWindSpd")) ## set prior for all interactions
                 


strat.gamma.hurd.fac <- brm(formula = CCBStrat ~ SST + CompRegWind*RegWindSpd + (1|Year),
                            family = 'gamma', 
                            prior = prior.strat.4,
                            warmup = 1000, inits = 0, cores = 4,
                            iter = 3000, chains = 2, 
                            control = list(stepsize = 0.01, 
                                           max_treedepth = 20, adapt_delta = 0.99), 
                            data = all.data)

conditions <- data.frame(CompRegWind = c("NE", "S", "NW", "W"))
strat.summary.fac <- summary(strat.gamma.hurd.fac, waic = TRUE)
plot(strat.gamma.hurd.fac, waic = TRUE)
pp_check(strat.gamma.hurd.fac)
plot(marginal_effects(strat.gamma.hurd.fac), points = TRUE)
plot(marginal_effects(strat.gamma.hurd.fac, "RegWindSpd", 
                      conditions = conditions), points = TRUE) 
save(strat.gamma.hurd.fac, file = "strat.gamma.hurd.fac.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/strat.gamma.hurd.fac.rda")

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_strat.gamma.hurd.fac <- marginal_effects(
  strat.gamma.hurd.fac, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_strat.gamma.hurd.fac, ncol = 5, points = TRUE)

## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef
fixed_eff = data.frame(strat.summary.fac$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
                `backtransformed est`, 
                `backtransformed low 95%`,
                `backtransformed high 95%`)
strat.transformed.table <- pander::pander(fixed_eff)

## standardize coefficients after backtransformation
# fixed_eff$`standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*sd(all.data$SST)/sd(all.data$CCBStrat),
#                                          rep(NA, 3),
#                                          fixed_eff$`backtransformed est`[6]*sd(all.data$RegWindSpd)/sd(all.data$CCBStrat),
#                                          rep(NA,3))

## range standardize coefficients after back transformation
# fixed_eff$`range standardized coefficients` =c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*diff(range(all.data$SST))/diff(range(all.data$CCBStrat)),
#                                          fixed_eff$`backtransformed est`[3]*.5/diff(range(all.data$CCBStrat)),
#                                          fixed_eff$`backtransformed est`[4]*.5/diff(range(all.data$CCBStrat)),
#                                          fixed_eff$`backtransformed est`[5]*.5/diff(range(all.data$CCBStrat)),
#                                          fixed_eff$`backtransformed est`[6]*diff(range(all.data$RegWindSpd))/diff(range(all.data$CCBStrat)),
#                                          rep(NA,3))

## standardize coefficients 
fixed_eff$`standardized coefficients` =c(NA, 
                                         fixed_eff$`Estimate`[2]*sd(all.data$SST)/sd(all.data$CCBStrat),
                                         rep(NA, 3),
                                         fixed_eff$`Estimate`[6]*sd(all.data$RegWindSpd)/sd(all.data$CCBStrat),
                                         rep(NA,3))

## range standardize coefficients 
fixed_eff$`range standardized coefficients` =c(NA, 
                                               fixed_eff$`Estimate`[2]*diff(range(all.data$SST))/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[3]*.5/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[4]*.5/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[5]*.5/diff(range(all.data$CCBStrat)),
                                               fixed_eff$`Estimate`[6]*diff(range(all.data$RegWindSpd))/diff(range(all.data$CCBStrat)),
                                               rep(NA,3))

## try plotting with bayesplot
post <- posterior_samples(strat.gamma.hurd.fac)


mcmc_areas(
  post[, 2:9], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )


bayes_R2(strat.gamma.hurd.fac)



## is there an effect of the contrasts?
post.strat.a <- 
  strat.gamma.hurd.fac %>%
  posterior_samples()

## does regional wind have an effect on strat
## compute averages for each category and 
## summarize the results with the transpose of base R’s apply() function, 
## rounding to two digits of precision.
post.strat.a$mu_RegWindNE <- post.strat.a$b_Intercept
post.strat.a$mu_RegWindNW <- post.strat.a$b_Intercept + post.strat.a$b_CompRegWindNW
post.strat.a$mu_RegWindS <- post.strat.a$b_Intercept + post.strat.a$b_CompRegWindS
post.strat.a$mu_RegWindW   <- post.strat.a$b_Intercept + post.strat.a$b_CompRegWindW

## There is no effect of regional wind in any direction
## with 90% intervals
round(t(apply(post.strat.a[ , 33:36], 2, quantile, c(.5, .05, .95))), digits = 2)

## do using 80% intervals make a diffference
round(t(apply(post.strat.a[ , 33:36], 2, quantile, c(.5, .1, .90))), digits = 2)


post.strat.a %>%
  transmute(RegWindNE = b_Intercept,
            RegWindNW = b_Intercept + `b_CompRegWindNW`,
            RegWindS = b_Intercept + `b_CompRegWindS`,
            RegWindW = b_Intercept + `b_CompRegWindW`)%>%
  gather(key, value) %>%
  ggplot(aes(x = value, group = key, color = key, fill = key)) +
  geom_density(alpha = 1/4) +
  scale_x_continuous() +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Regional Wind Direction on Stratification") +
  theme(text = element_text(family = "Times"))

## If the proportion of the difference between two
## wind directions is less than 5% than it is extremely likely
## to be different because that means that the 90% Percentile
## Intervals of the difference between the two directions
## don't overlap 0.  

post.strat.a %>%
  mutate(RegWindNE = b_Intercept,
         RegWindNW = b_Intercept + `b_CompRegWindNW`,
         RegWindS = b_Intercept + `b_CompRegWindS`,
         RegWindW = b_Intercept + `b_CompRegWindW`) %>% 
  mutate(diffNE.NW = RegWindNW - RegWindNE,
         diffNE.S = RegWindNE - RegWindS,
         diffNE.W = RegWindW - RegWindNE, 
         diffNW.S = RegWindNW - RegWindS, 
         diffNW.W = RegWindW - RegWindNW, 
         diffS.W = RegWindW - RegWindS) %>%
  summarise(Proportion_of_the_difference_below_0NE.NW = sum(diffNE.NW < 0) / length(diffNE.NW),
            Proportion_of_the_difference_below_0NE.S = sum(diffNE.S < 0) / length(diffNE.S),
            Proportion_of_the_difference_below_0NE.W = sum(diffNE.W < 0) / length(diffNE.W),
            Proportion_of_the_difference_below_0NW.S = sum(diffNW.S < 0) / length(diffNW.S),
            Proportion_of_the_difference_below_0NW.W = sum(diffNW.W < 0) / length(diffNW.W),
            Proportion_of_the_difference_below_0S.W = sum(diffS.W < 0) / length(diffS.W))



## does the interaction between regional wind spd
## and regional wind direction have an effect on strat.
## compute averages for each category and 
## I think the NE part should be more than just the intercept
post.strat.a$mu_RegWindNE.spd <- post.strat.a$b_Intercept
post.strat.a$mu_RegWindNW.spd <- post.strat.a$b_Intercept + post.strat.a$"b_CompRegWindNW:RegWindSpd"
post.strat.a$mu_RegWindS.spd <- post.strat.a$b_Intercept + post.strat.a$"b_CompRegWindS:RegWindSpd"
post.strat.a$mu_RegWindW.spd  <- post.strat.a$b_Intercept + post.strat.a$"b_CompRegWindW:RegWindSpd"

## There is no effect of the interaction 
## between regional wind speed and direction
## with 90% intervals
round(t(apply(post.strat.a[ , 37:40], 2, quantile, c(.5, .05, .95))), digits = 2)

## do using 80% intervals make a diffference
## nope
round(t(apply(post.strat.a[ , 37:40], 2, quantile, c(.5, .1, .90))), digits = 2)


post.strat.a %>%
  transmute(RegWindNE.spd = b_Intercept,
            RegWindNW.spd = b_Intercept + `b_CompRegWindNW:RegWindSpd`,
            RegWindS.spd = b_Intercept + `b_CompRegWindS:RegWindSpd`,
            RegWindW.spd = b_Intercept + `b_CompRegWindW:RegWindSpd`)%>%
  gather(key, value) %>%
  ggplot(aes(x = value, group = key, color = key, fill = key)) +
  geom_density(alpha = 1/4) +
  scale_x_continuous() +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Regional Wind Direction and Wind Speed on Stratification") +
  theme(text = element_text(family = "Times"))


## use emmeans to work on the interaction between
## regional wind direction and speed on stratification
emmeans(strat.gamma.hurd.fac, ~ RegWindSpd | CompRegWind, cov.reduce = FALSE)
emmeans(strat.gamma.hurd.fac, ~ RegWindSpd | CompRegWind)

## this gives the 90% quantile
strat.gamma.hurd.fac %>%
  emmeans( ~ RegWindSpd * CompRegWind) %>%
  gather_emmeans_draws() %>%
  median_qi(.width = .90)

strat.gamma.hurd.fac %>%
  emmeans( ~ CompRegWind*RegWindSpd) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
 geom_halfeyeh(.width = c(.90))



#### Model A CCBStrat w/month ####
prior.strat.4 <- c(set_prior("cauchy(.19, .03)", class = "Intercept"), ## setting this as a gamma distribution throws an error "Rejecting initial value: Gradient evaluated at the initial value is not finite."
                   set_prior("normal(0.19, 0.09)", class = "b", coef = "SST"),
                   set_prior("normal(0, 0.175)", class = "b"), 
                   set_prior("normal(0, 0.175)", class = "b", coef = "RegWindSpd")) ## set prior for all interactions

strat.A.month <- brm(formula = CCBStrat ~ Month + SST + CompRegWind*RegWindSpd + (1|Year),
                            family = 'gamma', 
                            prior = prior.strat.4,
                            warmup = 1000, inits = 0, cores = 4,
                            iter = 3000, chains = 2, 
                            control = list(stepsize = 0.01, 
                                           max_treedepth = 20, adapt_delta = 0.99), 
                            data = all.data)


strat.summary.A.month <- summary(strat.A.month, waic = TRUE)
plot(strat.A.month, waic = TRUE)
pp_check(strat.A.month)
plot(marginal_effects(strat.A.month), points = TRUE)
plot(marginal_effects(strat.A.month, "CompRegWind"), points = TRUE)
conditions <- data.frame(CompRegWind = c("NE", "S", "NW", "W"))
p.s <- plot(marginal_effects(strat.A.month, 
                      conditions = conditions), points = TRUE)
save(strat.A.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/strat.A.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/strat.A.month.rda")

p.s[[2]] + ggplot2::xlim(0,10) +
  facet_grid(Month ~ CompRegWind)

p <- plot(marginal_effects(strat.A.month), points = TRUE) 
p[[2]] + ggplot2::xlim(0,10) + 
  facet_grid(Month ~ CompRegWind)

## try plotting with bayesplot
post <- posterior_samples(strat.A.month)


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


bayes_R2(strat.A.month)

## Does the interaction between wind spd and
## direction influence stratification?
## No.
strat.A.month %>%
  emtrends(~RegWindSpd*CompRegWind, var="RegWindSpd",
           at=list(RegWindSpd = c(2, 11))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


strat.A.month %>%
  emtrends(~RegWindSpd*CompRegWind, var="RegWindSpd",
           at=list(CompRegWind =c("NE", "NW", "S", "W"))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

emtrends(strat.A.month, ~RegWindSpd | CompRegWind, 
         var = "RegWindSpd", at = list(RegWindSpd = c(2,11)))

## Get the main effect of wind speed at its mean
## over hte average of all wind directions.
plot(hpd.summary(prob = 90, emmeans(strat.A.month, ~RegWindSpd))) +
  geom_vline(xintercept = 0, color = "red", lty = 2)

emmeans(strat.A.month, ~RegWindSpd)

## Get teh main effect of wind direction 
## over the average of all wind speeds
emmeans(strat.A.month, ~CompRegWind)
plot(hpd.summary(prob = 90, emmeans(strat.A.month, ~CompRegWind))) +
  geom_vline(xintercept = 0, color = "red", lty = 2)



## This was done with emmeans because we are only 
## trying to get the contrast.  This tells us
## there are differences between the effect of
## some months on stratification.
strat.A.month %>% 
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
plot(hpd.summary(prob = 90, emmeans(strat.A.month, ~ Month))) +
       geom_vline(xintercept = 0, color = "red", lty=2)
strat.em <- emmeans(strat.A.month, ~ Month)

as_tibble(strat.em) %>%
  mutate(exp(emmean))

## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## This says at different wind directions wind speed
## as a (-) effect for W and NW wind.  
strat.A.month %>% 
  emtrends(~CompRegWind, var = "RegWindSpd") %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## BEWARE this uses prob = 95% HPD 
## the rest of my analysis is at 90%
strat.trend <- emtrends(strat.A.month, 
                        pairwise ~ CompRegWind, 
                        var = "RegWindSpd")



## Since there is no effect of the interaction
## I can use emmeans to understand the effect of 
## wind direction on its own.  THis plot tells us
## that south and west have different influences on strat, and
## NW and south have different influences on strat.
strat.A.month %>% 
  emmeans( ~ CompRegWind) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## this shows us that NW and W have a (-) effect on strat.
## this gives a warning about results being misleading
## which we can ignore because there is no effect of
## the interaction in this model.
plot(hpd.summary(prob = 90, emmeans(strat.A.month, ~ CompRegWind))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
strat.em.wind <- emmeans(strat.A.month, ~ CompRegWind)


## back transform coefs for CompRegWind
## this model was gamma dist. so coefs
## need to be exp
regwind.coef <- as_tibble(strat.trend$emtrends) %>%
  mutate(exp(RegWindSpd.trend))

## This shows the relationship of each
## wind direction at 90% HPD.  this looks at the wind speed effect at different directions.
strat.A.month %>%
  emtrends(~CompRegWind, var = "RegWindSpd") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = CompRegWind)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of of windspd*direction
## to do a sanity check on the slopes being the same.
emmip(strat.A.month, CompRegWind ~ RegWindSpd, 
      cov.reduce = range)

acf(residuals(strat.A.month)[,1])

## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

strat.coefs <- data.frame(strat.summary.A.month$fixed, check.names = F) %>%
                    select(Estimate) %>%
                    
                    ## first pull out the est.
                    ## coefs from the summary table
                    ## that don't deal with the contrast
                    slice(6, 10) %>%
                    
                    ## put row names in
                    cbind(covariate = c("SST", "RegWindSpd")) %>%
                
                    ## exponentiate the coefs
                    ## that don't deal with contrasts
                    mutate(exp(Estimate))


## this will calculate the coefficients
## when there are contrasts.
strat.coefs.contrasts <- post %>%
  mutate(JanNE = b_Intercept,
         FebNE = b_Intercept + b_Month2,
         MarchNE = b_Intercept + b_Month3,
         AprilNE = b_Intercept + b_Month4,
         MayNE = b_Intercept + b_Month5) %>% 
  as.data.frame() %>%
  summarize(mean.JanNE = mean(JanNE),
            mean.FebNE = mean(FebNE),
            mean.MarchNE = mean(MarchNE),
            mean.AprilNE = mean(AprilNE),
            mean.MayNE = mean(MayNE)) %>%
  mutate(exp.JanNE = exp(mean.JanNE),
            exp.FebNE = exp(mean.FebNE),
            exp.MarchNE = exp(mean.MarchNE),
            exp.AprilNE = exp(mean.AprilNE),
            exp.MayNE = exp(mean.MayNE))





## Simple effect of Reg Wind Spd over all wind directions
emmeans(strat.A.month, ~RegWindSpd) ## this is the effect of wind speed over all wind directions

## Simple effect of Wind Direction when wind speed is held at its mean
emmeans(strat.A.month, ~RegWindSpd + CompRegWind) ## this is the effect of each wind direction at hte mean wind speed


#### Model A MTZ ####
########### Original hypoth: Mean total zooplankton is conditional upon stratification in CCB
########### the interaction between Regional wind speed and direction with a 2 month lag
########### and regional calanus with a 2 month lag, and a random effect of year.  
########### The test of d-separation for the linear model indicated regional SST
########### also directly influences MTZ.  



#prior.mtz.3 <- c(set_prior("normal(0, 10", class = "b"), ## for CompRegWind
#               set_prior("normal(8, 4)", class = "b", coef = "RegWindSpd"), 
#               set_prior("normal(10, 5)", class = "b", coef = "SST"),
#               set_prior("cauchy(.34,.16)", class = "b", coef = "CCBStrat"),
#               set_prior("cauchy(1, 1.5)", class = "b", coef = "RegZplLag1"))

#prior.mtz.4 <- c(set_prior("cauchy(374, 465)", class = "Intercept"),
#                 set_prior("normal(0,3333)", class = "b", coef =  "SST"),
#                 set_prior("normal(2.5, 1.25)", class = "b", coef = "RegZpl"),
#                 set_prior("normal(0, 9493)", class = "b", coef = "CCBStrat"),
                #set_prior("(0,1)", class = "b", coef = "CompRegWind"),
 #                set_prior("normal(-1666,833)", class = "b", coef = "RegWindSpd")
                 #set_prior("()", class = "b", coef = "CompRegWind:RegWindSpd")
    #             )
#

#mtz.gamma.hurd.rad <- brm(formula = MTZ ~ RegZpl + SST + CCBStrat + CompRegWind*RegWindSpd + (1|Year), 
#                      family = 'gamma', prior = prior.mtz.4,
#                      warmup= 500, inits = 0, cores = 2,
#                      iter = 3000, chains = 2, 
#                     control = list(stepsize = 0.01,
#                                     max_treedepth = 26, adapt_delta = 0.99),
#                      data = all.data)

#mtz.summary <- summary(mtz.gamma.hurd.rad)
#plot(mtz.gamma.hurd.rad, waic = TRUE)
#pp_check(mtz.gamma.hurd.rad)
#plot(marginal_effects(mtz.gamma.hurd.rad), points = TRUE)

## show the marginal effects separately for each year
#conditions <- data.frame(Year = unique(all.data$Year))
#rownames(conditions) <- unique(all.data$Year)
#me_mtz.gamma.hurd <- marginal_effects(
#  mtz.gamma.hurd, conditions = conditions, 
#  re_formula = NULL, method = "predict")
#plot(me_mtz.gamma.hurd, ncol = 5, points = TRUE)


## ran on the clusters doesn't converge
#load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/mtz.gamma.hurd.rad.rda")

## Priors and model for model where wind direction is a factor not continuous
#prior.mtz.3 <- c(set_prior("normal(0,10)", class = "b"),
#                 set_prior("normal(8, 4)", class = "b", coef = "RegWindSpd"), 
#                 set_prior("normal(10, 5)", class = "b", coef = "SST"),
#                 set_prior("cauchy(.3,.6)", class = "b", coef = "CCBStrat"))
                 #   set_prior("cauchy(1, 1.5)", class = "b", coef = "RegZpl"))
                 
                 
#model_formula = MTZ ~ 0 + RegZpl + SST + CCBStrat + CompRegWind * RegWindSpd + (1|Year)
### SEE MODEL that was run on clusters ******************
#mtz.gamma.hurd.rad <- brm(formula = model_formula,
#                            family = 'gamma', prior = prior.mtz.rad,
#                            warmup= 1000, inits = 0, cores = 4,
#                            iter = 2000, chains = 2, 
#                            control = list(stepsize = 0.01,
#                            max_treedepth = 25, adapt_delta = 0.99),
#                                           data = all.data)
                 
#mtz.summary <- summary(mtz.gamma.hurd.rad)
#plot(mtz.gamma.hurd.rad, waic = TRUE)
#pp_check(mtz.gamma.hurd.rad)
#plot(marginal_effects(mtz.gamma.hurd.rad), points = TRUE)


## ran on the clusters with more iterations to help with convergance
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/mtz.gamma.hurd.2.rda")
mtz.summary <- summary(mtz.gamma.hurd.2)                
plot(mtz.gamma.hurd.2, waic = TRUE)
pp_check(mtz.gamma.hurd.2)
conditions <- data.frame(CompRegWind = c("NE", "S", "NW", "W"))
plot(marginal_effects(mtz.gamma.hurd.2), points = TRUE)
p <- plot(marginal_effects(mtz.gamma.hurd.2, 
                      conditions = conditions),
          cond_=c("NE", "S", "NW", "W"), 
          points = TRUE) 

p[[1]] +
  xlab("Regional Calanus finmarchicus") +
  ylab("Cape Cod Bay Mean Total Zooplankton") 

p[[2]] +
  xlab("Regional Sea Surface Temperature") +
  ylab("Cape Cod Bay Mean Total Zooplankton")

p[[3]] +
  xlab("Stratification") +
  ylab("Cape Cod Bay Mean Total Zooplankton")

p[[5]] +
  xlab("Regional Wind Speed") +
  ylab("Cape Cod Bay Mean Total Zooplankton")

p2 <- plot(marginal_effects(mtz.gamma.hurd.2, 
                            "RegWindSpd:CompRegWind"),
          points = TRUE) 
  

p2[[1]] +
  facet_wrap(~CompRegWind) +
  xlab("Regional Wind Speed") +
  ylab("Cape Cod Bay Mean Total Zooplankton") +
  theme(legend.position = "none")

## back transform coefs not involved
## in interaction or factors. This model
## was gamma so coefs need to be exp()
mtz.exp <- as_tibble(mtz.summary$fixed) %>%
  slice(2:4, 8) %>%
  cbind(coefs = c("RegZpl", "SST", "CCBStrat", "RegWindSpd")) %>%
  mutate(exp(Estimate))
  

## try plotting with bayesplot
post <- posterior_samples(mtz.gamma.hurd.2)


mcmc_areas(
  post[, 2:11], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(mtz.gamma.hurd.2)



## Does the interaction between wind spd and
## direction influence mtz?
## No
mtz.gamma.hurd.2 %>%
  emtrends(~RegWindSpd*CompRegWind, var="RegWindSpd",
           at=list(RegWindSpd = c(2, 11))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

## back transform the coefs for the interaction
## between RegWindSPd and Direction.
## something seems wrong here the coefs are the same as
## the coefs for wind direction on its own shown below
mtz.gamma.hurd.2 %>%
  emtrends(~RegWindSpd*CompRegWind, var="RegWindSpd",
           at=list(RegWindSpd = c(2, 11))) 

## Get the main effect of wind speed at its mean
## over hte average of all wind directions.
plot(hpd.summary(prob = 90, emmeans(mtz.gamma.hurd.2, ~RegWindSpd))) +
  geom_vline(xintercept = 0, color = "red", lty = 2)

emmeans(mtz.gamma.hurd.2, ~RegWindSpd)

## Get teh main effect of wind direction 
## over the average of all wind speeds
emmeans(mtz.gamma.hurd.2, ~CompRegWind)
plot(hpd.summary(prob = 90, emmeans(mtz.gamma.hurd.2, ~CompRegWind))) +
  geom_vline(xintercept = 0, color = "red", lty = 2)


## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## THIs tells us there is no difference
## between different wind directions on MTZ when we 
## vary the wind speed.
mtz.gamma.hurd.2 %>% 
  emtrends(~CompRegWind, var = "RegWindSpd") %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)


## BEWARE this uses prob = 95% HPD 
## the rest of my analysis is at 90%
mtz.trend <- emtrends(mtz.gamma.hurd.2, 
                        pairwise ~ CompRegWind, 
                        var = "RegWindSpd")

## back transform coefs. for wind direction. 
## this model is gamma distributed so the coefs
## need to be exp()
as_tibble(mtz.trend$emtrends) %>%
  mutate(exp(RegWindSpd.trend))

## Since there is no effect of the interaction
## I can use emmeans to understand the effect of 
## wind direction on its own.  THis plot tells us
## that NE and W have different influences on MTZ, and
## NE and S have different influences on MTZ, and NE and NW
## have different influences on MTZ.
mtz.gamma.hurd.2 %>% 
  emmeans( ~ CompRegWind) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## this shows us that all directions have a (+) effect
## on MTZ. this gives a warning about results being misleading
## which we can ignore because there is no effect of
## the interaction in this model.
plot(hpd.summary(prob = 90, emmeans(mtz.gamma.hurd.2, ~ CompRegWind))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
mtz.em.wind <- emmeans(mtz.gamma.hurd.2, ~ CompRegWind)


## This shows the relationship of each
## wind direction at 90% HPD
mtz.gamma.hurd.2 %>%
  emtrends(~CompRegWind, var = "RegWindSpd") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = CompRegWind)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of of windspd*direction
## to do a sanity check on the slopes being the same.
emmip(mtz.gamma.hurd.2, CompRegWind ~ RegWindSpd, 
      cov.reduce = range)

###################  MODEL 6 SST ####
########### Original hypoth: Sea Surface Temperature is influenced by the NAO with a 4 yr lag 
########### with a random effect of year. 

# prior.sst <-  c(set_prior("normal(.39, 2)", class = "b", coef = "NAO"))
# 
# prior.sst.4 <- c(set_prior("normal(6,4)", class = "Intercept"),
#                  set_prior("normal(-0.65, 0.32)", class = "b", coef = "NAO4yrlag"),
#                  set_prior("normal(0.5, 0.25)", class = "b", coef = "SST2yrLag"),
#                  set_prior("normal(0, 0.4)", class = "b", coef = "RegWindSpd"))
# 
# sst.mod <- brm(SST ~ NAO4yrlag + SST2yrLag + RegWindSpd*CompRegWind + (~1|Year),
#                prior = prior.sst.4, 
#                warmup = 1000, inits = 0, 
#                iter = 2500, chains =2, 
#                control = list(stepsize = 0.01,
#                              max_treedepth = 15, adapt_delta = 0.99),
#                data = all.data)
# 
# 
# sst.summary <- summary(sst.mod, waic = TRUE)
# plot(sst.mod, waic = TRUE)
# pp_check(sst.mod)
# plot(marginal_effects(sst.mod))
# save(sst.mod, file = "sst.mod.rda")
# load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.mod.rda")
# 
# conditions <- data.frame(Year = unique(all.data$Year))
# rownames(conditions) <- unique(all.data$Year)
# me_sst.mod <- marginal_effects(
#   sst.mod, conditions = conditions, 
#   re_formula = NULL, method = "predict")
# plot(me_sst.mod, ncol = 5, points = TRUE)
# 
# 
# ## standardize coefficients 
# fixed_eff = data.frame(sst.summary$fixed, check.names = F)
# fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
# fixed_eff$`standardized coefficients` =c(NA, 
#                                          fixed_eff$`Estimate`[2]*sd(all.data$NAO4yrlag)/sd(all.data$SST),
#                                          fixed_eff$`Estimate`[3]*sd(all.data$SST2yrLag)/sd(all.data$SST),
#                                          fixed_eff$`Estimate`[4]*sd(all.data$RegWindSpd)/sd(all.data$SST),
#                                          rep(NA,6))



###################  MODEL 8 RegZpl ####
########### Original hypoth: Regional Zooplankton data 
########### are predicted by NAO lagged at 4 yrs and SST lagged at 2 yrs
########### with a random effect of year

# prior.regzpl.4 <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
#                     set_prior("normal(1807, 903)", class = "b", coef = "NAO4yrlag"),
#                     set_prior("normal(13753, 6976)", class = "b", coef = "SST2yrLag"),
#                     set_prior("normal(0.5, 0.25)", class = "b", coef = "RegZplLag1"),
#                     set_prior("normal(47277, 9455)", class = "b", coef = "CCBStrat"))
#                     

## this model took about 32 hours to run,
## but did not converge at 4000 iter 1000warmup and
##maxtreedepth = 20.  Re-run with higher iterations
## and longer treedepth
# regzpl.mod <- brm(RegZpl ~ NAO4yrlag + SST2yrLag + RegZplLag1 + CCBStrat +  (~1|Year),
#                    prior = prior.regzpl.4, warmup = 1000, 
#                    family = "gamma", 
#                    inits = 0, cores = 4, 
#                    iter = 5000, chains = 2,
#                    control = list(stepsize = 0.01, 
#                                   max_treedepth = 25, adapt_delta = 0.99),
#                    data = all.data)
# 
# summary(regzpl.mod)
# plot(regzpl.mod, waic = TRUE)
# pp_check(regzpl.mod)
# plot(marginal_effects(regzpl.mod), points = TRUE)
# 
# conditions <- data.frame(Year = unique(all.data$Year))
# rownames(conditions) <- unique(all.data$Year)
# me_regzpl.mod <- marginal_effects(
#   regzpl.mod, conditions = conditions, 
#   re_formula = NULL, method = "predict")
# plot(me_regzpl.mod, ncol = 5, points = TRUE)

###################  MODEL 9 for SEM A RegZpl.lag ####
########### Original hypoth: Regional Zooplankton data with 1 month lag 
########### are predicted by NAO lagged at 4 yrs and SST lagged at 2 yrs
########### with a random effect of year, and will influence regional habitat quality next month

# prior.regzpl.lag.4 <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
#                         set_prior("normal(36957, 13636)", class = "b", coef = "NAO4yrlag"),
#                         set_prior("normal(27272, 13636)", class = "b", coef = "SST2yrLag"))
#                        
# 
# 
# 
# regzpl.lag <- brm(RegZplLag1 ~ NAO4yrlag + SST2yrLag + (~1|Year),
#                   prior = prior.regzpl.lag.4, warmup = 1000, 
#                   family = "gamma", inits = 0, cores = 4, 
#                   iter = 2000, chains = 2,
#                   control = list(stepsize = 0.01, 
#                    max_treedepth = 10, adapt_delta = 0.99),
#                      data = all.data)
# 
# regzpl.lag.summary <- summary(regzpl.lag)
# plot(regzpl.lag, waic = TRUE)
# pp_check(regzpl.lag)
# plot(marginal_effects(regzpl.lag), points = TRUE)
# save(regzpl.lag, file = "regzpl.lag.rda")
# load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpl.lag.rda")
# 
# conditions <- data.frame(Year = unique(all.data$Year))
# rownames(conditions) <- unique(all.data$Year)
# me_regzpl.lag <- marginal_effects(
#   regzpl.lag, conditions = conditions, 
#   re_formula = NULL, method = "predict")
# plot(me_regzpl.lag, ncol = 5, points = TRUE)
# 
# 
# ## back transform results from gamma
# ## BRMS uses inverse link for gamma family so to 
# ## back transform 1/coef
# fixed_eff = data.frame(regzpl.lag.summary$fixed, check.names = F)
# fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
# fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
# fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
# fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
# fixed_eff = fixed_eff %>% 
#   dplyr::select(`backtransformed est`, `backtransformed low 95%`,
#                 `backtransformed high 95%`)
# regzpl.lag.table <- pander::pander(fixed_eff)
# 
# ## standardize coefficients after transformation
# fixed_eff$`standardized coefficients` = c(NA, 
#                                          fixed_eff$`backtransformed est`[2]*sd(all.data$NAO4yrlag)/sd(all.data$RegZplLag1),
#                                          fixed_eff$`backtransformed est`[3]*sd(all.data$SST2yrLag)/sd(all.data$RegZplLag1))
# 

###################  MODEL A2 ####
###################  WE made model A2 because Model A was too complicated
################### by removing SST2yrlag and RegZpllag1 we reduced the number of arrows
################### while keeping the major hypotheses.

###################  MODEL A2 SST ####


prior.sst.a2 <- c(set_prior("normal(6,4)", class = "Intercept"),
                 set_prior("normal(-0.65, 0.32)", class = "b", coef = "NAO4yrlag"),
                 set_prior("normal(0, 0.4)", class = "b", coef = "RegWindSpd"))


## this model runs quickly
sst.a2 <- brm(SST ~ NAO4yrlag + RegWindSpd*CompRegWind + (~1|Year),
               prior = prior.sst.a2, 
               warmup = 1000, inits = 0, 
               iter = 3000, chains =2, 
               control = list(stepsize = 0.01,
                              max_treedepth = 15, adapt_delta = 0.99),
               data = all.data)


sst.a2.summary <- summary(sst.a2, waic = TRUE)
plot(sst.a2, waic = TRUE)
pp_check(sst.a2)
plot(marginal_effects(sst.a2))
save(sst.a2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.a2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.a2.rda")

conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_sst.a2 <- marginal_effects(
  sst.a2, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_sst.a2, ncol = 5, points = TRUE)

## range standardize coefficients 
fixed_eff = data.frame(sst.a2.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`range standardized coefficients` =c(NA,
                                         fixed_eff$`Estimate`[2]*diff(range(all.data$NAO4yrlag))/diff(range(all.data$SST)),
                                         fixed_eff$`Estimate`[3]*diff(range(all.data$RegWindSpd))/diff(range(all.data$SST)),
                                         fixed_eff$`Estimate`[4]*.5/diff(range(all.data$SST)),
                                         fixed_eff$`Estimate`[5]*.5/diff(range(all.data$SST)),
                                         fixed_eff$`Estimate`[6]*.5/diff(range(all.data$SST)),
                                         rep(NA,3))


## try plotting with bayesplot
post <- posterior_samples(sst.a2)


mcmc_areas(
  post[, 2:9], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(sst.a2)

post %>%
  transmute(RegWindNE = b_Intercept,
            RegWindNW = b_Intercept + `b_CompRegWindNW`,
            RegWindS = b_Intercept + `b_CompRegWindS`,
            RegWindW = b_Intercept + `b_CompRegWindW`)%>%
  gather(key, value) %>%
  ggplot(aes(x = value, group = key, color = key, fill = key)) +
  geom_density(alpha = 1/4) +
  scale_x_continuous() +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Regional Wind Direction on SST") +
  theme(text = element_text(family = "Times"))

## If the proportion of the difference between two
## wind directions is less than 5% than it is extremely likely
## to be different because that means that the 90% Percentile
## Intervals of the difference between the two directions
## don't overlap 0.  

post %>%
  mutate(RegWindNE = b_Intercept,
         RegWindNW = b_Intercept + `b_CompRegWindNW`,
         RegWindS = b_Intercept + `b_CompRegWindS`,
         RegWindW = b_Intercept + `b_CompRegWindW`) %>% 
  mutate(diffNE.NW = RegWindNW - RegWindNE,
         diffNE.S = RegWindNE - RegWindS,
         diffNE.W = RegWindW - RegWindNE, 
         diffNW.S = RegWindNW - RegWindS, 
         diffNW.W = RegWindW - RegWindNW, 
         diffS.W = RegWindW - RegWindS) %>%
  summarise(Proportion_of_the_difference_below_0NE.NW = sum(diffNE.NW < 0) / length(diffNE.NW),
            Proportion_of_the_difference_below_0NE.S = sum(diffNE.S < 0) / length(diffNE.S),
            Proportion_of_the_difference_below_0NE.W = sum(diffNE.W < 0) / length(diffNE.W),
            Proportion_of_the_difference_below_0NW.S = sum(diffNW.S < 0) / length(diffNW.S),
            Proportion_of_the_difference_below_0NW.W = sum(diffNW.W < 0) / length(diffNW.W),
            Proportion_of_the_difference_below_0S.W = sum(diffS.W < 0) / length(diffS.W))


## use emmeans to work on the interaction between
## local wind direction and speed on SST
## this gives the 90% quantile
sst.a2 %>%
  emmeans( ~ RegWindSpd * CompRegWind) %>%
  gather_emmeans_draws() %>%
  median_qi(.width = .90)

sst.a2 %>%
  emmeans( ~ CompRegWind*RegWindSpd) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


###################  MODEL A2 SST w/Month ####
prior.sst.a2 <- c(set_prior("normal(6,4)", class = "Intercept"),
                  set_prior("normal(-0.65, 0.32)", class = "b", coef = "NAO4yrlag"),
                  set_prior("normal(0, 0.4)", class = "b", coef = "RegWindSpd"))


## this model runs quickly
sst.a2.month <- brm(SST ~ Month + NAO4yrlag + RegWindSpd*CompRegWind + (~1|Year),
              prior = prior.sst.a2, 
              warmup = 1000, inits = 0, 
              iter = 3000, chains =2, 
              control = list(stepsize = 0.01,
                             max_treedepth = 15, adapt_delta = 0.99),
              data = all.data)


sst.a2.month.summary <- summary(sst.a2.month, waic = TRUE)
plot(sst.a2.month, waic = TRUE)
pp_check(sst.a2.month)
plot(marginal_effects(sst.a2.month, "Month"), points = TRUE)
save(sst.a2.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.a2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.a2.month.rda")




## try plotting with bayesplot
post <- posterior_samples(sst.a2.month)


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

bayes_R2(sst.a2.month)


## Does the interaction between wind spd and
## direction influence SST?
## No.
sst.a2.month %>%
  emtrends(~RegWindSpd*CompRegWind, var="RegWindSpd",
           at=list(RegWindSpd = c(2, 11))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

## Since there is no effect of the interaction
## I can use emmeans to understand the effect of 
## wind direction on its own.  THis plot tells us
## that south and west have different influences on strat, and
## NW and south have different influences on strat.
sst.a2.month %>% 
  emmeans( ~ CompRegWind) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## this shows us that all wind has a (+) effect on SST.
## this gives a warning about results being misleading
## which we can ignore because there is no effect of
## the interaction in this model.
plot(hpd.summary(prob = 90, emmeans(sst.a2.month, ~ CompRegWind))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
sst.em.wind <- emmeans(sst.a2.month, ~ CompRegWind)


## Get the main effect of wind speed at its mean
## over hte average of all wind directions.
plot(hpd.summary(prob = 90, emmeans(sst.a2.month, ~RegWindSpd))) +
  geom_vline(xintercept = 0, color = "red", lty = 2)

emmeans(sst.a2.month, ~RegWindSpd)

## Get teh main effect of wind direction 
## over the average of all wind speeds
emmeans(sst.a2.month, ~CompRegWind)
plot(hpd.summary(prob = 90, emmeans(sst.a2.month, ~CompRegWind))) +
  geom_vline(xintercept = 0, color = "red", lty = 2)


## This was done with emmeans because we are only 
## trying to get the contrast.  This tells us
## there are differences between the effect of
## some months on SST.
sst.a2.month %>% 
  emmeans( ~ Month) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

## this model was normally distributed
## so the coefs don't need to be back transformed
## but the contrasts need to be dealt with
sst.a2.month %>% 
  emmeans( ~ Month)

## This tells us that all months have a positive
## effect on SST. This was also done iwth 
## emmeans because we are only trying to get the contrast.
## change the HPD to 90% (the default is 95%) to match
## the rest of my analysis
plot(hpd.summary(prob = 90, emmeans(sst.a2.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
sst.em <- emmeans(sst.a2.month, ~ Month)


## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## There is no effect of wind direction on its own.
sst.a2.month %>% 
  emtrends(~CompRegWind, var = "RegWindSpd") %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## these are the coefs for 
## CompRegWind
sst.a2.month %>% 
  emtrends(~CompRegWind, var = "RegWindSpd")

## BEWARE this uses prob = 95% HPD 
## the rest of my analysis is at 90%
sst.trend <- emtrends(sst.a2.month, 
                        pairwise ~ CompRegWind, 
                        var = "RegWindSpd")

## This shows the relationship of each
## wind direction at 90% HPD
sst.a2.month %>%
  emtrends(~CompRegWind, var = "RegWindSpd") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = CompRegWind)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of of windspd*direction
## to do a sanity check on the slopes being the same.
emmip(sst.a2.month, CompRegWind ~ RegWindSpd, 
      cov.reduce = range)

acf(residuals(sst.a2.month))



###################  Model A2 RegZpl ####

prior.regzpl.a2 <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
                    set_prior("normal(1807, 903)", class = "b", coef = "NAO4yrlag"),
                    set_prior("normal(47277, 9455)", class = "b", coef = "CCBStrat"),
                    set_prior("normal(13753, 6976)", class = "b", coef = "SST"))


## this model runs quickly
regzpl.a2 <- brm(RegZpl ~ NAO4yrlag + SST + CCBStrat +  (~1|Year),
                  prior = prior.regzpl.a2, warmup = 1000, 
                  family = "gamma", 
                  cores = 4, 
                  iter = 5000, chains = 2,
                  control = list(stepsize = 0.01, 
                                 max_treedepth = 15, adapt_delta = 0.99),
                  data = all.data)

regzpl.a2.summary <- summary(regzpl.a2)
plot(regzpl.a2, waic = TRUE)
pp_check(regzpl.a2)
plot(marginal_effects(regzpl.a2), points = TRUE)
save(regzpl.a2, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpl.a2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpl.a2.rda")


conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_regzpl.a2 <- marginal_effects(
  regzpl.a2, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_regzpl.a2, ncol = 5, points = TRUE)



## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef
fixed_eff = data.frame(regzpl.a2.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`backtransformed est` =1/(fixed_eff$Estimate)
fixed_eff$`backtransformed low 95%` = 1/(fixed_eff$`l-95% CI`)
fixed_eff$`backtransformed high 95%` = 1/(fixed_eff$`u-95% CI`)
fixed_eff = fixed_eff %>% 
  dplyr::select(`Estimate`, `l-95% CI`, `u-95% CI`, 
                `backtransformed est`, 
                `backtransformed low 95%`,
                `backtransformed high 95%`)
regzpl.lag.table <- pander::pander(fixed_eff)

## standardize coefficients after transformation
# fixed_eff$`standardized coefficients` = c(NA, 
#                                           fixed_eff$`backtransformed est`[2]*sd(all.data$NAO4yrlag)/sd(all.data$RegZpl),
#                                           fixed_eff$`backtransformed est`[3]*sd(all.data$SST)/sd(all.data$RegZpl),
#                                           fixed_eff$`backtransformed est`[4]*sd(all.data$CCBStrat)/sd(all.data$RegZpl))
# 
 ## range standardize coefficients after transformation
# fixed_eff$`range standardized coefficients` = c(NA, 
#                                           fixed_eff$`backtransformed est`[2]*diff(range(all.data$NAO4yrlag))/diff(range(all.data$RegZpl)),
#                                           fixed_eff$`backtransformed est`[3]*diff(range(all.data$SST))/diff(range(all.data$RegZpl)),
#                                           fixed_eff$`backtransformed est`[4]*diff(range(all.data$CCBStrat))/diff(range(all.data$RegZpl)))
# 

## standardize coefficients 
fixed_eff$`standardized coefficients` = c(NA,
                                          fixed_eff$`Estimate`[2]*sd(all.data$NAO4yrlag)/sd(all.data$RegZpl),
                                          fixed_eff$`Estimate`[3]*sd(all.data$SST)/sd(all.data$RegZpl),
                                          fixed_eff$`Estimate`[4]*sd(all.data$CCBStrat)/sd(all.data$RegZpl))

## range standardize coefficients 
fixed_eff$`range standardized coefficients` = c(NA,
                                          fixed_eff$`Estimate`[2]*diff(range(all.data$NAO4yrlag))/diff(range(all.data$RegZpl)),
                                          fixed_eff$`Estimate`[3]*diff(range(all.data$SST))/diff(range(all.data$RegZpl)),
                                          fixed_eff$`Estimate`[4]*diff(range(all.data$CCBStrat))/diff(range(all.data$RegZpl)))




## try plotting with bayesplot
post <- posterior_samples(regzpl.a2)


mcmc_areas(
  post[, 2:4], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(regzpl.a2)

#### Model A2 RegZpl w/Month ####
prior.regzpl.a2 <- c(set_prior("cauchy(4773, 6703)", class = "Intercept"),
                     set_prior("normal(1807, 903)", class = "b", coef = "NAO4yrlag"),
                     set_prior("normal(47277, 9455)", class = "b", coef = "CCBStrat"),
                     set_prior("normal(13753, 6976)", class = "b", coef = "SST"))


## this model runs quickly
regzpl.a2.month <- brm(RegZpl ~ Month + NAO4yrlag + SST + CCBStrat +  (~1|Year),
                 prior = prior.regzpl.a2, warmup = 1000, 
                 family = "gamma", cores = 4, 
                 iter = 5000, chains = 2,
                 control = list(stepsize = 0.01, 
                                max_treedepth = 15, adapt_delta = 0.99),
                 data = all.data)

regzpl.a2.month.summary <- summary(regzpl.a2.month)
plot(regzpl.a2.month, waic = TRUE)
pp_check(regzpl.a2.month)
plot(marginal_effects(regzpl.a2.month), points = TRUE)
save(regzpl.a2.month, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpl.a2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpl.a2.month.rda")



## try plotting with bayesplot
post <- posterior_samples(regzpl.a2.month)


mcmc_areas(
  post[, 1:8], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(regzpl.a2.month)

regzpl.a2.month %>%
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
plot(hpd.summary(prob = 90, emmeans(regzpl.a2.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
zpl.em <- emmeans(regzpl.a2.month, ~ Month)

acf(residuals(regzpl.a2.month)[,1])



## back transform results. This model was
## gamma distributed with a log link
## so the coefs need to be exp().

reg.zpl.coefs <- data.frame(regzpl.a2.month.summary$fixed, check.names = F) %>%
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
reg.zpl.coefs.contrasts <- post %>%
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


####### Model 4 and 5 we likely won't use ################
###################  MODEL 4 for SEM A RegWindSpd  we likely won't use 
########### Original hypoth: Regional wind speed (2 month lag) is influenced by NAO (at some lag),
########### with a random effect of year. The test of d-separation for this linear
########### model indicated regional wind direction and regional SST also causes regional wind speed.
#################### D-seps indicate we can get rid of htis model


# prior.spd.3 <-  c(set_prior("normal(.39, 2)", class = "b", coef = "NAO"), ## this is the monthly NAO
#                   set_prior("normal(0, 10)", class = "b"), ## CompRegWind
#                   set_prior("normal(10, 5)", class = "b", coef = "SST"))
# 
# prior.spd.4 <- c(set_prior("normal(10,5)", class = "Intercept"),
#                  # set_prior("", class = "b", coef = "CompRegWind"),
#                  # set_prior("", class = "b", coef = "SST"),
#                  set_prior("normal(.63, .32)", class = "b", coef = "NAO"))
# 
# 
# RegWindSpd.mod <- brm(RegWindSpd ~ 1 + CompRegWind + NAO + SST + (1|Year),
#                       prior = prior.spd.4,
#                       warmup= 1000, inits = 0, cores = 4,
#                       iter = 2000, chains = 2, 
#                       control = list(stepsize = 0.01,
#                                      max_treedepth = 15, adapt_delta = 0.99),
#                       data = all.data)
# 
# 
# RegWindSpd.mod.form <- bf(RegWindSpd ~ NAO + RegWindDirec + sst + (1|Year), family = gaussian(link = "identity"))
# 
# 
# summary(RegWindSpd.mod, waic = TRUE)
# plot(RegWindSpd.mod, waic = TRUE)
# pp_check(RegWindSpd.mod)
# plot(marginal_effects(RegWindSpd.mod), points = TRUE)
# 
# ## show the marginal effects separately for each year
# conditions <- data.frame(Year = unique(all.data$Year))
# rownames(conditions) <- unique(all.data$Year)
# me_RegWindSpd.mod <- marginal_effects(
#   RegWindSpd.mod, conditions = conditions, 
#   re_formula = NULL, method = "predict")
# plot(me_RegWindSpd.mod, ncol = 5, points = TRUE)


###################  MODEL 5 for SEM A RegWindDirec  we likely won't use ####
########### Original hypoth: Regional wind direction (2 month lag) is influenced by the NAO
########### with a random effect of year. The test of d-separation for this linear
###########  model indicated sst also causes regional wind direction.
#################### D-seps indicate we can get rid of htis model


# prior.direc.3 <-  c(set_prior("normal(.39, 2)", class = "b", coef = "NAO"), ## monthly NAO
#                     set_prior("normal(10, 5)", class = "b", coef = "SST"))
# 
# prior.direc.4 <- c(set_prior("", class = "b", coef = "NAO"),
#                    set_prior("", class =  "b", coef = "SST"),
#                    set_prior("", class = "Intercept"))
# 
# 
# 
# RegWindDirc.mod <- brm(CompRegWind ~ NAO + SST + (~1|Year),
#                        #prior = prior.direc.3, 
#                        family = "categorical", 
#                        warmup= 1000, inits = 0, cores = 4,
#                        iter = 2000, chains = 2, 
#                        control = list(stepsize = 0.01,
#                                       max_treedepth = 15, adapt_delta = 0.99),
#                        data = all.data)
# 
# 
# RegWindDirec.mod.form <- bf(RegWindDirecRad ~ NAO + SST + (~1|Year),
#                             family = von_mises)
# 
# 
# 
# 
# summary(RegWindDirc.mod)
# plot(RegWindDirc.mod, waic = TRUE)
# pp_check(RegWindDirec.mod)
# plot(marginal_effects(RegWindDirc.mod))
# 
# conditions <- data.frame(Year = unique(all.data$Year))
# rownames(conditions) <- unique(all.data$Year)
# me_RegWindDirec.mod <- marginal_effects(
#   RegWindDirec.mod, conditions = conditions, 
#   re_formula = NULL, method = "predict")
# plot(me_RegWindDirec.mod, ncol = 5, points = TRUE)
#***************************************************************************************
################################### Model 4 and 5 we likely won't use ################








#################### Model 1 for SEM B1 ####
####################### This is the same as model B 
####################### except we remove Centro and Pseudo

prior.b1 <- c(set_prior("normal(406,30)", class = "b", coef = "pop.est"), 
             set_prior("cauchy(30, 3000)", class = "b", coef = "CalCCB"),
             set_prior("normal(5, 3)", class = "b", coef = "SST"),
             set_prior("cauchy(.25, 3)", class = "b", coef = "CCBStrat")) 



abund.gamma.hurd.B1 <- brm(eg.abund ~ pop.est + CalCCB + SST + 
                            CalCCB * CCBStrat + (1|Year), 
                          family = hurdle_gamma, prior = prior.b1,
                          warmup = 1000, inits = 0, cores = 4, 
                          iter = 2000, chains = 2, 
                          control = list(stepsize = 0.01, max_treedepth = 15,
                                         adapt_delta=0.99), data = all.data)



abund.gamma.hurd.form.B1 <- bf(eg.abund ~ pop.est + CalCCB + sst + CalCCB * CCBStrat + (1|Year), 
                                family = hurdle_gamma)



summary(abund.gamma.hurd.B1, waic = TRUE)
plot(abund.gamma.hurd.B1, waic = TRUE)
pp_check(abund.gamma.hurd.B1)
plot(marginal_effects(abund.gamma.hurd.B1), points = TRUE)

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_abund.gamma.hurd.B1 <- marginal_effects(
  abund.gamma.hurd.B1, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_abund.gamma.hurd.B1, ncol = 5, points = TRUE)



##################################################
#######################
#################### SEM B1
#######################
###################################################


SEM.B1 <- brm(abund.gamma.hurd.form.B1 +
               strat.gamma.hurd.form +
               cal.gamma.hurd.form.B +
               ##  RegWindSpd.mod.form +
               RegWindDirec.mod.form +
               ##  sst.mod.form,  
               set_rescor(FALSE), 
             warmup = 1000, inits = 0, cores = 4,
             iter = 2000, chains = 3, 
             control = list(stepsize = 0.01, max_treedepth = 20, 
                            adapt_delta = 0.99), data = all.data)

pp_check(SEM.B1, resp = 'egabund')
pp_check(SEM.B1, resp = 'CCBStrat')
pp_check(SEM.B1, resp = 'MTZ')

summary(SEM.B1, waic = TRUE)




##################################################################################
########################################
################### Model 2 SEM C
################### Model 1,3, 4, 5 are the same as SEM A
################### Original hypoth: CCB stratification influenced by an 
################### interaction between CCB wind speed and CCB wind direction
################### and SST.  The d-sep test indicated this is also influenced 
################### by regional wind speed
#######################################

prior.strat.C <- c(set_prior("normal(-0.5, .6)", class = "b", coef = "sinLocalWindDirecRad"), 
                 set_prior("normal(.2,.5)", class = "b", coef = "cosLocalWindDirecRad"),
                 set_prior("normal(7, 1.6)", class = "b", coef = "LocalWindSpd"),
                 set_prior("normal(8, 3)", class = "b", coef = "RegWindSpd"),
                 set_prior("normal(5, 3)", class = "b", coef = "SST"))

strat.gamma.hurd.c <- brm(CCBStrat ~ SST + sinLocalWindDirecRad * cosLocalWindDirecRad + LocalWindSpd + RegWindSpd + (1|Year), 
                        family =  hurdle_gamma, prior = prior.strat.C,
                      warmup = 1000, inits = 0, cores = 4,
                      iter = 2000, chains = 2, control = list(stepsize = 0.01, 
                      max_treedepth = 15, adapt_delta = 0.99), 
                      data = all.data)

strat.gamma.hurd.form.c <- bf(CCBStrat ~ sst + LocalWindDirec * LocalWindSpd + RegWindSpd + (1|Year), 
                              family =  hurdle_gamma)

summary(strat.gamma.hurd.c, waic = TRUE)
plot(strat.gamma.hurd.c, waic = TRUE)
pp_check(strat.gamma.hurd.c)
plot(marginal_effects(strat.gamma.hurd.c), points = TRUE)

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_strat.gamma.hurd.c <- marginal_effects(
  strat.gamma.hurd.c, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_strat.gamma.hurd.c, ncol = 5, points = TRUE)

##################################################################################
########################################
################### Model 6 SEM C
################### Model 1,3, 4, 5 are the same as SEM A
################### Original hypoth: Local wind speed is influenced by sst, the NAO
################### and local wind direction.  the d-sep test indicated 
################### regional wind direction was another influencer on local wind spd
#######################################

prior.local.spd <-  c(set_prior("normal(.39, 2)", class = "b", coef = "NAO"),
                ## RegWindDirec probably should not be a normal distribution
                set_prior("von_mises(253, 50)", class = "b", coef = "RegWindDirec"),
                set_prior("von_mises(246,88)", class = "b", coef = "LocalWindDirec"),
                set_prior("normal(5, 3)", class = "b", coef = "sst"))

LocalWindSpd.mod <- brm(LocalWindSpd ~ sst + NAO + LocalWindDirec + RegWindDirec +
                           (1|Year),
                      prior = prior.local.spd,
                      warmup= 1000, inits = 0, cores = 4,
                      iter = 2000, chains = 2, 
                      control = list(stepsize = 0.01,
                                     max_treedepth = 15, adapt_delta = 0.99),
                      data = all.data)

LocalWindSpd.mod.form <- bf(LocalWindSpd ~ sst + NAO + LocalWindDirec + RegWindDirec +
                              (1|Year))

summary(LocalWindSpd.mod)
plot(LocalWindSpd.mod, waic = TRUE)
pp_check(LocalWindSpd.mod)
plot(marginal_effects(LocalWindSpd.mod), points = TRUE)

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_LocalWindSpd.mod <- marginal_effects(
  LocalWindSpd.mod, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_LocalWindSpd.mod, ncol = 5, points = TRUE)

##################################################################################
########################################
################### Model 7 SEM C
################### Model 1,3, 4, 5 are the same as SEM A
################### Original hypoth: Local wind direction is influenced by the 
################### NAO and SST. 
#######################################

prior.local.direc <-  c(set_prior("normal(.39, 2)", class = "b", coef = "NAO"),
                  set_prior("normal(5, 3)", class = "b", coef = "sst"))


LocalWindDirc.mod <- brm(LocalWindDirec ~ NAO + sst + (~1|Year),
                       prior = prior.local.direc, family = von_mises,
                       warmup= 1000, inits = 0, cores = 4,
                       iter = 2000, chains = 2, 
                       control = list(stepsize = 0.01,
                                      max_treedepth = 15, adapt_delta = 0.99),
                       data = all.data)


LocalWindDirec.mod.form <- bf(LocalWindDirec ~ NAO + sst + (~1|Year),
                             prior = prior.local.direc, family = von_mises)


summary(LocalWindDirc.mod)
plot(LocalWindDirc.mod, waic = TRUE)
pp_check(LocalWindDirc.mod)
plot(marginal_effects(LocalWindDirc.mod), points = TRUE)

conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_LocalWindDirc.mod <- marginal_effects(
  LocalWindDirc.mod, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_LocalWindDirc.mod, ncol = 5, points = TRUE)



################################################
###############################################
###################  MODEL 4 for SEM E
###############################################
########### Original hypoth: Regional wind speed (2 month lag) is influenced by AO (at some lag),
########### with a random effect of year. The test of d-separation for this linear
########### model indicated regional wind direction and regional SST also causes regional wind speed.
##############################################
##############################################

prior.spd.E <-  c(set_prior("normal(.01, 1.1)", class = "b", coef = "AO"),
                ## RegWindDirec probably should not be a normal distribution
                set_prior("von_mises(253, 50)", class = "b", coef = "RegWindDirec"),
                set_prior("normal(5, 3)", class = "b", coef = "sst"))

RegWindSpd.mod.E <- brm(RegWindSpd ~ AO + RegWindDirec + sst + (1|Year),
                      prior = prior.spd.E,
                      warmup= 1000, inits = 0, cores = 4,
                      iter = 2000, chains = 2, 
                      control = list(stepsize = 0.01,
                                     max_treedepth = 15, adapt_delta = 0.99),
                      data = all.data)


RegWindSpd.mod.form.E <- bf(RegWindSpd ~ AO + RegWindDirec + sst + (1|Year), 
                          family = gaussian(link = "identity"))


summary(RegWindSpd.mod.E)
plot(RegWindSpd.mod.E, waic = TRUE)
pp_check(RegWindSpd.mod.E)
plot(marginal_effects(RegWindSpd.mod.E), points = TRUE)

## show the marginal effects separately for each year
conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_RegWindSpd.mod.E <- marginal_effects(
  RegWindSpd.mod.E, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_RegWindSpd.mod.E, ncol = 5, points = TRUE)

################################################
###############################################
###################  MODEL 5 for SEM E
###############################################
########### Original hypoth: Regional wind direction (2 month lag) is influenced by the AO
########### with a random effect of year. The test of d-separation for this linear
###########  model indicated sst also causes regional wind direction.
##############################################
##############################################

prior.direc.E <-  c(set_prior("normal(0.1, 1.1)", class = "b", coef = "AO"),
                  set_prior("normal(5, 3)", class = "b", coef = "sst"))


RegWindDirc.mod.E <- brm(RegWindDirec ~ AO + sst + (~1|Year),
                       prior = prior.direc.E, family = von_mises,
                       warmup= 1000, inits = 0, cores = 4,
                       iter = 2000, chains = 2, 
                       control = list(stepsize = 0.01,
                                      max_treedepth = 15, adapt_delta = 0.99),
                       data = all.data)


RegWindDirec.mod.form.E <- bf(RegWindDirec ~ AO + sst + (~1|Year),
                            family = von_mises)




summary(RegWindDirc.mod.E)
plot(RegWindDirc.mod.E, waic = TRUE)
pp_check(RegWindDirc.mod.E)
plot(marginal_effects(RegWindDirc.mod.E), points = TRUE)

conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_RegWindDirec.mod.E <- marginal_effects(
  RegWindDirc.mod.E, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_RegWindDirec.mod.E, ncol = 5, points = TRUE)

################################################
###############################################
###################  MODEL 6 for SEM E
###############################################
########### Original hypoth: Sea Surface Temperature is influenced by the AO at 
########### some time lag with a random effect of year. 
##############################################
##############################################

prior.sst.E <-  c(set_prior("normal(0.1,1.1)", class = "b", coef = "AO"))

sst.mod.E <- brm(SST ~ AO + (~1|Year),
               prior = prior.sst.E, 
               warmup = 1000, inits = 0, cores = 4,
               iter = 2000, chains =2, 
               control = list(stepsize = 0.01,
                              max_treedepth = 10, adapt_delta = 0.99),
               data = all.data)


sst.mod.form.E <- bf(sst ~ AO + (~1|Year), family = gaussian(link = "identity"))


summary(sst.mod.E, waic = TRUE)
plot(sst.mod.E, waic = TRUE)
pp_check(sst.mod.E)
plot(marginal_effects(sst.mod.E), points = TRUE)

conditions <- data.frame(Year = unique(all.data$Year))
rownames(conditions) <- unique(all.data$Year)
me_sst.mod.E <- marginal_effects(
  sst.mod.E, conditions = conditions, 
  re_formula = NULL, method = "predict")
plot(me_sst.mod.E, ncol = 5, points = TRUE)







################################################
###########################
############## Compare models
###########################
################################################


#WAIC(SEM.A, SEM.B, SEM.B1, SEM.C, SEM.E)
#LOO(SEM.A, SEM.B, SEM.B1, SEM.C, SEM.E)



########################           MINI SEM           ##############################
#######################   for relationship between     
####################   regzpl and eg abundance with month   

## this is very similar to SEM B model 1.  But we're going to bring month in as 
## a covariate.  

## this was run on the clusters
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/abund.mini.month.regzpl.rda")

#prior.b <- c(set_prior("cauchy(50,50)", class = "Intercept"),
#             set_prior("normal(.32, .16)", class = "b", coef = "pop.est"), 
             #set_prior("", class = "b", coef = "CalCCB*mtz.patchiness"),
#             set_prior("normal(0.003, 0.0015)", class = "b", coef = "CalCCB"),
#             set_prior("normal(8.5, 4.25)", class = "b", coef = "mtz.patchiness"),
#             set_prior("normal(0, 12)", class = "b", coef = "SST"),
#             set_prior("normal(0, 0.00045)", class = "b", coef = "RegZpl"),
#             set_prior("normal(0.025, 0.0125)", class = "b", dpar = "hu", coef = "mtz.patchiness"),
#             set_prior("normal(-0.04, 0.02)", class = "b", dpar = "hu", coef = "SST"),
#             set_prior("normal(0.00001, 0.000005)", class = "b", dpar = "hu", coef = "CalCCB"),
#             set_prior("normal(0.001, 0.0005)", class = "b", dpar = "hu", coef = "pop.est"))


#model_formula_mini = eg.abund ~ SST + pop.est + mtz.patchiness*CalCCB + Month*RegZpl + (1|Year)
#model_formula_hu_mini = update(model_formula_mini,  hu ~ . )
#model_formula_mini = bf(model_formula_mini, model_formula_hu_mini)

#abund.gamma.hurd.mini <- brm(model_formula_mini, 
#                          family = 'hurdle_gamma', prior = prior.b,
#                          warmup = 500, inits = 0, cores = 4, 
#                          iter = 1000, chains = 2, 
 #                         control = list(stepsize = 0.01, max_treedepth = 20,
#                                         adapt_delta=0.99), data = all.data)

abund.mini.summary <- summary(abund.mini.month.regzpl, waic = TRUE)
plot(abund.mini.month.regzpl, waic = TRUE)
pp_check(abund.mini.month.regzpl)
p <- plot(marginal_effects(abund.mini.month.regzpl), spaghetti = T, 
          nsamps = 200, points = TRUE)


plot(p)$eg.abund + ggplot2::ylim(0,300)
p[[8]] + ggplot2::ylim(0,300) +
  facet_wrap(~Month)


## back transform results from gamma
## BRMS uses inverse link for gamma family so to 
## back transform 1/coef.  the hurdle part needs to be exp. 
fixed_eff = data.frame(abund.mini.summary$fixed, check.names = F)
fixed_eff$Est.Error = fixed_eff$Eff.Sample = fixed_eff$Rhat = NULL
fixed_eff$`Odds ratio` =c(NA, exp(fixed_eff$Estimate[2]),
                          rep(NA, 14),
                          exp(fixed_eff$Estimate[17:30]))
fixed_eff$`OR low 95%` = c(NA, exp(fixed_eff$`l-95% CI`[2]),
                           rep(NA,14),
                           exp(fixed_eff$`l-95% CI`[17:30]))
fixed_eff$`OR high 95%` = c(NA, exp(fixed_eff$`u-95% CI`[2]),
                            rep(NA,14),
                            exp(fixed_eff$Estimate[17:30]))
fixed_eff$`backtransformed est` = c(1/(fixed_eff$Estimate[1]), 
                                    NA,
                                    1/fixed_eff$Estimate[3:16],
                                    rep(NA, 14))
fixed_eff$`backtransformed low 95%` = c(1/(fixed_eff$`l-95% CI`[1]), 
                                        NA,
                                        1/fixed_eff$`l-95% CI`[3:16],
                                        rep(NA, 14))
fixed_eff$`backtransformed high 95%` = c(1/(fixed_eff$`u-95% CI`[1]), 
                                         NA,
                                         1/fixed_eff$`u-95% CI`[3:16],
                                         rep(NA, 14))

fixed_eff = fixed_eff %>% 
  dplyr::select(`Odds ratio`, `OR low 95%`, `OR high 95%`, `backtransformed est`, 
                `backtransformed low 95%`,
                `backtransformed high 95%`)
 pander::pander(fixed_eff)

## standardize coefficients after transformation
fixed_eff$`standardized coefficients` =c(NA, NA,
                                         fixed_eff$`backtransformed est`[3]*sd(all.data$pop.est)/sd(all.data$eg.abund),
                                         fixed_eff$`backtransformed est`[4]*sd(all.data$SST)/sd(all.data$eg.abund),
                                         fixed_eff$`backtransformed est`[5]*sd(all.data$mtz.patchiness)/sd(all.data$eg.abund),
                                         fixed_eff$`backtransformed est`[6]*sd(all.data$CalCCB)/sd(all.data$eg.abund),
                                         rep(NA, 4),
                                         fixed_eff$`backtransformed est`[11]*sd(all.data$RegZpl)/sd(all.data$eg.abund), 
                                         rep(NA,19))



## standardize binary coefficients
# Extract predicted values on the link scale
preds <- predict(abund.mini.month.regzpl, type = "link")

# Compute sd of error variance using theoretical variances
sd.y.LT <- sqrt(var(preds) + pi^2/3)

# Compute sd of x
sd.pop.est <- sd(all.data$pop.est)
sd.SST <- sd(all.data$SST)
sd.patchiness <- sd(all.data$mtz.patchiness)
sd.CalCCB <- sd(all.data$CalCCB)
sd.RegZpl <- sd(all.data$RegZpl)


# extract beta coefs
Beta.glm.pop <- fixed_eff[17,1]
Beta.glm.SST <- fixed_eff[18,1]
Beta.glm.patchiness <- fixed_eff[19, 1]
Beta.glm.CalCCB <- fixed_eff[20, 1]
Beta.glm.regzpl <- fixed_eff[25, 1]

## range standardize coefficients after transformation
fixed_eff$`range standardized coefficients` =c(NA, NA,
                                         fixed_eff$`backtransformed est`[3]*diff(range(all.data$pop.est))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`backtransformed est`[4]*diff(range(all.data$SST))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`backtransformed est`[5]*diff(range(all.data$mtz.patchiness))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`backtransformed est`[6]*diff(range(all.data$CalCCB))/diff(range(all.data$eg.abund)),
                                         fixed_eff$`backtransformed est`[7]*.5/diff(range(all.data$eg.abund)),
                                         fixed_eff$`backtransformed est`[8]*.5/diff(range(all.data$eg.abund)),
                                         fixed_eff$`backtransformed est`[9]*.5/diff(range(all.data$eg.abund)),
                                         fixed_eff$`backtransformed est`[10]*.5/diff(range(all.data$eg.abund)),
                                         fixed_eff$`backtransformed est`[11]*diff(range(all.data$RegZpl)/sd(all.data$eg.abund)), 
                                         rep(NA,5),
                                         Beta.glm.pop * sd.pop.est / sd.y.LT[1],
                                         Beta.glm.SST * sd.SST / sd.y.LT[1],
                                         Beta.glm.patchiness * sd.patchiness / sd.y.LT[1],
                                         Beta.glm.CalCCB * sd.CalCCB /sd.y.LT[1],
                                         rep(NA, 4), 
                                         Beta.glm.regzpl * sd.RegZpl/sd.y.LT[1], 
                                         rep(NA, 5))



## is there an effect of the contrasts?
post.mini.a <- 
  abund.mini.month.regzpl %>%
  posterior_samples()

## compute averages for each category and 
## summarize the results with the transpose of base R’s apply() function, 
## rounding to two digits of precision.
post.mini.a$mu_Month1 <- post.mini.a$b_Intercept
post.mini.a$mu_Month2 <- post.mini.a$b_Intercept + post.mini.a$b_Month2
post.mini.a$mu_Month3 <- post.mini.a$b_Intercept + post.mini.a$b_Month3
post.mini.a$mu_Month4   <- post.mini.a$b_Intercept + post.mini.a$b_Month4
post.mini.a$mu_Month5   <- post.mini.a$b_Intercept + post.mini.a$b_Month5

## does month have an effect (gamma part)
round(t(apply(post.mini.a[ ,75:79], 2, quantile, c(.5, .05, .95))), digits = 2)

post.mini.a$mu_hu_Month1 <- post.mini.a$b_hu_Intercept
post.mini.a$mu_hu_Month2 <- post.mini.a$b_hu_Intercept + post$b_hu_Month2
post.mini.a$mu_hu_Month3 <- post.mini.a$b_hu_Intercept + post$b_hu_Month3
post.mini.a$mu_hu_Month4 <- post.mini.a$b_hu_Intercept + post$b_hu_Month4
post.mini.a$mu_hu_Month5 <- post.mini.a$b_hu_Intercept + post$b_hu_Month5

## does month have an effect (hurdle part)
round(t(apply(post.mini.a[ ,80:84], 2, quantile, c(.5, .05, .95))), digits = 2)



## try plotting with bayesplot
post <- posterior_samples(abund.mini.month.regzpl)

mcmc_areas(
  post[, 3:30], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
)

bayes_R2(abund.mini.month.regzpl)

## interpreting the interaction terms
## are the slopes of the relationship between regzpl*month
## really different for each month

post %>%
  transmute(gamma_may = b_RegZpl + `b_Month5:RegZpl`,
    gamma_april = b_RegZpl + `b_Month4:RegZpl`,
    gamma_march = b_RegZpl + `b_Month3:RegZpl`,
    gamma_Feb = b_RegZpl + `b_Month2:RegZpl`,
    gamma_Jan = b_RegZpl) %>%
  gather(key, value) %>%
  group_by(key) %>%
  summarise(mean = mean(value))


post %>%
  mutate(gamma_may = b_RegZpl + `b_Month5:RegZpl`,
         gamma_april = b_RegZpl + `b_Month4:RegZpl`,
         gamma_march = b_RegZpl + `b_Month3:RegZpl`,
         gamma_Feb = b_RegZpl + `b_Month2:RegZpl`,
         gamma_Jan = b_RegZpl) %>% 
  transmute(may_april_diff = gamma_may - gamma_april,
            may_march_diff = gamma_may - gamma_march,
            may_feb_diff = gamma_may - gamma_Feb,
            may_jan_diff = gamma_may - gamma_Jan) %>%
  summarise(May_april_Proportion_of_the_difference_below_0 = sum(may_april_diff < 0) / length(may_april_diff),
            May_Jan_Proportion_of_the_difference_below_0 = sum(may_jan_diff < 0) / length(may_jan_diff))

## plot the distribution of slopes for the
## interaction between regzpl and month
post %>%
  transmute(gamma_may = b_RegZpl + `b_Month5:RegZpl`,
            gamma_april = b_RegZpl + `b_Month4:RegZpl`,
            gamma_march = b_RegZpl + `b_Month3:RegZpl`,
            gamma_Feb = b_RegZpl + `b_Month2:RegZpl`,
            gamma_Jan = b_RegZpl) %>%
  gather(key, value) %>%
  ggplot(aes(x = value, group = key, color = key, fill = key)) +
  geom_density(alpha = 1/4) +
  scale_x_continuous(expression(gamma), expand = c(0, 0)) +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Month and RegZpl slopes") 


#### MINI #2 SST*RegZPL ####
## Now we do a very similar to above but we remove month and make the 
# SST be the interaction with RegZpl.

## LOad from the clusters
## this was updated with the new pop est data
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/abund.gamma.hurd.mini_updated_pop_est_data.rda")


#prior.b <- c(set_prior("cauchy(50,50)", class = "Intercept"),
#             set_prior("normal(.32, .16)", class = "b", coef = "pop.est"), 
             #set_prior("", class = "b", coef = "CalCCB*mtz.patchiness"),
#             set_prior("normal(0.003, 0.0015)", class = "b", coef = "CalCCB"),
#             set_prior("normal(8.5, 4.25)", class = "b", coef = "mtz.patchiness"),
#             set_prior("normal(0, 12)", class = "b", coef = "SST"),
#             set_prior("normal(0, 0.00045)", class = "b", coef = "RegZpl"),
#             set_prior("normal(0.025, 0.0125)", class = "b", dpar = "hu", coef = "mtz.patchiness"),
#             set_prior("normal(-0.04, 0.02)", class = "b", dpar = "hu", coef = "SST"),
##             set_prior("normal(0.00001, 0.000005)", class = "b", dpar = "hu", coef = "CalCCB"),
#             set_prior("normal(0.001, 0.0005)", class = "b", dpar = "hu", coef = "pop.est"))


#model_formula_mini = eg.abund ~ pop.est + mtz.patchiness*CalCCB + SST*RegZpl + (1|Year)
#model_formula_hu_mini = update(model_formula_mini,  hu ~ . )
#model_formula_mini = bf(model_formula_mini, model_formula_hu_mini)

#abund.gamma.hurd.mini <- brm(model_formula_mini, 
#                             family = 'hurdle_gamma', prior = prior.b,
##                             warmup = 1000, inits = 0, cores = 4, 
#                             iter = 4000, chains = 2, 
#                             control = list(stepsize = 0.01, max_treedepth = 20,
#                                            adapt_delta=0.99), data = all.data)




abund.mini.summary <- summary(abund.gamma.hurd.mini, waic = TRUE)
plot(abund.gamma.hurd.mini, waic = TRUE)
pp_check(abund.gamma.hurd.mini)
plot(marginal_effects(abund.gamma.hurd.mini), points = TRUE)
  

## try plotting with bayesplot
post <- posterior_samples(abund.gamma.hurd.mini)


mcmc_areas(
  post[, 3:16], 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
)

bayes_R2(abund.gamma.hurd.mini)





## change around some of the ylimits and facets
p <- plot(marginal_effects(abund.gamma.hurd.mini)) 
plot(p)$eg.abund + ggplot2::ylim(0,300)
p[[8]] + ggplot2::ylim(0,300) +
  facet_wrap(~Month)

int_conditions <- list(
  CalCCB = setNames(c(36.5, 320.4, 4758.4), c("1st qu", "median", "3rd qu"))
)
p<-marginal_effects(abund.gamma.hurd.mini, "mtz.patchiness:CalCCB", 
                    int_conditions = int_conditions)

int_conditions <- list(
  RegZpl = setNames(c(13729, 25548, 48514), c("1st qu", "median", "3rd qu"))
)
p<-marginal_effects(abund.gamma.hurd.mini, effects = "Month:RegZpl",
                 int_conditions = int_conditions)



#### MINI 3 ####
## Dan suggested combining calanus and pseudocalanus and trying that
## this was run on the clusters
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/abund.gamma.hurd.mini.3.rda")
# all.data$Cal.Pseudo <- all.data$CalCCB + all.data$Pseudo
# 
# 
# prior.b <- c(set_prior("cauchy(50,50)", class = "Intercept"),
#             set_prior("normal(.32, .16)", class = "b", coef = "pop.est"),
#             set_prior("normal(0.003, 0.0015)", class = "b", coef = "Cal.Pseudo"),
#             set_prior("normal(8.5, 4.25)", class = "b", coef = "mtz.patchiness"),
#             set_prior("normal(0, 12)", class = "b", coef = "SST"),
#             set_prior("normal(0, 0.00045)", class = "b", coef = "RegZpl"),
#             set_prior("normal(0.025, 0.0125)", class = "b", dpar = "hu", coef = "mtz.patchiness"),
#             set_prior("normal(-0.04, 0.02)", class = "b", dpar = "hu", coef = "SST"),
#             set_prior("normal(0.00001, 0.000005)", class = "b", dpar = "hu", coef = "Cal.Pseudo"),
#             set_prior("normal(0.001, 0.0005)", class = "b", dpar = "hu", coef = "pop.est"))
# 
# 
# model_formula_mini = eg.abund ~ SST + pop.est + mtz.patchiness*Cal.Pseudo + Month*RegZpl + (1|Year)
# model_formula_hu_mini = update(model_formula_mini,  hu ~ . )
# model_formula_mini = bf(model_formula_mini, model_formula_hu_mini)
# 
# abund.gamma.hurd.mini.3 <- brm(model_formula_mini,
#                          family = 'hurdle_gamma', prior = prior.b,
#                          warmup = 500, inits = 0, cores = 4,
#                          iter = 1000, chains = 2,
#                         control = list(stepsize = 0.01, max_treedepth = 20,
#                                         adapt_delta=0.99), data = all.data)
# 
abund.mini.summary.3 <- summary(abund.gamma.hurd.mini.3, waic = TRUE)
plot(abund.gamma.hurd.mini.3, waic = TRUE)
pp_check(abund.gamma.hurd.mini.3)
p <- plot(marginal_effects(abund.gamma.hurd.mini.3), points = TRUE)

p[[8]] + ggplot2::ylim(0,300) +
  facet_wrap(~Month)


## try plotting with bayesplot
post <- posterior_samples(abund.gamma.hurd.mini.3)


mcmc_areas(
  post, pars = c("b_SST"), 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
)

bayes_R2(abund.gamma.hurd.mini.3)
