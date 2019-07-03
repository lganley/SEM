library(brms)
library(bayesplot)
library(tidybayes)
library(tidyverse)

## Originally we ran all of the abundance models as gamma hurdle models
## but now we think they should be zero inflated negative binomial because
## gamma hurdles can't have TRUE zeros (and we can have surveys in which
## there really are zero whales present).

## load in the data
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/all.data.rda")

## first we need to make the abundance estimates whole numbers
## Jarrett suggested just rounding them to start.
all.data$whole.abund <- round(all.data$eg.abund)

## ZINB for abundance Model A ####
## these are the priors explained in teh blue notebook
# prior4 <- c(set_prior("cauchy(50, 50)", class = "Intercept"),
#             set_prior("normal(0.0025, 0.001 )", class = "b", coef = "MTZ"), 
#             set_prior("normal(.32, .16)", class = "b", coef = "pop.est"),
#             set_prior("normal(0, 12)", class = "b", coef = "SST"),
#             set_prior("normal(120, 60)", class = "b", coef = "MTZ:CCBStrat"),
#             set_prior("normal(42, 42)", class = "b", coef = "CCBStrat"),
#             set_prior("normal(0,0.00045)", class = "b", coef = "RegZpl"))
# 
# ## this takes about 5 hours to run
# zinba <- brm(whole.abund ~ pop.est + SST + MTZ * CCBStrat + RegZpl + (1|Year), 
#                  data = all.data, prior = prior4, 
#                  family = zero_inflated_negbinomial(),
#                  warmup = 1000, inits = 0, cores = 4,
#                  control = list(stepsize = 0.01, max_treedepth = 15, 
#                  adapt_delta = 0.99),
#                              iter = 4000, chains = 4)
# 
# abund.summary <- summary(zinba, waic = TRUE)
# plot(zinba, waic = TRUE)
# pp_check(zinba) 
# plot(marginal_effects(zinba), points = TRUE)
# save(zinba, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/zinba.rda")
# load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/zinba.rda")
# ## try plotting with bayesplot
# post.zinba <- posterior_samples(zinba)
# 
# 
# mcmc_areas(
#   post.zinba[, 2:3], 
#   prob = 0.9, # 90% intervals thick parts
#   prob_outer = 0.99, # 99% thinner outer line
#   point_est = "mean"
# ) +
#   ggplot2::labs(
#     title = "Posterior distributions",
#     subtitle = "with means and 90% intervals"
#   )
# 
# bayes_R2(zinba)


## ZINB for abundance Model A w/month ####
## this was run on the clusters (see zinb.a.R)
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/zinba.month.rda")

abund.summary <- summary(zinba.month, waic = TRUE)
plot(zinba.month, waic = TRUE)
pp_check(zinba.month) 
p<-plot(marginal_effects(zinba.month), points = TRUE)
## plot marginal effects under different conditions
conditions <- data.frame(Month = c("1", "2", "3", "4", "5"))
plot(marginal_effects(zinba.month, "RegZpl", 
                      conditions = conditions), points = TRUE) +
  ggplot2::ylim(0,600)

p <- plot(marginal_effects(zinba.month), points = TRUE)

## facet regzpl by month
plot(marginal_effects(zinba.month, "SST", conditions = conditions), points = TRUE) 
  ggplot2::ylim(0,600) 

p[[6]] + 
  ggplot2::ylim(0,400)



## try plotting with bayesplot
post.zinba <- posterior_samples(zinba.month)


mcmc_areas(
  post.zinba, pars = c("b_RegZpl"),
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(zinba.month)


## Does the interaction between RegZpl and month
## influence abundance
zinba.month %>%
  emtrends(~RegZpl*Month, var="RegZpl",
           at=list(RegZpl = c(1005, 302579))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

## BEWARE this uses prob = 95% HPD 
## the rest of my analysis is at 90%
abund.trend <- emtrends(zinba.month, 
                        pairwise ~ Month, 
                        var = "RegZpl")
## back transform coefs for Month
## this model was zinb. so coefs
## need to be exp
regzpl.coef <- as_tibble(abund.trend$emtrends) %>%
  mutate(exp(RegZpl.trend))

## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## There is a difference in the effect of Month
## as you vary RegZpl.
zinba.month %>% 
  emtrends(~Month, var = "RegZpl") %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


## This gives the main effect of month on its own
## This averages over the values of RegZpl
emmeans(zinba.month,  ~ Month)

plot(hpd.summary(prob = 90, emmeans(zinba.month, ~ Month))) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## This gives teh main effect of RegZpl when all
## teh months are averaged.  
emmeans(zinba.month,  ~ RegZpl)

plot(hpd.summary(prob = 90, emmeans(zinba.month, ~ RegZpl))) +
  geom_vline(xintercept = 0, color = "red", lty=2)


## This shows the relationship of each
## month as you vary RegZpl 90% HPD
zinba.month %>%
  emtrends(~Month, var = "RegZpl") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = Month)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of of month*regzpl
## to do a sanity check on the slopes being the same.
emmip(zinba.month, Month ~ RegZpl, 
      cov.reduce = range)

## Does the interaction between CCBStrat and MTZ
## influence abundance.  Because both are continuous
## check the CCBStrat vs MTZ and then the 
## MTZ vs CCBStrat to determine if there is an
## interaction
zinba.month %>%
  emtrends(~CCBStrat*MTZ, var="MTZ",
           at=list(MTZ = c(536, 59315))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  theme(axis.text.y = element_text(angle = 45)) +
  geom_vline(xintercept = 0, color = "red", lty=2)

zinba.month %>%
  emtrends(~CCBStrat*MTZ, var="CCBStrat",
           at=list(CCBStrat = c(0.04, 3.2))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  theme(axis.text.y = element_text(angle = 45)) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of mtz*strat
## to do a sanity check on the slopes being the same.
emmip(zinba.month, MTZ ~ CCBStrat, 
      cov.reduce = range)

emmip(zinba.month, CCBStrat ~ MTZ, 
      cov.reduce = range)

zinba.month %>%
  emtrends(~CCBStrat, var = "MTZ") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = CCBStrat)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)


acf(residuals(zinba.month)[,1])



## back transform coefs.
## in brms the Zi part is already back transformed
## so the probability of no whales is .11
abund.a.coefs <- as_tibble(abund.summary$fixed) %>%
                      slice(1:2) %>%
                      cbind(coefs = c("pop.est", "SST")) %>%
                      mutate(exp(Estimate))


## interpretation of raw coefs from interaction
## CCBStrat: increasing CCBStrat by 1 in January
## at the mean MTZ value will decrease abundance
## by -0.32.
## MTZ: increasing MTZ by 1 in January at the
## mean value of CCBStrat will increase abundance
## by 0.
## CCBStrat*MTZ: The effect of CCBStrat on
## abundance in January increases by 0 for 
## every unit increase in MTZ.  

## ZINB for abundance Model B ####
prior.b2 <- c(set_prior("cauchy(50,50)", class = "Intercept"),
              set_prior("normal(.32, .16)", class = "b", coef = "pop.est"), 
              set_prior("normal(0, 0.00045)", class = "b", coef = "RegZpl"),
              set_prior("normal(8.5, 4.25)", class = "b", coef = "mtz.patchiness"),
              set_prior("normal(0, 12)", class = "b", coef = "SST"))


zinbb <- brm(whole.abund ~ SST + pop.est + RegZpl + mtz.patchiness + (1|Year), 
                           family =zero_inflated_negbinomial(),
                           prior = prior.b2,
                           warmup = 1000, inits = 0, cores = 4, 
                           iter = 4000, chains = 4, 
                           control = list(stepsize = 0.01, max_treedepth = 15,
                                          adapt_delta=0.99), data = all.data)

zinbb.summary <- summary(zinbb, waic = TRUE)
plot(zinbb, waic = TRUE)
pp_check(zinbb)
plot(marginal_effects(zinbb), points = TRUE)
save(zinbb, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/zinbb.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/zinbb.rda")

## try plotting with bayesplot
post.zinbb <- posterior_samples(zinbb)


mcmc_areas(
  post.zinbb, pars = c("b_mtz.patchiness"), 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(zinbb)


#### ZINB for abundance Model B w/month ####
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/zinbb.month.rda")


zinbb.summary <- summary(zinbb.month, waic = TRUE)
plot(zinbb.month, waic = TRUE)
pp_check(zinbb.month)

conditions <- data.frame(Month = c("1", "2", "3", "4", "5"))
plot(marginal_effects(zinbb.month, "SST",
     conditions = conditions), points = TRUE)

## facet regzpl by month
p[[8]] +  
  facet_wrap(~Month) +
  ggplot2::ylim(0,600) 

## try plotting with bayesplot
post.zinbb <- posterior_samples(zinbb.month)


mcmc_areas(
  post.zinbb, pars = c("b_RegZpl"), 
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )

bayes_R2(zinbb.month)

## Does the interaction between RegZpl and month
## influence abundance.
## Yes.
zinbb.month %>%
  emtrends(~RegZpl*Month, var="RegZpl",
           at=list(RegZpl = c(1005, 302579))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

emtrends(zinbb.month, ~RegZpl | Month, 
         var = "RegZpl", at = list(RegWindSpd = c(1005,302579)))

## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## THis shows the difference in the effect of month
## as you vary RegZpl
zinbb.month %>% 
  emtrends(~Month, var = "RegZpl") %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))

## BEWARE this uses prob = 95% HPD 
## the rest of my analysis is at 90%
zinbb.trend.month <- emtrends(zinbb.month, 
                        pairwise ~ Month, 
                        var = "RegZpl")


## This shows the relationship of each
## month at 90% HPD
zinbb.month %>%
  emtrends(~Month, var = "RegZpl") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = Month)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of of month*regzpl
## to do a sanity check on the slopes being different.
emmip(zinbb.month, Month ~ RegZpl, 
      cov.reduce = range)



## this shows us that RegZpl by itself doesn't influence.
## abundance.  But this is part of an interaction sso these
## results may be misleading
plot(hpd.summary(prob = 90, emmeans(zinbb.month, ~ RegZpl))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
sst.em.wind <- emmeans(zinbb.month, ~ CompRegWind)


## Get the main effect of wind speed at its mean
## over hte average of all wind directions.
plot(hpd.summary(prob = 90, emmeans(zinbb.month, ~Month))) +
  geom_vline(xintercept = 0, color = "red", lty = 2)

emmeans(zinbb.month, ~Month)


acf(residuals(zinbb.month)[,1])

## ZINB for abundance Model C ####

# eg.abund.C.prior <- c(set_prior("cauchy(50, 50)", class = "Intercept"),
#                      set_prior("normal(.32, .16)", class = "b", coef = "pop.est"),
#                      set_prior("normal(8.5, 4.25)", class = "b", coef = "mtz.patchiness"),
#                      set_prior("normal(0, 0.00045)", class = "b", coef = "RegZpl"),
#                      set_prior("normal(42, 42)", class = "b", coef = "CCBStrat"))
# 
# 
# 
# zinbc <- brm(formula = whole.abund ~ CCBStrat + SpringDate + pop.est + mtz.patchiness + RegZpl + (1|Year),
#                   family = zero_inflated_negbinomial(), 
#                   prior = eg.abund.C.prior,
#                   warmup = 1000, inits = 0,
#                   iter = 4000, chains = 4, cores = 4,
#                   control = list(stepsize = 0.01, max_treedepth = 15,
#                                  adapt_delta=0.99), data = all.data)
# 
# 
# zinbc.summary <- summary(zinbc, waic = TRUE)
# plot(zinbc, waic1 = TRUE)
# pp_check(zinbc)
# plot(marginal_effects(zinbc), points = TRUE)
# bayes_R2(zinbc)
# save(zinbc, file = "/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/zinbc.rda")
# load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/zinbc.rda")
# 
# ## try plotting with bayesplot
# post.zinbc <- posterior_samples(zinbc)
# 
# 
# mcmc_areas(
#   post.zinbc, pars = c("b_RegZpl"), 
#   prob = 0.9, # 90% intervals thick parts
#   prob_outer = 0.99, # 99% thinner outer line
#   point_est = "mean"
# ) +
#   ggplot2::labs(
#     title = "Posterior distributions",
#     subtitle = "with means and 90% intervals"
#   )
# 
# 

## ZINB for abundance Model C w/month ####
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/zinbc.month.rda")


zinbc.summary <- summary(zinbc.month, waic = TRUE)
plot(zinbc.month, waic1 = TRUE)
pp_check(zinbc.month)
conditions <- data.frame(Month = c("1", "2", "3", "4", "5"))
p <- plot(marginal_effects(zinbc.month, conditions = conditions), points = TRUE)


p[[1]] +
  xlab("Stratification") +
  ylab("Cape Cod Bay right whale abundance")

p[[2]] +
  xlab("Spring Transition Date") +
  ylab("Cape Cod Bay right whale abundance")

p[[3]] +
  xlab("Population size") +
  ylab("Cape Cod Bay right whale abundance")

p[[4]] +
  xlab("Zooplankton patchiness") +
  ylab("Cape Cod Bay right whale abundance")

p2 <- plot(marginal_effects(zinbc.month), points = TRUE)

p2[[5]] +
  ylab("Cape Cod Bay right whale abundance")



p3 <- plot(marginal_effects(zinbc.month, 
                            "RegZpl:Month"), 
           points = TRUE)
p3[[1]] + 
    facet_wrap(~Month) +
    xlab("Regional Calanus finmarchicus") +
    ylab("Cape Cod Bay right whale abundance") +
    ggplot2::ylim(0,800)



 

bayes_R2(zinbc.month)

## try plotting with bayesplot
post.zinbc <- posterior_samples(zinbc.month)


mcmc_areas(
  post.zinbc, pars = c("b_RegZpl"),
  prob = 0.9, # 90% intervals thick parts
  prob_outer = 0.99, # 99% thinner outer line
  point_est = "mean"
) +
  ggplot2::labs(
    title = "Posterior distributions",
    subtitle = "with means and 90% intervals"
  )


bayes_R2(zinbc.month)



## Does the interaction between RegZpl and month
## influence abundance
## Yes.
zinbc.month %>%
  emtrends(~RegZpl*Month, var="RegZpl",
           at=list(RegZpl = c(1005, 302579))) %>%  
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90))


## Here we use emtrends because this is  
## part of an interaction between a factor variable
## adn a continuous covariate.  
## There is an effect of Month on its own.
zinbc.month %>% 
  emtrends(~Month, var = "RegZpl") %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh(.width = c(.90)) +
  geom_vline(xintercept = 0, color = "red", lty=2)

## This shows the relationship of each
## wind direction at 90% HPD
zinbc.month %>%
  emtrends(~Month, var = "RegZpl") %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = .value, y = Month)) +
  geom_halfeyeh(.width = c(.90))  +
  geom_vline(xintercept = 0, color = "red", lty=2)

## plot a linear version of of windspd*direction
## to do a sanity check on the slopes being the same.
emmip(zinbc.month, Month ~ RegZpl, 
      cov.reduce = range)


## this shows us that RegZpl by itself does influence.
## abundance.  But this is part of an interaction sso these
## results may be misleading
plot(hpd.summary(prob = 90, emmeans(zinbc.month, ~ RegZpl))) +
  geom_vline(xintercept = 0, color = "red", lty=2)
sst.em.wind <- emmeans(zinbc.month, ~ CompRegWind)


## Get the main effect of wind speed at its mean
## over hte average of all wind directions.
plot(hpd.summary(prob = 90, emmeans(zinbc.month, ~Month))) +
  geom_vline(xintercept = 0, color = "red", lty = 2)

emmeans(zinbc.month, ~Month)


acf(residuals(zinbc.month)[,1])

