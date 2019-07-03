library(tidyverse)
library(brms)
library(ggplot2)
library(rcartocolor)

## load in the data
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/all.data.rda")

## first we need to make the abundance estimates whole numbers
## Jarrett suggested just rounding them to start.
all.data$whole.abund <- round(all.data$eg.abund)

## load models

## Model A
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/abund.gamma.hurd.a.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/strat.gamma.hurd.fac.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/mtz.gamma.hurd.2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.a2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpl.a2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/regzpl.a2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/sst.a2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/strat.A.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/zinba.month.rda")

## Model B
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/abund.gamma.hurd.B2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.b2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.b2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/cal.gamma.hurd.B2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/mtz.patchy.b.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/Pseudo.b.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/zinbb.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.b2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.b2.month.rda")

## Model C
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/eg.abund.C.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/patchy2.C.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.C2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.C2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/chl.C2.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/mtz.C2.noint.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/from_clusters/zinbc.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/patchy.C.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/CCBStrat.C2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/RegZpl.C2.month.rda")
load("/Users/laura.ganley001/Documents/R_Projects/SEM/saved_R/chl.C2.month.rda")


waic_tibble <- function(...){
  tibble(model_name = rep(waic(...)$model_name, times = 3)) %>% 
    bind_cols(
      waic(...)$estimates %>% 
        data.frame() %>% 
        rownames_to_column() %>% 
        rename(statistic = rowname,
               estimate  = Estimate,
               se        = SE)
    )
}

####Total WAIC Model A w/ Gamma Hurdle####
a.waic <- sum(waic(abund.gamma.hurd) %>%
             pluck("waic"),
           waic(strat.gamma.hurd.fac) %>%
             pluck("waic"),
           waic(mtz.gamma.hurd.2) %>%
             pluck("waic"),
           waic(sst.a2) %>%
             pluck("waic"),
           waic(regzpl.a2) %>%
             pluck("waic"))

#### TOTAL WAIC SE Model A w/Gamma Hurdle ####
a.se <- sum(waic(abund.gamma.hurd) %>%
                pluck(8),
              waic(strat.gamma.hurd.fac) %>%
                pluck(8),
              waic(mtz.gamma.hurd.2) %>%
                pluck(8),
              waic(sst.a2) %>%
                pluck(8),
              waic(regzpl.a2) %>%
                pluck(8))



####Total WAIC Model B w/Gamma Hurdle####
b.waic <- sum(waic(abund.gamma.hurd.B2) %>%
                pluck("waic"),
              waic(CCBStrat.b2) %>%
                pluck("waic"),
              waic(RegZpl.b2) %>%
                pluck("waic"),
              waic(cal.gamma.hurd.B2) %>%
                pluck("waic"),
              waic(mtz.patchy.b) %>%
                pluck("waic"),
              waic(Pseudo.b) %>%
                pluck("waic"))


#### Total WAIC SE Model B w/Gamma Hurdle####
b.se <- sum(waic(abund.gamma.hurd.B2) %>%
                pluck(8),
              waic(CCBStrat.b2) %>%
                pluck(8),
              waic(RegZpl.b2) %>%
                pluck(8),
              waic(cal.gamma.hurd.B2) %>%
                pluck(8),
              waic(mtz.patchy.b) %>%
                pluck(8),
              waic(Pseudo.b) %>%
                pluck(8))

####Total WAIC Model C w/Gamma Hurdle####
c.waic <- sum(waic(eg.abund.C) %>%
                pluck("waic"), 
              waic(patchy2.C) %>% 
                pluck("waic"),
              waic(CCBStrat.C2) %>%
                pluck("waic"),
              waic(RegZpl.C2) %>%
                pluck("waic"),
              waic(chl.C2) %>%
                pluck("waic"), 
              waic(mtz.C2) %>%
                pluck("waic"))

####Total WAIC SE Model C w/Gamma Hurdle####
c.se <- sum(waic(eg.abund.C) %>%
              pluck(8), 
            waic(patchy2.C) %>% 
              pluck(8),
            waic(CCBStrat.C2) %>%
              pluck(8),
            waic(RegZpl.C2) %>%
              pluck(8),
            waic(chl.C2) %>%
              pluck(8), 
            waic(mtz.C2) %>%
              pluck("waic"))

####Total WAIC Model A w/ZINB w/Month####
zinb.a.waic <- sum(waic(zinba.month) %>%
                     pluck("waic"),
                   waic(strat.A.month) %>%
                     pluck("waic"),
                   waic(mtz.gamma.hurd.2) %>%
                     pluck("waic"),
                   waic(sst.a2.month) %>%
                     pluck("waic"),
                   waic(regzpl.a2.month) %>%
                     pluck("waic"))


#### TOTAL WAIC SE Model A w/ZINB w/month ####
zinb.a.se <- sum(waic(zinba.month) %>%
                   pluck(8),
                 waic(strat.A.month) %>%
                   pluck(8),
                 waic(mtz.gamma.hurd.2) %>%
                   pluck(8),
                 waic(sst.a2.month) %>%
                   pluck(8),
                 waic(regzpl.a2.month) %>%
                   pluck(8))

####Total WAIC Model B w/ZINB w/month####
zinb.b.waic <- sum(waic(zinbb.month) %>%
                pluck("waic"),
              waic(CCBStrat.b2.month) %>%
                pluck("waic"),
              waic(RegZpl.b2.month) %>%
                pluck("waic"),
              waic(cal.gamma.hurd.B2) %>%
                pluck("waic"),
              waic(mtz.patchy.b) %>%
                pluck("waic"),
              waic(Pseudo.b) %>%
                pluck("waic"))


#### Total WAIC SE Model B w/ZINB ####
zinb.b.se <- sum(waic(zinbb.month) %>%
              pluck(8),
            waic(CCBStrat.b2.month) %>%
              pluck(8),
            waic(RegZpl.b2.month) %>%
              pluck(8),
            waic(cal.gamma.hurd.B2) %>%
              pluck(8),
            waic(mtz.patchy.b) %>%
              pluck(8),
            waic(Pseudo.b) %>%
              pluck(8))



####Total WAIC Model C w/ZINB w/month ####
zinb.c.waic <- sum(waic(zinbc.month) %>%
                pluck("waic"), 
              waic(patchy.C.month) %>% 
                pluck("waic"),
              waic(CCBStrat.C2.month) %>%
                pluck("waic"),
              waic(RegZpl.C2.month) %>%
                pluck("waic"),
              waic(chl.C2.month) %>%
                pluck("waic"), 
              waic(mtz.C2) %>%
                pluck("waic"))

####Total WAIC SE Model C w/ZINB####
zinb.c.se <- sum(waic(zinbc.month) %>%
              pluck(8), 
            waic(patchy.C.month) %>% 
              pluck(8),
            waic(CCBStrat.C2.month) %>%
              pluck(8),
            waic(RegZpl.C2.month) %>%
              pluck(8),
            waic(chl.C2.month) %>%
              pluck(8), 
            waic(mtz.C2) %>%
              pluck("waic"))


#### Model Weights ####
abund.weight <- model_weights(abund.gamma.hurd, 
                              abund.gamma.hurd.B2,
                              eg.abund.C,
              weights = "waic") %>% 
              round(digits = 2) %>%
  as_tibble() %>% 
  rename(weight = value) %>% 
  mutate(model  = c("eg.abund.A", "eg.abund.B", "eg.abund.C"),
         weight = weight %>% round(digits = 2)) %>% 
  select(model, weight) %>% 
  arrange(desc(weight)) %>% 
  knitr::kable()

zinb.weight <- model_weights(zinba.month, 
                             zinbb.month, 
                             zinbc.month,
                             weights = "waic") %>%
                             round(digits = 2) %>%
  as_tibble() %>%
  rename(weight = value) %>% 
  mutate(model  = c("ZINBA", "ZINBB", "ZINBC"),
         weight = weight %>% round(digits = 2)) %>% 
  select(model, weight) %>% 
  arrange(desc(weight)) %>% 
  knitr::kable()

strat.weight <- model_weights(strat.A.month, 
                              CCBStrat.b2.month,
                              CCBStrat.C2.month,
                              weights = "waic") %>% 
                round(digits = 2) %>%
  as_tibble() %>% 
  rename(weight = value) %>% 
  mutate(model  = c("Strat.A", "Strat.B", "Strat.C"),
         weight = weight %>% 
           round(digits = 2)) %>% 
  select(model, weight) %>% 
  arrange(desc(weight)) %>% 
  knitr::kable()


mtz.weight <- model_weights(mtz.gamma.hurd.2,
                            mtz.C2,
                            weights = "waic") %>% 
  round(digits = 2) %>%
  as_tibble() %>% 
  rename(weight = value) %>% 
  mutate(model  = c("MTZ.A", "MTZ.C"),
         weight = weight %>% round(digits = 2)) %>% 
  select(model, weight) %>% 
  arrange(desc(weight)) %>% 
  knitr::kable()

patchy.weight <- model_weights(mtz.patchy.b, 
                               patchy.C.month,
                               weights = "waic") %>% 
  round(digits = 2) %>%
  as_tibble() %>% 
  rename(weight = value) %>% 
  mutate(model  = c("patchy.B", "patchy.C"),
         weight = weight %>% round(digits = 2)) %>% 
  dplyr::select(model, weight) %>% 
  arrange(desc(weight)) %>% 
  knitr::kable()

RegZpl.weight <- model_weights(regzpl.a2.month, 
                               RegZpl.b2.month,
                               RegZpl.C2.month,
                               weights = "waic") %>% 
  round(digits = 2) %>%
  as_tibble() %>% 
  rename(weight = value) %>% 
  dplyr::mutate(model  = c("RegZpl.A", "RegZpl.B", "RegZpl.C"),
         weight = weight %>% round(digits = 2)) %>% 
  dplyr::select(model, weight) %>% 
  arrange(desc(weight)) %>% 
  knitr::kable()

#### Model Coef. Comparison ####
my_coef_tab <-
  rbind(broom::tidy(abund.gamma.hurd),
        broom::tidy(abund.gamma.hurd.B2), 
        broom::tidy(eg.abund.C)) %>%
  mutate(model = c(rep("eg.abund.A", times = nrow(broom::tidy(abund.gamma.hurd))),
                   rep("eg.abund.B", times = nrow(broom::tidy(abund.gamma.hurd.B2))),
                   rep("eg.abund.C", times = nrow(broom::tidy(eg.abund.C))))) %>%
  filter(term != "lp__" & term != "shape") %>%
 dplyr::select(model, everything())

my_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  dplyr::select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# data wrangling
my_coef_tab1 <-
  my_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_CCBStrat", "b_mtz.patchiness", "b_pop.est",
                    "b_RegZpl", "b_SpringDate", "b_SST"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("eg.abund.A", "eg.abund.B", "eg.abund.C")),
         term  = factor(term, levels = c("b_CCBStrat", "b_mtz.patchiness", "b_pop.est",
                                         "b_RegZpl", "b_SpringDate", "b_SST"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:222],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")
  

# plot
ggplot(data = my_coef_tab,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = my_coef_tab$axis) +
  ggtitle("Coefficients for Gamma part of abundance models (1 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))

# 2nd part of gamma abund models
my_coef_tab2 <-
  my_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_MTZ", "b_MTZ:CCBStrat"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("eg.abund.A", "eg.abund.B", "eg.abund.C")),
         term  = factor(term, levels = c("b_MTZ", "b_MTZ:CCBStrat"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:218],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")


# plot
ggplot(data = my_coef_tab2,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = my_coef_tab2$axis) +
  ggtitle("Coefficients for Gamma part of abundance models (2 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))


## Hurdle part of abundance models
hurd_tab <-
  rbind(broom::tidy(abund.gamma.hurd),
        broom::tidy(abund.gamma.hurd.B2), 
        broom::tidy(eg.abund.C)) %>%
  mutate(model = c(rep("eg.abund.A", times = nrow(broom::tidy(abund.gamma.hurd))),
                   rep("eg.abund.B", times = nrow(broom::tidy(abund.gamma.hurd.B2))),
                   rep("eg.abund.C", times = nrow(broom::tidy(eg.abund.C))))) %>%
  filter(term != "lp__" & term != "shape") %>%
  dplyr::select(model, everything())

hurd_tab %>%
  complete(term = distinct(., term), model) %>%
  dplyr::select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# data wrangling
hurd_tab1 <-
  hurd_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_hu_CCBStrat", "b_hu_mtz.patchiness", "b_hu_pop.est",
                    "b_hu_RegZpl", "b_hu_SpringDate", "b_hu_SST"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("eg.abund.A", "eg.abund.B", "eg.abund.C")),
         term  = factor(term, levels = c("b_hu_CCBStrat", "b_hu_mtz.patchiness", "b_hu_pop.est",
                                         "b_hu_RegZpl", "b_hu_SpringDate", "b_hu_SST"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:222],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")


# plot
ggplot(data = hurd_tab1,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = hurd_tab1$axis) +
  ggtitle("Coefficients for Hurdle part of abundance models") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))




# 2nd part of gamma abund models
hurd_tab2 <-
  hurd_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_hu_MTZ", "b_hu_MTZ:CCBStrat"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("eg.abund.A", "eg.abund.B", "eg.abund.C")),
         term  = factor(term, levels = c("b_hu_MTZ", "b_hu_MTZ:CCBStrat"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:218],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")


# plot
ggplot(data = hurd_tab2,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = hurd_tab2$axis) +
  ggtitle("Coefficients for Hurdle part of abundance models (2 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))


#### Compare ZIN Models ####
zin_coef_tab <-
  rbind(broom::tidy(zinba.month),
        broom::tidy(zinbb.month), 
        broom::tidy(zinbc.month)) %>%
  mutate(model = c(rep("ZIN.A", times = nrow(broom::tidy(zinba.month))),
                   rep("ZIN.B", times = nrow(broom::tidy(zinbb.month))),
                   rep("ZIN.C", times = nrow(broom::tidy(zinbc.month))))) %>%
  filter(term != "lp__") %>%
  dplyr::select(model, everything())

zin_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  dplyr::select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# data wrangling
zin_coef_tab1 <-
  zin_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_pop.est", "b_SST", "b_MTZ",
                    "b_CCBStrat", "b_RegZpl", "b_MTZ:CCBStrat"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("ZIN.A", "ZIN.B", "ZIN.C")),
         term  = factor(term, levels = c("b_pop.est", "b_SST", "b_MTZ",
                    "b_CCBStrat", "b_RegZpl", "b_MTZ:CCBStrat"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:126],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

zin_coef_tab2 <-
  zin_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_mtz.patchiness", "b_SpringDate", "b_Intercept",
                    "b_Month2", "b_Month3", "b_Month4", "b_Month5"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("ZIN.A", "ZIN.B", "ZIN.C")),
         term  = factor(term, levels = c("b_mtz.patchiness", "b_SpringDate", "b_Intercept",
                                         "b_Month2", "b_Month3", "b_Month4", "b_Month5"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:127],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = zin_coef_tab1,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = zin_coef_tab1$axis) +
  ggtitle("Coefficients for Zero Inflated models (1 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))

# plot
ggplot(data = zin_coef_tab2,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = zin_coef_tab2$axis) +
  ggtitle("Coefficients for Zero Inflated models (2 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))




#### Compare Strat w/month Models ####

strat_coef_tab <-
  rbind(broom::tidy(strat.A.month),
        broom::tidy(CCBStrat.b2.month), 
        broom::tidy(CCBStrat.C2.month)) %>%
  mutate(model = c(rep("CCBStrat.A", times = nrow(broom::tidy(strat.A.month))),
                  rep("CCBStrat.B", times = nrow(broom::tidy(CCBStrat.b2.month))),
                   rep("CCBStrat.C", times = nrow(broom::tidy(CCBStrat.C2.month))))) %>%
  filter(term != "lp__") %>%
  dplyr::select(model, everything())

strat_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  dplyr::select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# data wrangling
strat_coef_tab1 <-
  strat_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_Intercept", "b_Month2", "b_Month3",
                      "b_Month4", "b_Month5", "b_SST"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("CCBStrat.A", "CCBStrat.B", "CCBStrat.C")),
         term  = factor(term, levels = c("b_Intercept", "b_Month2", "b_Month3",
                                         "b_Month4", "b_Month5"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:141],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "<NA>")

# plot
ggplot(data = strat_coef_tab1,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = strat_coef_tab1$axis) +
  ggtitle("Coefficients for Stratification models (1 of 4)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))


## part 2 of strat models

strat_coef_tab2 <-
  strat_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_CompRegWindNW:RegWindSpd", 
                    "b_CompRegWindS:RegWindSpd", 
                    "b_CompRegWindW:RegWindSpd","b_AO1yrLag",
                    "b_LocalWindSpd","b_CompLocalWindNW"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("CCBStrat.A", "CCBStrat.B", "CCBStrat.C")),
         term  = factor(term, levels = c("b_CompRegWindNW:RegWindSpd", 
                                         "b_CompRegWindS:RegWindSpd", 
                                         "b_CompRegWindW:RegWindSpd",
                                         "b_AO1yrLag", "b_LocalWindSpd",
                                         "b_CompLocalWindNW"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:141],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = strat_coef_tab2,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = strat_coef_tab2$axis) +
  ggtitle("Coefficients for Stratification models (2 of 4)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))


## part 3 of strat models

strat_coef_tab3 <-
  strat_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_CompLocalWindS", "b_RegWindSpd",
                    "b_CompLocalWindW", "b_LocalWindSpd:CompLocalWindNW",
                    "b_LocalWindSpd:CompLocalWindS",
                    "b_LocalWindSpd:CompLocalWindW"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("CCBStrat.A", "CCBStrat.B", "CCBStrat.C")),
         term  = factor(term, levels = c("b_CompLocalWindS", "b_RegWindSpd",
                                         "b_CompLocalWindW", "b_LocalWindSpd:CompLocalWindNW",
                                         "b_LocalWindSpd:CompLocalWindS",
                                         "b_LocalWindSpd:CompLocalWindW"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:141],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = strat_coef_tab3,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = strat_coef_tab3$axis) +
  ggtitle("Coefficients for Stratification models (3 of 4)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))

## part 4 of strat models

strat_coef_tab4 <-
  strat_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_SST", "b_SpringDate",
                    "b_gulf.stream.north.wall"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("CCBStrat.A", "CCBStrat.B", "CCBStrat.C")),
         term  = factor(term, levels = c("b_SST", "b_SpringDate",
                                         "b_gulf.stream.north.wall"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:138],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = strat_coef_tab4,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = strat_coef_tab4$axis) +
  ggtitle("Coefficients for Stratification models (4 of 4)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))

#### Compare RegZpl Models w/month ####

regzpl_coef_tab <-
  rbind(broom::tidy(regzpl.a2.month),
        broom::tidy(RegZpl.b2.month), 
        broom::tidy(RegZpl.C2.month)) %>%
  mutate(model = c(rep("RegZpl.A", times = nrow(broom::tidy(regzpl.a2.month))),
                   rep("RegZpl.B", times = nrow(broom::tidy(RegZpl.b2.month))),
                   rep("RegZpl.C", times = nrow(broom::tidy(RegZpl.C2.month))))) %>%
  filter(term != "lp__") %>%
  dplyr::select(model, everything())

regzpl_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  dplyr::select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# data wrangling
regzpl_coef_tab1 <-
  regzpl_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_NAO4yrlag", "b_SST", 
                    "b_CCBStrat", "b_chlorophyll", "b_NAO2yrlag"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("RegZpl.A", "RegZpl.B", "RegZpl.C")),
         term  = factor(term, levels = c( "b_NAO4yrlag", "b_SST", 
                                         "b_CCBStrat", "b_chlorophyll",
                                         "b_NAO2yrlag"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:101],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = regzpl_coef_tab1,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = regzpl_coef_tab1$axis) +
  ggtitle("Coefs for Regional Zooplankton models (1 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))

# data wrangling
regzpl_coef_tab2 <-
  regzpl_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_Intercept", "b_Month2", 
                    "b_Month3", "b_Month4", "b_Month5"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("RegZpl.A", "RegZpl.B", "RegZpl.C")),
         term  = factor(term, levels = c("b_Intercept", "b_Month2", 
                                          "b_Month3", "b_Month4", "b_Month5"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:101],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")
  

# plot
ggplot(data = regzpl_coef_tab2,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = regzpl_coef_tab2$axis) +
  ggtitle("Coefs for Regional Zooplankton models (2 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))

  
#### Compare MTZ Models ####
mtz_coef_tab <-
  rbind(broom::tidy(mtz.gamma.hurd.2),
        broom::tidy(mtz.C2)) %>%
  mutate(model = c(rep("MTZ.A", times = nrow(broom::tidy(mtz.gamma.hurd.2))),
                   rep("MTZ.C", times = nrow(broom::tidy(mtz.C2))))) %>%
  filter(term != "lp__") %>%
  dplyr::select(model, everything())

mtz_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  dplyr::select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# data wrangling
mtz_coef_tab1 <-
  mtz_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_RegZpl", "b_SST", 
                    "b_CCBStrat", "b_CompRegWindNW", "b_CompRegWindS",
                    "b_CompRegWindW", "b_RegWindSpd"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("MTZ.A", "MTZ.C")),
         term  = factor(term, levels = c("b_RegZpl", "b_SST", 
                                         "b_CCBStrat", "b_CompRegWindNW", "b_CompRegWindS",
                                         "b_CompRegWindW", "b_RegWindSpd"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:75],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = mtz_coef_tab1,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = mtz_coef_tab1$axis) +
  ggtitle("Coefs for MTZ models (1 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))


## 2nd half of mtz
# data wrangling
mtz_coef_tab2 <-
  mtz_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_CompRegWindNW:RegWindSpd", "b_CompRegWindS:RegWindSpd",
                    "b_CompRegWindW:RegWindSpd", "b_SpringDate"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("MTZ.A", "MTZ.C")),
         term  = factor(term, levels = c("b_CompRegWindNW:RegWindSpd", "b_CompRegWindS:RegWindSpd",
                                         "b_CompRegWindW:RegWindSpd", "b_SpringDate"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:72],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = mtz_coef_tab2,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = mtz_coef_tab2$axis) +
  ggtitle("Coefs for MTZ models (2 of 2)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))




#### Compare Zpl Patchiness Models ####
patchy_coef_tab <-
  rbind(broom::tidy(mtz.patchy.b),
        broom::tidy(patchy.C.month)) %>%
  mutate(model = c(rep("Patchy.B", times = nrow(broom::tidy(mtz.patchy.b))),
                   rep("Patchy.C", times = nrow(broom::tidy(patchy.C.month))))) %>%
  filter(term != "lp__") %>%
  dplyr::select(model, everything())

patchy_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  dplyr::select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# data wrangling
patchy_coef_tab1 <-
  patchy_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_SST", "b_Pseudo",
                    "b_CalCCB","b_CCBStrat", "b_Centro",
                    "b_LocalWindSpd"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("Patchy.B", "Patchy.C")),
         term  = factor(term, levels = c("b_SST", "b_Pseudo",
                                         "b_CalCCB","b_CCBStrat", "b_Centro",
                                         "b_LocalWindSpd"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:86],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = patchy_coef_tab1,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = patchy_coef_tab1$axis) +
  ggtitle("Cooefficients for Zooplankton Patchiness models (1 of 3)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))


## part 2 of patchiness
patchy_coef_tab2 <-
  patchy_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_CompLocalWindNW",
                    "b_CompLocalWindS", "b_CompLocalWindW",
                    "b_LocalWindSpd:CompLocalWindNW",
                    "b_LocalWindSpd:CompLocalWindS",
                    "b_LocalWindSpd:CompLocalWindW"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("Patchy.B", "Patchy.C")),
         term  = factor(term, levels = c("b_CompLocalWindNW",
                                         "b_CompLocalWindS", "b_CompLocalWindW",
                                         "b_LocalWindSpd:CompLocalWindNW",
                                         "b_LocalWindSpd:CompLocalWindS",
                                         "b_LocalWindSpd:CompLocalWindW",
                                         "b_MTZ"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:86],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = patchy_coef_tab2,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = patchy_coef_tab2$axis) +
  ggtitle("Coefs for Patchiness models (2 of 3)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))


## part 3 of patchiness
patchy_coef_tab3 <-
  patchy_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  rbind(
    tibble(
      model     = NA,
      term      = c("b_MTZ",
                    "b_Month2",
                    "b_Month3",
                    "b_Month4",
                    "b_Month5",
                    "b_Intercept"),
      estimate  = NA,
      std.error = NA,
      lower     = NA,
      upper     = NA)) %>%
  mutate(axis  = ifelse(is.na(model), term, model),
         model = factor(model, levels = c("Patchy.B", "Patchy.C")),
         term  = factor(term, levels = c("b_MTZ",
                                         "b_Month2",
                                         "b_Month3",
                                         "b_Month4",
                                         "b_Month5",
                                         "b_Intercept"))) %>%
  arrange(term, model) %>%
  mutate(axis_order = letters[1:86],
         axis = ifelse(str_detect(axis, "b6."), str_c("      ", axis), axis)) %>%
  filter(term != "NA")

# plot
ggplot(data = patchy_coef_tab3,
       aes(x = axis_order,
           y = estimate,
           ymin = lower,
           ymax = upper)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_pointrange(color = "navyblue", alpha = 1/2, 
                  fill = "blue", size = .35) +
  scale_x_discrete(NULL, labels = patchy_coef_tab3$axis) +
  ggtitle("Coefs for Patchiness models (3 of 3)") +
  coord_flip() +
  theme(text         = element_text(family = "Calibri"),
        panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))


#### Model Averaging ####
#### Model Average RegZpl w/Month ####
## make some new data for the RegZpl models

nd.cross <- crossing(NAO4yrlag = rep(0.2875),
                     Year = rep(c("1998", "1999", "2000", "2001", "2002",
                                  "2003", "2004", "2005", "2006", "2007",
                                  "2008", "2009", "2010", "2011", "2012", 
                                  "2013", "2014", "2015", "2016", "2017")),
                     Month = rep(c("1", "2", "3", "4", "5")),
                     SST = rep(5.58),
                     CCBStrat = seq(from = 0, to = 3.22),
                     chlorophyll = rep(0.75),
                     NAO2yrlag = rep(0.27),
                     RegZpl = rep(42758))


# we'll get the `RegZpl.C2`-implied trajectory with `fitted()`
fitd_RegZpl.C2.month <-
  fitted(RegZpl.C2.month, newdata = nd.cross) %>%
  as_tibble() %>%
  bind_cols(nd.cross)


# the model-average trajectory comes from `pp_average()`
pp_average(regzpl.a2.month, 
           RegZpl.b2.month, 
           RegZpl.C2.month, 
           weights = "waic",
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = nd.cross) %>%
  as_tibble() %>%
  bind_cols(nd.cross) %>%
  ggplot(aes(x = CCBStrat, y = Estimate, color = Year)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "ivory", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_RegZpl.C2.month, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_RegZpl.C2.month,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, aes(x = CCBStrat, y = RegZpl), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "Regional Calanus finmarchicus", x = "Cape Cod Bay Stratification") +
  coord_cartesian(xlim = range(all.data$CCBStrat), 
                  ylim = range(all.data$RegZpl)) +
  facet_wrap(~Month) +
  theme_classic() +
  ggtitle("Model averaged posterior predictive distribution") +
  theme(text = element_text(family = "Calibri")) + 
  theme(legend.position="bottom")




#### Model Average Stratification w/Month ####
## make some new data for the stratification models

cross.strat <- crossing(RegWindSpd = rep(7.67),
                        Year = rep(c("1998")), 
                        SST = seq(from = 2, to = 11),
                        CompRegWind = rep(c("W", "S", "NE", "NW")),
                        CompLocalWind = rep(c("NE", "NW", "S", "W")),
                        Month = rep(c("1", "2", "3", "4", "5")), 
                        LocalWindSpd = rep(6.84),
                        AO1yrLag = rep(-0.12),
                        gulf.stream.north.wall = rep(0.25),
                        SpringDate = rep(147.4),
                        CCBStrat = rep(0.65))

fitd_Strat.b <-
  fitted(CCBStrat.b2.month, newdata = cross.strat) %>%
  as_tibble() %>%
  bind_cols(cross.strat)

pp_average(strat.A.month, 
           CCBStrat.b2.month, 
           weights = "waic",
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = cross.strat) %>%
  as_tibble() %>%
  bind_cols(cross.strat) %>%
  ggplot(aes(x = SST, y = Estimate, color = CompRegWind)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "ivory", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_Strat.b, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_Strat.b,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, aes(x = SST, y = CCBStrat), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "Stratification", x = "SST") +
  coord_cartesian(xlim = range(all.data$SST), 
                  ylim = range(all.data$CCBStrat)) +
  theme_classic() +
  facet_grid(CompLocalWind ~ Month) +
  ggtitle("Model averaged posterior predictive distribution") +
  theme(text = element_text(family = "Calibri"))




#### Model Avg Patchiness ####
patch.cross <- 
  crossing(LocalWindSpd = rep(6.8),
         Year = rep(c("1998")), 
         SST = rep(5.5),
         Pseudo = rep(4093),
         CalCCB = rep(3732),
         Centro = rep(2340),
         Month = rep(c("1", "2", "3", "4", "5")), 
         CompLocalWind = rep(c("S")),
         CCBStrat = seq(from = 0, to = 3.5),
         MTZ = rep(12100),
         mtz.patchiness = rep(4.7)) ## this is the mean of the Strat data


fitd_patch.C <-
  fitted(patchy2.C, newdata = patch.cross) %>%
  as_tibble() %>%
  bind_cols(patch.cross)

pp_average(mtz.patchy.b, patchy2.C,
           weights = "waic",
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = patch.cross) %>%
  as_tibble() %>%
  bind_cols(patch.cross) %>%
  ggplot(aes(x = CCBStrat, y = Estimate, color = CompLocalWind)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "ivory", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_patch.C, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_patch.C,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, aes(x = CCBStrat, y = mtz.patchiness), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "Patchiness", x = "Stratification") +
  coord_cartesian(xlim = range(all.data$CCBStrat), 
                  ylim = range(all.data$mtz.patchiness)) +
  theme_classic() +
  facet_grid(~ Month) +
  ggtitle("Model averaged posterior predictive distribution") +
  theme(text = element_text(family = "Calibri"))


#### Model Avg MTZ ####
## make some new data for the mtz models
mtz.cross <- 
  crossing(RegWindSpd = rep(7.6),
         Year = rep(c("1998", "1999", "2000", "2001", "2002",
                      "2003", "2004", "2005", "2006", "2007",
                      "2008", "2009", "2010", "2011", "2012", 
                      "2013", "2014", "2015", "2016", "2017")), 
         SST = rep(5.5),
         CompRegWind = rep(c("S", "W", "NE", "NW")),
         CCBStrat = rep(0.65),
         RegZpl = seq(from = 1000, to = 303000),
         SpringDate = rep(147),
         MTZ = rep(12100.2)) ## this is the mean of the MTZ data


fitd_mtz.A <-
  fitted(mtz.gamma.hurd.2, newdata = mtz.cross) %>%
  as_tibble() %>%
  bind_cols(mtz.cross)

pp_average(mtz.gamma.hurd.2, mtz.C2,
           weights = "waic",
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = mtz.cross) %>%
  as_tibble() %>%
  bind_cols(mtz.cross) %>%
  ggplot(aes(x = RegZpl, y = Estimate, color = Year)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "ivory", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_mtz.A, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_mtz.A,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, aes(x = RegZpl, y = MTZ), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "MTZ", x = "RegZpl") +
  coord_cartesian(xlim = range(all.data$RegZpl), 
                  ylim = range(all.data$MTZ)) +
  facet_grid(CompRegWind ~ Month) +
  theme_classic() +
  ggtitle("Model averaged posterior predictive distribution") +
  theme(text = element_text(family = "Calibri"))


#### Model Avg ZIN ####
## make some new data for the effect of SST on 
## abundance models.  If I use the first tibble
## with all the possible years the plots look very 
## jagged due to the random effect of year.
zin.cross <- 
  tibble(MTZ = rep(12100, 12000),
         Year = rep(c("1998", "1999", "2000", "2001", "2002",
                      "2003", "2004", "2005", "2006", "2007",
                      "2008", "2009", "2010", "2011", "2012", 
                      "2013", "2014", "2015", "2016", "2017"), 600),
         Month = rep(c("1", "2", "3", "4", "5"), 2400),
         SST = seq(from = 2, to = 11, length.out = 12000),
         SpringDate = rep(147, 12000),
         RegZpl = rep(42758, 12000),
         pop.est = rep(407.6, 12000),
         CCBStrat = rep(0.65640, 12000),
         mtz.patchiness = rep(3.98, 12000),
         eg.abund = rep(43, 12000)) ## this is the mean of the Strat data


# we'll get the `zinbC`-implied trajectory with `fitted()`
## ZinbC is the best model determined by model weights
fitd_zinb.C <-
  fitted(zinbc.month, newdata = zin.cross) %>%
  as_tibble() %>%
  bind_cols(zin.cross)

# the model-average trajectory comes from `pp_average()`
ppa <- pp_average(zinba.month, 
           zinbb.month, 
           zinbc.month,
           weights = "waic",
           summary = FALSE,
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = zin.cross) %>%
  as_tibble() %>%
  bind_cols(zin.cross) 


  ggplot(data = fitd_zinb.C, 
         aes(x = SST, y = Estimate, color = Year)) +
  geom_ribbon(data = fitd_zinb.C, aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "ivory", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_zinb.C, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_zinb.C,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, 
             aes(x = SST, y = whole.abund), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "Abundance", x = "SST") +
  coord_cartesian(xlim = range(all.data$SST), 
                  ylim = range(all.data$whole.abund)) +
  theme_classic() +
  facet_wrap(~Month) +
  ggtitle("Model averaged posterior predictive distribution \nfrom zin abundance models") +
  theme(text = element_text(family = "Calibri"))


## The effect of RegZpl on ZINB abundance
zin.zpl.cross <- 
  crossing(MTZ = rep(12100),
         Year = rep(c("1998", "1999", "2000", "2001", "2002",
                      "2003", "2004", "2005", "2006", "2007",
                      "2008", "2009", "2010", "2011", "2012", 
                      "2013", "2014", "2015", "2016", "2017")), 
         Month = rep(c("1", "2", "3", "4", "5")),
         SST = rep(5.58),
         SpringDate = rep(147),
         RegZpl = seq(from = 1004.6, to = 302579),
         pop.est = rep(407.6),
         CCBStrat = rep(0.65640),
         mtz.patchiness = rep(3.98),
         eg.abund = rep(43)) ## this is the mean of the Strat data

fitd_zinb.C <-
  fitted(zinbc.month, newdata = zin.zpl.cross) %>%
  as_tibble() %>%
  bind_cols(zin.zpl.cross)

 pp_average(zinba.month, zinbb.month, zinbc.month,
           weights = "waic",
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = zin.zpl.cross) %>%
  as_tibble() %>%
  bind_cols(zin.zpl.cross) %>%
  ggplot(aes(x = RegZpl, y = Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "gray50", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_zinb.B, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_zinb.B,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, aes(x = RegZpl, y = whole.abund), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "Abundance", x = "RegZpl") +
  coord_cartesian(xlim = range(all.data$RegZpl), 
                  ylim = range(all.data$whole.abund)) +
  theme_classic() +
  ggtitle("Model averaged posterior predictive distribution \n from ZIN abundance models") +
  theme(text = element_text(family = "Calibri"))

## the effect of pop.est on ZINB abundance
zin.pop.est <- 
  crossing(MTZ = rep(12100, times = 100),
         Year = rep(c("1998", "1999", "2000", "2001", "2002",
                      "2003", "2004", "2005", "2006", "2007",
                      "2008", "2009", "2010", "2011", "2012", 
                      "2013", "2014", "2015", "2016", "2017"), 5), 
         SST = rep(5.58, length.out = 100),
         SpringDate = rep(147, times= 100),
         RegZpl = rep(42758, length.out = 100),
         pop.est = seq(from = 310, to = 481, length.out = 100),
         CCBStrat = rep(0.65640, length.out = 100),
         mtz.patchiness = rep(3.98, length.out = 100),
         eg.abund = rep(43, times = 100)) ## this is the mean of the Strat data

fitd_zinb.C <-
  fitted(zinbc.month, newdata = zin.pop.est) %>%
  as_tibble() %>%
  bind_cols(zin.pop.est)

pp_average(zinba, zinbb, zinbc,
           weights = "waic",
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = zin.pop.est) %>%
  as_tibble() %>%
  bind_cols(zin.pop.est) %>%
  ggplot(aes(x = pop.est, y = Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "gray50", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_zinb.C, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_zinb.C,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, aes(x = pop.est, y = whole.abund), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "Abundance", x = "Population estimate") +
  coord_cartesian(xlim = range(all.data$pop.est), 
                  ylim = range(all.data$whole.abund)) +
  theme_classic() +
  ggtitle("Model averaged posterior predictive distribution \n from ZIN abundance models") +
  theme(text = element_text(family = "Calibri"))


## The effect of patchiness on ZINB abundance
zin.patchy <- 
  crossing(MTZ = rep(12100, times = 100),
         Year = rep(c("1998", "1999", "2000", "2001", "2002",
                      "2003", "2004", "2005", "2006", "2007",
                      "2008", "2009", "2010", "2011", "2012", 
                      "2013", "2014", "2015", "2016", "2017"), 5), 
         SST = rep(5.58, length.out = 100),
         SpringDate = rep(147, times= 100),
         RegZpl = rep(42758, length.out = 100),
         pop.est = rep(407.6, length.out = 100),
         CCBStrat = rep(0.65640, length.out = 100),
         mtz.patchiness = seq(from = 0.08, to = 18, length.out = 100),
         eg.abund = rep(43, times = 100)) ## this is the mean of the Strat data

fitd_zinb.C.patchy <-
  fitted(zinbc.month, newdata = zin.patchy) %>%
  as_tibble() %>%
  bind_cols(zin.patchy)

pp_average(zinba, zinbb, zinbc,
           weights = "waic",
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = zin.patchy) %>%
  as_tibble() %>%
  bind_cols(zin.patchy) %>%
  ggplot(aes(x = mtz.patchiness, y = Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "gray50", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_zinb.C.patchy, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_zinb.C.patchy,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, aes(x = mtz.patchiness, y = whole.abund), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "Abundance", x = "Patchiness") +
  coord_cartesian(xlim = range(all.data$mtz.patchiness), 
                  ylim = range(all.data$whole.abund)) +
  theme_classic() +
  ggtitle("Model averaged posterior predictive distribution \n from ZIN abundance models") +
  theme(text = element_text(family = "Calibri"))

## The effect of stratification on ZINB abundance
nd.zin.strat <- 
  crossing(MTZ = rep(12100, times = 100),
         Year = rep(c("1998", "1999", "2000", "2001", "2002",
                      "2003", "2004", "2005", "2006", "2007",
                      "2008", "2009", "2010", "2011", "2012", 
                      "2013", "2014", "2015", "2016", "2017"), 5), 
         SST = rep(5.58, length.out = 100),
         SpringDate = rep(147, times= 100),
         RegZpl = rep(42758, length.out = 100),
         pop.est = rep(407.6, length.out = 100),
         CCBStrat = seq(from = 0.04, to = 3.22, length.out = 100),
         mtz.patchiness = rep(3.98, length.out = 100),
         eg.abund = rep(43, times = 100)) ## this is the mean of the Strat data

fitd_zinb.C.strat <-
  fitted(zinbc.month, newdata = nd.zin.strat) %>%
  as_tibble() %>%
  bind_cols(nd.zin.strat)

pp_average(zinba, zinbb, zinbc,
           weights = "waic",
           method  = "fitted",  # for new data predictions, use `method = "predict"`
           newdata = nd.zin.strat) %>%
  as_tibble() %>%
  bind_cols(nd.zin.strat) %>%
  ggplot(aes(x = CCBStrat, y = Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), 
              fill = "gray50", alpha = 1/4) +
  geom_line(color = "gray50") +
  geom_ribbon(data  = fitd_zinb.C.strat, aes(ymin = Q2.5, ymax = Q97.5),
              color = "royalblue", fill = "transparent", linetype = 2) +
  geom_line(data = fitd_zinb.C.strat,
            color = "royalblue", linetype = 2) +
  geom_point(data = all.data, aes(x = CCBStrat, y = whole.abund), 
             size = 2, alpha = 2/3, color = "navyblue") +
  labs(y = "Abundance", x = "Stratification") +
  coord_cartesian(xlim = range(all.data$CCBStrat), 
                  ylim = range(all.data$whole.abund)) +
  theme_classic() +
  ggtitle("Model averaged posterior predictive distribution \n from ZIN abundance models") +
  theme(text = element_text(family = "Calibri"))



