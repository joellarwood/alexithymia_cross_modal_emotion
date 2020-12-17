
#################################################################
##                        Model Accuracy                       ##
##                           D prime                           ##
##          Collapsing across trails within condition          ##
#################################################################


# Load packages -----------------------------------------------------------

library(tidyverse)
library(psycho)
# Load data ---------------------------------------------------------------

survey_scores <- here::here(
  "results",
  "data",
  "processed",
  "survey_scores_imputed.rds"
) %>% 
  read_rds() %>% 
  select(contains("z"),
         pid)

trials <- here::here(
  "results",
  "data",
  "processed",
  "trials.rds"
) %>% 
  read_rds()


data_agg <- trials %>% 
  group_by(
    pid, block
  ) %>% 
  summarise(
    rt_mean = mean (rt, na.rm = TRUE),
    rt_sd = sd(rt, na.rm = TRUE),
    rt_se = plotrix::std.error(rt),
    hit = sum(hit),
    cor_rej = sum(cor_rej),
    miss = sum(miss),
    false_alarm = sum(false_alarm),
    d_prime = psycho::dprime(
      n_hit = hit,
      n_fa = false_alarm,
      n_miss = miss,
      n_cr = cor_rej
    )[["dprime"]]
  ) %>% 
  select(
    pid,
    block,
    d_prime,
    contains("rt")
  ) %>% 
  right_join(
    select(surveys_scores, pid, tas_imputed_z, em_imputed_z, pa_imputed_z)
  ) 



# Fit Models --------------------------------------------------------------


# block only --------------------------------------------------------------

block_d <- lmer(
  d_prime ~ block + (1 | pid),
  data = data_agg
)

anova(block_d, 3)


# Main effect of block and TAS --------------------------------------------

block_tas_d <- lmer(
  d_prime ~ block + tas_imputed_z + (1 | pid),
  data = data_agg
)

anova(block_tas_d, 3)


# Interaction between block and TAS ---------------------------------------

block_by_tas_d <- lmer(
  d_prime ~ block * tas_imputed_z + (1 | pid),
  data = data_agg
)

anova(block_by_tas_d, 3)


# Interaction between block and TAS and between block and perceptu --------

block_by_tas_pa_d <- lmer(
  d_prime ~ block * tas_imputed_z + block * pa_imputed_z + (1 | pid),
  data = data_agg
)

anova(block_by_tas_pa_d, 3)

# Interaction between block and TAS and between block and emotion --------

block_by_tas_em_d <- lmer(
  d_prime ~ block * tas_imputed_z + block * em_imputed_z + (1 | pid),
  data = data_agg
)

anova(block_by_tas_em_d, 3)


# Max fixed effects -------------------------------------------------------

max_d <- lmer(
  d_prime ~ block * tas_imputed_z + block * pa_imputed_z + block * em_imputed_z + (1 | pid),
  data = data_agg
)

anova(max_d, 3)


# Compared models ---------------------------------------------------------



