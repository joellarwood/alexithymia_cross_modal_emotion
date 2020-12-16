
##################################################################
##                 Define Models for Hypotheses                 ##
##################################################################


# Load packages -----------------------------------------------------------

library(afex)
library(lmerTest)
library(tidyverse)
library(emmeans)

# Load data ---------------------------------------------------------------


trials <- here::here(
  "results",
  "data",
  "processed",
  "trials.rds"
) %>% 
  read_rds()

surveys <- here::here(
  "results",
  "data",
  "processed",
  "survey_scores.rds"
) %>% 
  read_rds() %>% 
  select(contains("z"),
         pid)

surveys_imputed <- here::here(
  "results",
  "data",
  "processed",
  "survey_scores_imputed.rds"
) %>% 
  read_rds() %>% 
  select(contains("z"),
         pid)



data <- trials %>% 
  left_join(
    select(surveys, pid, contains("z")),
    by = "pid"
  ) %>% 
  left_join(
    select(surveys_imputed, pid, contains("z")),
    by = "pid"
  ) 
  
write_rds(
  data,
  here::here(
    "results",
    "data",
    "modelling_data",
    "modelling_data_acc.rds"
  )
)


glimpse(data)



# Fit the models using the imputed data -----------------------------------


# Reaction time -----------------------------------------------------------


# Stimulus only -----------------------------------------------------------

rt_imputed_model_block <- lmerTest::lmer(
  rt ~ block  + (1 + block | pid),
  data = data,
  REML = FALSE
)

# save model 

rt_imputed_model_block %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "rt",
      "imputed",
      "rt_imputed_block_model.rds"
    )
  )

summary(rt_imputed_model_block)
anova(rt_imputed_model_block, type = 3)



# Stimulus and TAS --------------------------------------------------------


rt_imputed_model_block_tas <- lmerTest::lmer(
  rt ~ block * tas_imputed_z + (1 + block | pid),
  data = data,
  REML = FALSE
)

rt_imputed_model_block_tas %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "rt",
      "imputed",
      "rt_imputed_block_tas_model.rds"
    ),
  )
anova(rt_imputed_model_block_tas, type = 3)


# Stimulus * TAS + Stimulus * Gold MSI ------------------------------------


rt_imputed_model_block_tas_msi <- lmerTest::lmer(
  rt ~ block * tas_imputed_z + block * em_imputed_z + (1 + block | pid),
  data = data,
  REML = FALSE
)

rt_imputed_model_block_tas_msi %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "rt",
      "imputed",
      "rt_imputed_block_tas_msi_model.rds"
    )
  )

anova(rt_imputed_model_block_tas_msi, type = 3)



# Compare Models ----------------------------------------------------------

AIC(rt_imputed_model_block)
AIC(rt_imputed_model_block_tas)
AIC(rt_imputed_model_block_tas)

# D prime -----------------------------------------------------------------

data_agg <- trials %>% 
  group_by(
    pid, block
  ) %>% 
  summarise(
    mean = mean (rt),
    sd = sd(rt),
    se = plotrix::std.error(rt),
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
    d_prime
  ) %>% 
  right_join(
    select(surveys_imputed, pid, tas_imputed_z, em_imputed_z)
  ) 

data_agg %>% 
  write_rds(
    here::here(
      "results",
      "data",
      "modelling_data",
      "modelling_data_d.rds"
    )
  )

d_imputed_model_block <- lmerTest::lmer(
  d_prime ~ block + (1| pid ),
  data = data_agg,
  REML = FALSE
)

d_imputed_model_block %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "d_prime",
      "imputed",
      "d_imputed_block_model.rds"
    )
  )

d_imputed_model_block_tas <- lmerTest::lmer(
  d_prime ~ block * tas_imputed_z + (1| pid ),
  data = data_agg,
  REML = FALSE
)

d_imputed_model_block_tas %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "d_prime",
      "imputed",
      "d_imputed_block_tas_model.rds"
    )
  )

d_imputed_model_block_tas_msi <- lmerTest::lmer(
  d_prime ~ block * tas_imputed_z + block * em_imputed_z + (1| pid ),
  data = data_agg,
  REML = FALSE
)

d_imputed_model_block_tas_msi %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "d_prime",
      "imputed",
      "d_imputed_block_tas_msi_model.rds"
    )
  )

anova(d_imputed_model_block, d_imputed_model_block_tas, d_imputed_model_block_tas_msi)

