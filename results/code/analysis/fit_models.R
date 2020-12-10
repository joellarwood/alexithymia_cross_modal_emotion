
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


glimpse(data)



# Fit the models using the imputed data -----------------------------------



# Reaction time -----------------------------------------------------------

rt_raw_model_block <- lmerTest::lmer(
  rt ~ block  + (1 + block | pid) + (1 | cue_target),
  data = drop_na(data, tas_z, em_z),
  REML = FALSE
)

# save model 

rt_raw_model_block %>% 
  write_rds(
    here::here(
    "results",
    "models",
    "rt",
    "raw",
    "rt_raw_block_model.rds"
  )
  )

summary(rt_raw_model_block)
anova(rt_raw_model_block, type = 3)

rt_raw_model_block_tas <- lmerTest::lmer(
  rt ~ block * tas_z + (1 + block | pid) + (1 | cue_target),
  data = drop_na(data, tas_z, em_z),
  REML = FALSE
)

rt_raw_model_block_tas %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "rt",
      "raw",
      "rt_raw_block_tas_model.rds"
    ),
  )
anova(rt_raw_model_block_tas, type = 3)


rt_raw_model_block_tas_emo <- lmerTest::lmer(
  rt ~ block * tas_z + block * em_z + (1 + block | pid) + (1 | cue_target),
  data = drop_na(data, tas_z, em_z),
  REML = FALSE
)

rt_raw_model_block_tas_emo %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "rt",
      "raw",
      "rt_raw_block_tas_msi_model.rds"
    )
  )

anova(rt_model_block_tas_emo, type = 3)



anova(rt_model_block, rt_model_block_tas, rt_model_block_tas_emo)

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
    )[["dprime"]],
    cue_emotion = cue_emotion
  ) %>% 
  select(
    pid,
    block,
    d_prime,
    cue_emotion
  ) %>% 
  right_join(
    select(surveys, pid, tas_z, em_z)
  ) %>% 
  drop_na(tas_z, em_z)

d_raw_model_block <- lmerTest::lmer(
  d_prime ~ block + (1| pid ),
  data = data_agg,
  REML = FALSE
)

d_raw_model_block %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "d_prime",
      "raw",
      "d_raw_block_model.rds"
    )
  )

d_raw_model_block_tas <- lmerTest::lmer(
  d_prime ~ block * tas_z + (1| pid ),
  data = data_agg,
  REML = FALSE
)

d_raw_model_block_tas %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "d_prime",
      "raw",
      "d_raw_block_tas_model.rds"
    )
  )

d_raw_model_block_tas_emo <- lmerTest::lmer(
  d_prime ~ block * tas_z + block * em_z + (1| pid ),
  data = data_agg,
  REML = FALSE
)

d_raw_model_block_tas_emo %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "d_prime",
      "raw",
      "d_raw_block_tas_emo_model.rds"
    )
  )

anova(d_model_block, d_model_block_tas, d_model_block_tas_emo)

# Imputed data ------------------------------------------------------------


# Reaction time -----------------------------------------------------------

rt_imputed_model_block <- lmerTest::lmer(
  rt ~ block  + (1 + block | pid) + (1 | cue_target),
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

rt_imputed_model_block_tas <- lmerTest::lmer(
  rt ~ block * tas_imputed_z + (1 + block | pid) + (1 | cue_target),
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


rt_imputed_model_block_tas_emo <- lmerTest::lmer(
  rt ~ block * tas_imputed_z + block * em_imputed_z + (1 + block | pid) + (1 | cue_target),
  data = data,
  REML = FALSE
)

rt_imputed_model_block_tas_emo %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "rt",
      "imputed",
      "rt_imputed_block_tas_msi_model.rds"
    )
  )

anova(rt_imputed_model_block_tas_emo, type = 3)


anova(rt_imputed_model_block, rt_imputed_model_block_tas, rt_imputed_model_block_tas_emo)

# note no change in pattern of results

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

d_imputed_model_block_tas_emo <- lmerTest::lmer(
  d_prime ~ block * tas_imputed_z + block * em_imputed_z + (1| pid ),
  data = data_agg,
  REML = FALSE
)

d_imputed_model_block_tas_emo %>% 
  write_rds(
    here::here(
      "results",
      "models",
      "d_prime",
      "imputed",
      "d_imputed_block_tas_emo_model.rds"
    )
  )

anova(d_imputed_model_block, d_imputed_model_block_tas, d_imputed_model_block_tas_emo)

