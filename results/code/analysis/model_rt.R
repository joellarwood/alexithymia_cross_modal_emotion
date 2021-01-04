#################################################################
##                     Model Reaction Time                     ##
#################################################################


# Load Pacakges -----------------------------------------------------------

library(tidyverse)
library(lmerTest)
library(emmeans)

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

trial_data <- trials %>% 
  left_join(
    select(survey_scores, pid, contains("z")),
    by = "pid"
  )

glimpse(trial_data)

# Reaction Time -----------------------------------------------------------


# Block Only  -------------------------------------------------------------

block_rt <- lmerTest::lmer(
  rt~ block + (block | pid),
  data = drop_na(trial_data, tas_imputed_z)
)
# Model did not converge when random effects for stimulus were added 

anova(block_rt, 3)


# # Block and TAS main effect -----------------------------------------------
# 
# block_tas_rt <- lmer(
#   rt~ block + tas_imputed_z + (block | pid),
#   data = trial_data
# )
# 
# anova(block_tas_rt, 3)

# Block and TAS interaction -----------------------------------------------

block_by_tas_rt <- lmerTest::lmer(
  rt~ block * tas_imputed_z + (block | pid),
  data = drop_na(trial_data, tas_imputed_z)
)

anova(block_by_tas_rt, 3)



# Compare two models ------------------------------------------------------

anova(block_rt, block_by_tas_rt, type = 3)


# Emmeans of preferred model ----------------------------------------------

block_emmeans <- lmerTest::difflsmeans(block_rt)

block_emmeans

# Block and Music Emotions  -----------------------------------------------

block_em_rt <- lmer(
  rt~ block + em_imputed_z + (block | pid),
  data = trial_data
)

anova(block_em_rt, 3)
# Block by Music Emotions  -----------------------------------------------

block_by_em_rt <- lmer(
  rt~ block * em_imputed_z + (block | pid),
  data = trial_data
)

anova(block_by_em_rt, 3)
# Block and Perceptual Ability  -----------------------------------------------

block_pa_rt <- lmer(
  rt~ block + pa_imputed_z + (block | pid),
  data = trial_data
)

anova(block_pa_rt, 3)
# Block by Perceptual Ability  -----------------------------------------------

block_by_pa_rt <- lmer(
  rt~ block * pa_imputed_z + (block | pid),
  data = trial_data
)

anova(block_by_pa_rt, type = 3)


# Block and all individual differences  -----------------------------------

block_max_rt <- lmer(
  rt~ block * tas_imputed_z + block * em_imputed_z + block * pa_imputed_z + (block | pid),
  data = trial_data
)

anova(block_max_rt, 3)



