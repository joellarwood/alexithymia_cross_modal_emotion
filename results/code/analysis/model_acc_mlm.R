##################################################################
##                        Model Accuracy                        ##
##                  Using a Multilvel Framework                 ##
##    Refer to: Wright et el (2009) 110.1348/000711008X327632   ##
##################################################################

# Load data ---------------------------------------------------------------

surveys_scores <- here::here(
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
    select(surveys, pid, contains("z")),
    by = "pid"
  ) %>% 
  left_join(
    select(surveys_imputed, pid, contains("z")),
    by = "pid"
  ) %>% 
  mutate(
    congruence_num = case_when(
      congruence == "congruent" ~ 1,
      congruence == "incongruent" ~ 0
    ),
    response_num = case_when(
      response == "congruent" ~ 1,
      response == "incongruent" ~ 0
    )
  )

glimpse(trial_data)

# Fit models --------------------------------------------------------------


# Block Only  -------------------------------------------------------------

block_ac <- lme4::glmer(
  response_num ~ block * congruence_num + (block | pid),
  data = trial_data,
  family=binomial(link=probit)
)


# Block by TAS ------------------------------------------------------------

block_by_tas_ac <- lme4::glmer(
  response_num ~ block * tas_z * congruence_num + (congruence_num | pid),
  data = trial_data,
  family=binomial(link=probit)
)

# Ultimately the takeaway here is that having it as an MLM doesn't change the interprtation or aid understanding 
