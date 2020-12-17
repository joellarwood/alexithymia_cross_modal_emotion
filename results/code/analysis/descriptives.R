
##################################################################
##                         Descriptives                         ##
##################################################################


# Load Packages -----------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(psych) # Procedures for Psychological, Psychometric, and Personality Research
library(MissMech) # Testing Homoscedasticity, Multivariate Normality, and Missing Completely at Random 

# Load data ---------------------------------------------------------------
# Raw Data
surveys <- here::here(
  "results",
  "data",
  "processed",
  "survey_scores.rds"
) %>%
  read_rds()

glimpse(surveys)
# Imputed data

surveys_imputed <- here::here(
  "results",
  "data",
  "processed",
  "survey_scores_imputed.rds"
) %>%
  read_rds()

glimpse(surveys_imputed)



# Explore Missingness -----------------------------------------------------

observed <- surveys %>%
  select(tas_1:tas_20, ae_01:mt_06, age, gender)

visdat::vis_miss(observed) 

naniar::gg_miss_var(observed) # No variable had more than 5% missing cases

naniar::pct_miss_case(observed) # 30% of participants have at least 1 missing case

naniar::pct_miss_var(observed) # 83% of variables have at least one missing case

naniar::gg_miss_var(observed)


# Describe Data (Imputed set) ---------------------------------------------

#details of the imputation can be found in code/data_cleaning/impute_surveys.R

# Reverse coding has already occured 

jl_alpha <- function(data, vars){
  psych::alpha(select(data,
                      vars))$total$raw_alpha
}



descriptives <- surveys_imputed %>% 
  select(
    tas:mt,
    age
  ) %>% 
  mutate(
    age = as.numeric(age)
  ) %>% 
  psych::describe() %>% 
  select(n, 
         mean,
         sd) %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  mutate(
    alpha = case_when(
      variable == "tas" ~ jl_alpha(surveys_imputed, paste0("tas_", c(1:20))),
      variable == "ae" ~ jl_alpha(surveys_imputed, paste0("ae_0", c(1, 5, 7))),
      variable == "em" ~ jl_alpha(surveys_imputed, paste0("em_0", c(1:6))),
      variable == "pa" ~ jl_alpha(surveys_imputed, paste0("pa_0", c(3, 6, 7))),
      variable == "mt" ~ jl_alpha(surveys_imputed, paste0("mt_0", c(1, 2, 6)))
    ),
    variable = case_when(
      variable == "tas" ~ "TAS-20",
      variable == "ae" ~ "Active Engagement",
      variable == "em" ~ "Emotions",
      variable == "pa" ~ "Perceptual Abilities",
      variable == "mt" ~ "Musical Training",
      variable == "age" ~ "Age"
    ),
    across(
      where(is.numeric),
      round, 2
    )
  ) 
 

# Get Correlations --------------------------------------------------------

surveys_imputed %>% 
  select(
    tas:mt,
    age
  ) %>% 
  mutate(
    age = as.numeric(age)
  ) %>% 
  dplyr::relocate(
    tas, em, pa, everything()
  ) %>% 
  apaTables::apa.cor.table(
    filename = here::here(
      "results",
      "outputs", 
      "correlations.rtf")
  )


