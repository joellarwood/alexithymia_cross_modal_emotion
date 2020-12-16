
##################################################################
##                  Impute missing survey data                  ##
##################################################################


# Load pacakges -----------------------------------------------------------

library(tidyverse)
library(missRanger)



# Import data and clean for estimation ------------------------------------

# note the reverse coding has already occured for this dataset
data_missing <- here::here(
  "results",
  "data",
  "processed",
  "survey_scores.rds"
) %>%
  read_rds() %>%
  select(
    -contains("_z"),
    -tas,
    -eot,
    -ddf,
    -dif,
    -ae,
    -pa,
    -em,
    -mt
  )

# Here I want to get the proprtion of missing for each item 

prop_miss_col <- colMeans(is.na(data_missing)) %>% 
  as.data.frame() %>% 
  filter(. > 0) %>% 
  arrange(desc(.))

max(prop_miss_col) * 100 

prop_miss_p <- naniar::prop_miss_row(data_missing) %>% 
  as.data.frame() %>% 
  filter(. > 0) %>% 
  arrange(desc(.))

max(prop_miss_p) * 100 

# Most cases don't have musch missing data. I will estimate data for participnats who completed > 90% of items

data_imputed <- data_missing %>%
  naniar::add_prop_miss() %>% 
  filter(prop_miss_all < .1) %>% 
  missRanger::missRanger(
    formula = . ~ . - pid - prop_miss_all,
    pmm.k = 3
  )

data_imputed_z <- data_imputed %>%
  mutate(
    ddf = rowSums(
      select(
        .,
        paste(
          "tas",
          c("2", "4", "11", "12", "17"),
          sep = "_"
        )
      )
    ),
    dif = rowSums(
      select(
        .,
        paste(
          "tas",
          c("1", "3", "6", "7", "9", "13", 14),
          sep = "_"
        )
      )
    ),
    eot = rowSums(
      select(
        .,
        paste(
          "tas",
          c("5", "8", "10", "15", "16", "18", "19", "20"),
          sep = "_"
        )
      )
    ),
    tas = ddf + dif + eot,
    ae = rowMeans(select(., contains("ae"))),
    em = rowSums(select(., contains("em"))),
    pa = rowMeans(select(., contains("pa"))),
    mt = rowMeans(select(., contains("mt")))
  ) %>% 
  mutate(
    across(
      c(ddf:mt), ~ scale(.x)[,1], 
      .names = "{col}_imputed_z"
    )
  )

data_imputed_z %>% 
  write_rds(
    here::here(
      "results",
      "data",
      "processed",
      "survey_scores_imputed.rds"
    )
  )


