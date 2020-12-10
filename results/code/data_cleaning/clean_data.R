
#################################################################
##               Alexithymia and Muscal Emotions               ##
##                        jsPsych Import                       ##
#################################################################


# Load packages -----------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(rjson) # JSON for R


# load in data

json_raw <- here::here(
  "results",
  "data",
  "raw",
  "alexithymia_cross_modal_full.txt"
) %>%
  read_file()

json_flat <- gsub("\\]\\s+\\[", ",", json_raw) # reg ex mnakes on big json instead of multiple lines

json_conv <- jsonlite::fromJSON(json_flat)


# Extract trial data ------------------------------------------------------

trials <- json_conv %>%
  filter(
    block %in% c("face", "music", "word") &
      task %in% c("target", "cue")
  ) %>%
  select(
    internal_node_id,
    subject,
    task,
    block,
    stimulus,
    cue_song,
    cue_emotion,
    target_stimulus,
    target_emotion,
    condition,
    rt,
    key_press
  ) %>%
  mutate(
    index = gsub(".*\\.", "", internal_node_id)
  ) %>%
  select(
    index,
    subject,
    block,
    task,
    stimulus,
    cue_emotion,
    target_emotion,
    condition,
    rt,
    key_press
  ) %>%
  pivot_wider(
    names_from = task,
    values_from = c(
      stimulus,
      cue_emotion,
      target_emotion,
      condition,
      rt,
      key_press
    )
  ) %>%
  transmute(
    pid = as.factor(subject),
    block = block,
    cue_stimulus = stimulus_cue,
    target_stimulus = stimulus_target,
    cue_emotion = cue_emotion_cue,
    target_emotion = target_emotion_target,
    congruence = condition_target,
    rt = rt_target,
    key_press = key_press_target,
    cue_target = paste(cue_stimulus, target_stimulus, sep = "*"),
    response = case_when(
      key_press == 37 ~ "congruent",
      key_press == 39 ~ "incongruent"
    ),
    correct_response = as.factor(
      case_when(
        congruence == "congruent" & key_press == 37 |
          congruence == "incongruent" & key_press == 39 ~ "correct",
        congruence == "congruent" & key_press == 39 |
          congruence == "incongruent" & key_press == 37 ~ "incorrect"
      )
    ),
    # data for d prime
    # present == correct match
    hit = case_when(
      congruence == "congruent" &
        response == "congruent" ~ TRUE,
      TRUE ~ FALSE
    ),
    # correct reject when incongruent
    cor_rej = case_when(
      congruence == "incongruent" &
        response == "incongruent" ~ TRUE,
      TRUE ~ FALSE
    ),
    false_alarm = case_when(
      congruence == "incongruent" &
        response == "congruent" ~ TRUE,
      TRUE ~ FALSE
    ),
    miss = case_when(
      congruence == "congruent" &
        response == "incongruent" ~ TRUE,
      TRUE ~ FALSE
    ),
    block = forcats::fct_relevel(block, "word")
  )

trials %>%
  write_rds(
    here::here(
      "results",
      "data",
      "processed",
      "trials.rds"
    )
  )


# Get Survey Responses ----------------------------------------------------

surveys <- json_conv %>%
  filter(!is.na(responses)) %>%
  select(
    subject,
    responses
  ) %>%
  mutate(json = map(responses, ~ jsonlite::fromJSON(.) %>%
    as.data.frame() %>%
    mutate_all(as.character))) %>%
  unnest(cols = c(json)) %>%
  select(-responses) %>%
  mutate(
    across(c(tas_1:mt_06), as.numeric),
    across(where(is.numeric), ~ .x + 1)
  )


# TAS ---------------------------------------------------------------------


tas <- surveys %>%
  select(
    subject,
    contains("tas")
  ) %>%
  mutate(
    across(
      c(tas_4, tas_5, tas_10, tas_18, tas_19),
      ~ 6 - .x
    )
  ) %>%
  mutate(
    pid = as.factor(subject),
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
    tas = ddf + dif + eot
  ) %>%
  filter(
    !is.na(tas) | !is.na(ddf) | !is.na(eot) | !is.na(dif)
  ) %>%
  select(-subject)

tas %>%
  write_rds(
    here::here(
      "results",
      "data",
      "processed",
      "tas.rds"
    )
  )








# Gold MSI ----------------------------------------------------------------

msi <- surveys %>%
  select(
    subject,
    ae_01:mt_06
  ) %>%
  mutate(
    across(
      c(em_02, pa_03),
      ~ 8 - .x
    ),
  ) %>% 
mutate( # use means for the short version of scales but sum for emotion
  pid = as.factor(subject),
  ae = rowMeans(select(., contains("ae"))),
  em = rowSums(select(., contains("em"))),
  pa = rowMeans(select(., contains("pa"))),
  mt = rowMeans(select(., contains("mt")))
) %>% 
  filter(
    !is.na(ae) | !is.na(em) | !is.na(pa) | !is.na(mt)
    ) %>% 
  select(
    -subject
  )

msi %>%
  write_rds(
    here::here(
      "results",
      "data",
      "processed",
      "msi.rds"
    )
  )



# age and gender ----------------------------------------------------------

demos <- surveys %>% 
  select(
    subject, 
    Q0
  ) %>% 
  mutate(
    pid = as.factor(subject)
  )

demos["measure"] <- rep(c("na1", "na2", "age", "gender"), times = 300)

demos_widen <- demos %>% 
  pivot_wider(
    id_cols = pid,
    names_from = "measure",
    values_from = "Q0"
  ) %>% 
  select(
    pid,
    gender, 
    age
  )

demos_widen %>%
  write_rds(
    here::here(
      "results",
      "data",
      "processed",
      "age_gender.rds"
    )
  )



# Surveys -----------------------------------------------------------------

survey_scores <- tas %>% 
  full_join(msi, by = "pid") %>% 
  full_join(demos_widen, by = "pid") %>% 
  mutate(
    across(c(ddf:tas, ae:mt), 
    ~ scale(.x)[,1], 
    .names = "{col}_z"
  )
  )


survey_scores %>%
  write_rds(
    here::here(
      "results",
      "data",
      "processed",
      "survey_scores.rds"
    )
  )
