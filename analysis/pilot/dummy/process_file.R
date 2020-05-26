## processing pilot study data

library(here)
library(tidyverse)
library(janitor)

## Import file 

dummy_data <- here::here(
  "analysis",
  "pilot",
  "dummy", 
  "dummy_data.csv"
  ) %>% 
  read_csv() %>% 
  dplyr::select(
    subject,
    internal_node_id,
    stimulus,
    test_part, 
    target, 
    responses, 
    response
    ) %>% 
  dplyr::filter(
    test_part != "welcome", 
    test_part != "thanks", 
    test_part != "fixation",
    test_part != "instruction") %>% 
  janitor::clean_names()

# Internal node id defines which iteration of trials using last value 

dummy_data_trial_id <- dummy_data %>% 
  tidyr::separate(
    col = internal_node_id, 
    into = c("node_1", "node_2", "node_3"),
    sep = "-",
  )  %>% 
  tidyr::separate(
    col = node_3, 
    into = c("node_3_lead", "trial_index"), 
    remove = FALSE
  ) %>% 
  dplyr::select(
    -node_1, 
    -node_2, 
    -node_3, 
    -node_3_lead
  )

wide <- dummy_data_trial_id %>% 
  pivot_wider(id_cols = trial_index, 
              names_from = "test_part", 
              values_from = c("subject", "stimulus", "target", "responses", "response")) %>%
  janitor::clean_names() %>% 
  dplyr::select(
    subject_song,
    trial_index,
    stimulus_song,
    target_song,
    responses_emotion_rating, 
    response_valence_rating, 
    response_arousal_rating
  ) %>% 
  dplyr::rename(
    subject = subject_song, 
    target = target_song,
    emotion = responses_emotion_rating,
    valence = response_valence_rating,
    arousal = response_arousal_rating
  ) %>% 
  tidyr::separate(
    emotion, 
    into = c("QO", "emotion"), 
    sep = '\":"'
  ) %>% 
  dplyr::select(
    -QO
  ) %>% 
  dplyr::mutate(
    emotion = tolower(as.factor(gsub('(["]})', "", emotion)))
  ) %>% 
  dplyr::mutate(
    correct = if_else(
      emotion == target, 
      TRUE, 
      FALSE
    )
  )



