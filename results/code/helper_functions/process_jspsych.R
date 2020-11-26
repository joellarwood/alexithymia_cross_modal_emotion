
##################################################################
##                     Processing Functions                     ##
##                      Reading from JSON                       ##
##                  Extract RT and survey data                  ##
##                     Script: Joel Larwood                     ##
##################################################################


# Load pacakges -----------------------------------------------------------

library(jsonlite)
library(magrittr)
library(forcats)
library(dplyr)
library(tidyr)
library(stringr)


# Load JSON and retain needed columns -------------------------------------

jspsych_data <- function(file){
  # read in JSON file from jsPSych
  tmp <- jsonlite::stream_in(con = file(file))
  # select needed variables
  tmp <- select(
    tmp, 
    contains(c(
      "trial",
      "internal_node_id",
      "rt",
      "task",
      "block",
      "correct",
      "cue_song", 
      "target_stimulus",
      "target_emotion",
      "condition", 
      "key_press",
      "responses"
    )
    )
  )
  # add .0 to the rows without a postscript for uniform column naming for loop 
  tmp <-  rename_with(tmp,
                      .cols = !contains("."),
                      ~paste0(.x, ".0")) 
}



# Get number of participants ----------------------------------------------
get_n <- function(data){
  max(as.numeric(gsub("^([^.]+\\.)","", colnames(data)))) # regex to match [word.] leaving only the number
}



# Get trial data ----------------------------------------------------------


get_trial_data <- function(data){
  #have blank data frame to append to 
  tmp_loop <- data.frame()
  
  for (i in 0:get_n(data)){
    # select participant
    pid <- i
    print(pid)
    # get trial data
    tmp_trials <- select(
      data,
      paste(
        c(
          "internal_node_id",
          "rt",
          "trial",
          "task",
          "block",
          "correct",
          "cue_song", 
          "target_stimulus",
          "target_emotion",
          "condition", 
          "key_press"
        ),
        pid,
        sep = "." # select rows for participant
      )
    )
    # delete the .num aspect of column names for rbind 
    tmp_trials <- rename_with(
      tmp_trials,
      ~gsub(".[[:digit:]]{1,2}", 
            "",
            .x)
    )
    
    tmp_trials["pid"] <- pid
    
    tmp_loop <- bind_rows(tmp_loop, tmp_trials) # stack long data frames ontop of eachotehr
  }
  
  # tidy up the trials data
  tmp_loop <- tmp_loop %>% 
    dplyr::filter(
      task %in% c("cue", "target"),
      !is.na(rt),
      trial != "false"
    ) %>% 
    tidyr::separate(
      col = internal_node_id, 
      into = c("node_1", "node_2", "node_3", "node_4", "node_5"),
      sep = "-",
    ) %>% 
    tidyr::separate(
      col = node_5,
      into = c("tmp", "block_rep")
    ) %>% 
    dplyr::select(
      -tmp,
      -contains("node")
    ) %>% 
    dplyr::mutate(
      # give semantic labels to keypress
      response = case_when( 
        key_press == 37 ~ "congruent",
        key_press == 39 ~ "incongruent"
      ),
      across(
        where(is.character),
        as.factor),
      pid = as.factor(pid),
      # code correct response
      correct_response = as.factor(
        case_when(
          condition == "congruent" & key_press == 37 | condition == "incongruent" & key_press == 39 ~ "correct",
          condition == "congruent" & key_press == 39 | condition == "incongruent" & key_press == 37 ~ "incorrect"
        )
      ),
      # data for d prime
      # present == correct match 
      hit = case_when(
        condition == "congruent" & response == "congruent" ~ TRUE,
        TRUE ~ FALSE
      ),
      # correct reject when incongruent 
      cor_rej = case_when(
        condition == "incongruent" & response == "incongruent" ~ TRUE,
        TRUE ~ FALSE
      ),
      false_alarm = case_when(
        condition == "incongruent" & response == "congruent" ~ TRUE,
        TRUE ~ FALSE
      ),
      miss = case_when(
        condition == "congruent" & response == "incongruent" ~ TRUE,
        TRUE ~ FALSE
      ),
      block = forcats::fct_relevel(block, "word")
    ) 
  
  return(tmp_loop)
}
    




# Get survey responses ----------------------------------------------------

get_survey_data <- function(data) {
  # init blank survey to add results to
  survey_loop <- data.frame()
  for (i in 0:get_n(data)) {

    # get pid
    pid <- i

    tmp_survey <- paste("responses", pid, sep = ".")
    
    print(paste0("Process participant #", pid))
    

    # get tas

    tmp_surveys <- data %>%
      select(paste("responses", pid, sep = ".")) %>%
      drop_na() #%>% 
      # filter(
      #   grepl("tas", tmp_survey) 
      #   ) #%>%
      # as.character() %>%
      # jsonlite::parse_json() %>%
      # data.frame()

    
    tmp_tas <- jsonlite::parse_json(tmp_surveys[1,1]) %>% 
      as.data.frame()
    
    tmp_msi <- jsonlite::parse_json(tmp_surveys[2,1]) %>% 
      as.data.frame()
    
    tmp_age <- jsonlite::parse_json(tmp_surveys[3,1]) %>% 
      as.data.frame() %>% 
      rename(
        "age" = "Q0"
      )
    
    tmp_gender <- jsonlite::parse_json(tmp_surveys[4,1]) %>% 
      as.data.frame() %>% 
      rename(
        "gender" = "Q0"
      )
    
    tmp_responses <- bind_cols(tmp_tas, tmp_msi, tmp_age, tmp_gender) %>% 
      mutate(
        across(
          where(is.numeric),
          ~ .x + 1
        )
      ) %>%
      mutate_all(
        as.character
      ) %>% 
      mutate_
          
    
    tmp_responses["pid"] <- as.factor(pid)
    
    glimpse(tmp_responses)
    
    survey_loop <- bind_rows(survey_loop, tmp_responses)
  }
  
  return(survey_loop)
}
    
    