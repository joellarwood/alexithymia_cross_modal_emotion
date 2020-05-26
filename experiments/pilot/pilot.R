## Pilot Study - Validate Music Bursts
## Joel Larwood
## Written using jaysire

library(jaysire)
library(here)
library(magrittr)

## Set Instructions

welcome <- jaysire::trial_instructions(
  pages = "<p>In this study you will hear a short musical clip and then make some ratings about the emotion it expresses or conveys</p><p>First you will select which emotion you think the musical clip is expressing</p><p>You will then rate how pleasant the feeling expressed in the musical clip was and also how energetic it was</p> <p>Press the right arrow to move forward in the study</p>",
  key_forward = keycode("right arrow")
)

## Set ITI
### 1000ms

iti_1000 <- jaysire::trial_html_keyboard_response(
  stimulus = '<div style="font-size:60px;">+</div>',
  choices = jaysire::respond_no_key(),
  stimulus_duration = 1000,
  data = jaysire::insert_property(stage = "fixation")
)

iti_500 <- jaysire::trial_html_keyboard_response(
  stimulus ='<div style="font-size:60px;color:green">+</div>',
  choices = jaysire::respond_no_key(),
  stimulus_duration = 500,
  data = jaysire::insert_property(stage = "fixation")
)

iti <- jaysire::build_timeline(iti_1000)

## Set music trials
### Set key files

songs <- here::here(
  "experiments",
  "pilot",
  "songs"
)

song_names <- list.files(songs)

print(songs)

### song trial

song_trial <- jaysire::trial_audio_keyboard_response(
  stimulus = jaysire::insert_variable("song"),
  choices = jaysire::respond_no_key(),
  trial_duration = 1000,
  data = jaysire::insert_property(stage = "song")
)

### Emotion rating

emo_rate <- jaysire::trial_html_button_response(
  stimulus = "What emotion was expressed in the music clip?",
  choices = c(
    "Happy",
    "Sad",
    "Angry",
    "Relaxed",
    "None of these"
  ),
  response_ends_trial = TRUE,
  data = jaysire::insert_property(stage = "emotion_rating")
)

valence_rate <- jaysire::trial_html_slider_response(
  stimulus = "How pleasant/unpleasant was the feeling expressed in the msuic clip?",
  labels = c(
    "Very Unpleasant",
    "Neutral",
    "Very Pleasant"
  ),
  data = jaysire::insert_property(stage = "valence rating")
)

arousal_rate <- jaysire::trial_html_slider_response(
  stimulus = "How active or arousing was the feeling expressed in the msuic clip?",
  labels = c(
    "Very Low",
    "Neutral",
    "Very High"
  ),
  data = jaysire::insert_property(stage = "arousal rating")
)

## Build music trails timeline

music_trials <- jaysire::build_timeline(
  song_trial, emo_rate, valence_rate, arousal_rate
) %>%
  jaysire::set_variables(song = jaysire::insert_resource(song_names)) %>%
  jaysire::set_parameters(randomize_order = TRUE)

## Give thanks 

thanks <- jaysire::trial_html_keyboard_response(
  stimulus = "Thankyou"
)

# Build study 

## set output 
output <- here::here(
  "experiments",
  "pilot"
)

jaysire::build_experiment(
  timeline = jaysire::build_timeline(welcome, music_trials, thanks),
  resources = jaysire::build_resources(songs),
  columns = jaysire::insert_property(experiment = "pilot"),  
  path = output,
  on_finish = jaysire::save_locally()
)

