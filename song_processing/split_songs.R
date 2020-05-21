library("av")
library("tidyverse")

# Happy Songs 
# Here I need to split happy songs from Song et al into 1 second bursts and export them to a shortened folder 

#Set path 
happy_full_path <- "/Users/joellarwood/Dropbox/Joel PhD/music_emotions_song_et_al_2016/happy"
happy_short_path <- "/Users/joellarwood/Dropbox/Joel PhD/music_emotions_song_et_al_2016/happy/1_sec"

happy_songs <- list.files(happy_full_path, 
                          pattern = ".mp3")

for (song in 1:length(happy_songs)){
  song = happy_songs[song] 
  song_path = paste0(happy_full_path, "/", song)
  for (time in 0:30){
  start_time = time
  snippet_name = paste0(time, "_", song)
  av::av_audio_convert(
    audio = song_path, 
    output = paste0(happy_short_path, "/", snippet_name), 
    start_time = time, 
    total_time = 1
  )
  print(snippet_name)
    }
}

# Sad Songs 
# Here I need to split happy songs from Song et al into 1 second bursts and export them to a shortened folder 

#Set path 
sad_full_path <- "/Users/joellarwood/Dropbox/Joel PhD/music_emotions_song_et_al_2016/sad"
sad_short_path <- "/Users/joellarwood/Dropbox/Joel PhD/music_emotions_song_et_al_2016/sad/1_sec"

sad_songs <- list.files(sad_full_path, 
                          pattern = ".mp3")

for (song in 1:length(sad_songs)){
  song = sad_songs[song] 
  song_path = paste0(sad_full_path, "/", song)
  for (time in 0:30){
    start_time = time
    snippet_name = paste0(time, "_", song)
    av::av_audio_convert(
      audio = song_path, 
      output = paste0(sad_short_path, "/", snippet_name), 
      start_time = time, 
      total_time = 1
    )
    print(snippet_name)
  }
}

# Angry Songs 
# Here I need to split angry songs from Song et al into 1 second bursts and export them to a shortened folder 

#Set path 
angry_full_path <- "/Users/joellarwood/Dropbox/Joel PhD/music_emotions_song_et_al_2016/angry"
angry_short_path <- "/Users/joellarwood/Dropbox/Joel PhD/music_emotions_song_et_al_2016/angry/1_sec"

angry_songs <- list.files(angry_full_path, 
                          pattern = ".mp3")

for (song in 1:length(angry_songs)){
  song = angry_songs[song] 
  song_path = paste0(angry_full_path, "/", song)
  for (time in 0:30){
    start_time = time
    snippet_name = paste0(time, "_", song)
    av::av_audio_convert(
      audio = song_path, 
      output = paste0(angry_short_path, "/", snippet_name), 
      start_time = time, 
      total_time = 1
    )
    print(snippet_name)
  }
}


# Relaxed Songs 
# Here I need to split relaxed songs from Song et al into 1 second bursts and export them to a shortened folder 

#Set path 
relaxed_full_path <- "/Users/joellarwood/Dropbox/Joel PhD/music_emotions_song_et_al_2016/relaxed"
relaxed_short_path <- "/Users/joellarwood/Dropbox/Joel PhD/music_emotions_song_et_al_2016/relaxed/1_sec"

relaxed_songs <- list.files(relaxed_full_path, 
                          pattern = ".mp3")

for (song in 1:length(relaxed_songs)){
  song = relaxed_songs[song] 
  song_path = paste0(relaxed_full_path, "/", song)
  for (time in 0:30){
    start_time = time
    snippet_name = paste0(time, "_", song)
    av::av_audio_convert(
      audio = song_path, 
      output = paste0(relaxed_short_path, "/", snippet_name), 
      start_time = time, 
      total_time = 1
    )
    print(snippet_name)
  }
}