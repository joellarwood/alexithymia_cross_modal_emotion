Results from lab pilot
================

``` r
library(tidyverse)
```

``` r
lab_data <- here::here(
  "analysis",
  "lab_pilot", 
  "data",
  "processed",
  "combined_lab_pilot_data.csv"
) %>% 
  read_csv()
```

# Overview

The pilot presented a range of musical clips (1 sec in duration) that
were taken from songs that had been tagged as happy, sad, angry, or
relaxed on last.fm. Participants rated the emotion of the musical clip,
it’s expressed valence and expressed arousal.

As shown below, the data have been structure so that each trial is in
one row. Target refelcts what to song was tagged as and emotion reflects
the emotion the participant labelled the clip
as.

| subject         | trial\_index | stimulus\_song                                | target  | emotion | valence | arousal | correct |
| :-------------- | -----------: | :-------------------------------------------- | :------ | :------ | ------: | ------: | :------ |
| mr02t2o5o3n8nu8 |           24 | audio/4\_Come\_On\_Baby.mp3                   | relaxed | relaxed |      28 |      24 | TRUE    |
| s20encxw28459xp |            3 | audio/15\_Here\_Today.mp3                     | sad     | happy   |      62 |      59 | FALSE   |
| qd7yfgtyypvrqy3 |            9 | mp3/7\_Digital.mp3                            | relaxed | sad     |      50 |      64 | FALSE   |
| mr02t2o5o3n8nu8 |           13 | audio/3\_Walk\_on\_the\_wild\_side.mp3        | relaxed | relaxed |      59 |      63 | TRUE    |
| mr02t2o5o3n8nu8 |           43 | audio/8\_Fire\_And\_Rain\_\_(LP\_Version).mp3 | sad     | sad     |      24 |      76 | TRUE    |
| mr02t2o5o3n8nu8 |           40 | audio/4\_Destiny.mp3                          | relaxed | happy   |      65 |      77 | FALSE   |
| wto40f559ergwfw |           42 | audio/2\_Love’s\_Requiem.mp3                  | sad     | sad     |      26 |      66 | TRUE    |
| qd7yfgtyypvrqy3 |           16 | mp3/12\_Skinny\_Love.mp3                      | sad     | sad     |       7 |      16 | TRUE    |
| vzpuh9a0oxg5jbc |           38 | audio/5\_Love’s\_Requiem.mp3                  | sad     | sad     |      41 |      35 | TRUE    |
| r5l2vw7lqxdnfg7 |            1 | audio/24\_Restless.mp3                        | happy   | happy   |      77 |      84 | TRUE    |

# Calculating accuracy

## Participant ranges

It would be helpful to see if there is variance in participant
responses.

``` r
p_id <- lab_data %>% 
  select(subject) %>% 
  distinct() %>% 
  as.list() %>% 
  .$subject

lab_accuracy <- data.frame(stringsAsFactors = FALSE)

for (i in 1:length(p_id)){
  sub_tmp <- p_id[i]
  data_tmp <- lab_data %>% 
    filter(subject == sub_tmp)
  obs_tmp <- nrow(data_tmp) 
  correct_tmp <- data_tmp %>% 
    filter(correct == TRUE) %>% 
    nrow()
  acc_tmp <- ((correct_tmp/obs_tmp) * 100)
  row_tmp <- cbind(sub_tmp, obs_tmp, correct_tmp, acc_tmp)
  lab_accuracy <- rbind(lab_accuracy, row_tmp)
}

fac_to_num <- function(.x){
  as.numeric(as.character(.x))
}

lab_accuracy <- lab_accuracy %>% 
  mutate_at(.vars = vars("obs_tmp", "correct_tmp", "acc_tmp"), fac_to_num)
```

The results indicate that the mean accuracy was 62.2222222 with a
standard deviation of 14.3443828
