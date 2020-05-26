library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggwordcloud)

library(leaflet)
library(plotly)
library(DT)

library(rtweet)
library(tidytext)
library(qdap)
library(qdapDictionaries)
library(htmltools)

# Convert to an SPDF
library(rgdal)
library(KernSmooth)

library(rJava)
library(mailR)

# Plot theme
tweetPlotTheme <- theme(panel.background = element_blank(),
                        panel.grid.major = element_line(color = 'gray'),
                        panel.grid.minor = element_line(color = 'gray'),
                        axis.line = element_line(),
                        panel.border = element_rect(color = "black", fill=NA),
                        axis.title = element_text(size=10),
                        axis.text = element_text(size = 10),
                        plot.title = element_text(size=10),
                        legend.background = element_blank(),
                        legend.text = element_text(size = 10),
                        legend.title = element_text(size = 10),
                        legend.position = "top",
                        legend.direction = "horizontal")

# colors of emotions based on Plutchik's colour wheel of emotion
emoColors <- c("anger" = "#fe0000", "anticipation" = "#fea853", "disgust" = "#ff54ff", 
               "fear" = "#009600", "joy" = "#ffff54", "sadness" = "#5151ff", 
               "surprise" = "#5abdff", "trust" = "#52ff4f")

# kriging data
krigingDel_case <- read.csv("./data/Delhi_kriging_data_case.csv")
krigingDel_hosp <- read.csv("./data/Delhi_kriging_data_hosp.csv")

krigingBan_case <- read.csv("./data/Bangkok_kriging_data_case.csv")
krigingBan_hosp <- read.csv("./data/Bangkok_kriging_data_hosp.csv")

krigingJak_case <- read.csv("./data/Jakarta_kriging_data_case.csv")
krigingJak_hosp <- read.csv("./data/Jakarta_kriging_data_hosp.csv")

krigingMum_case <- read.csv("./data/Mumbai_kriging_data_case.csv")
krigingMum_hosp <- read.csv("./data/Mumbai_kriging_data_hosp.csv")

# epi time series data
epi_data_bang <- read.csv("./data/epi_data_bangkok.csv") 
epi_data_delhi <- read.csv("./data/epi_data_new delhi.csv")
epi_data_mumbai <- read.csv("./data/epi_data_mumbai.csv")
epi_data_jakarta <- read.csv("./data/epi_data_jakarta.csv")

# predictability

gaps_delhi <- read.csv("./data/Delhi-pre-obs-gap.csv")%>%
  left_join(read.csv("./data/indicator-Delhi.csv")) %>%
  mutate(hosp_risk = lag(order_by(week_no, diff(daily_hosp_week)/daily_hosp_week)),
         case_risk = lag(order_by(week_no, diff(daily_case_week)/daily_case_week)),
         case_pred = lag(order_by(week_no, diff(te_stm_case)/te_stm_case)),
         hosp_pred = lag(order_by(week_no, diff(te_stm_hosp)/te_stm_hosp)))

gaps_mum <- read.csv("./data/Mumbai-pre-obs-gap.csv")%>%
  left_join(read.csv("./data/indicator-Mumbai.csv")) %>%
  mutate(hosp_risk = lag(order_by(week_no, diff(daily_hosp_week)/daily_hosp_week)),
         case_risk = lag(order_by(week_no, diff(daily_case_week)/daily_case_week)),
         case_pred = lag(order_by(week_no, diff(te_stm_case)/te_stm_case)),
         hosp_pred = lag(order_by(week_no, diff(te_stm_hosp)/te_stm_hosp)))

gaps_bang <- read.csv("./data/Bangkok-pre-obs-gap.csv")%>%
  left_join(read.csv("./data/indicator-Bangkok.csv")) %>%
  mutate(hosp_risk = lag(order_by(week_no, diff(daily_hosp_week)/daily_hosp_week)),
         case_risk = lag(order_by(week_no, diff(daily_case_week)/daily_case_week)),
         case_pred = lag(order_by(week_no, diff(te_stm_case)/te_stm_case)),
         hosp_pred = lag(order_by(week_no, diff(te_stm_hosp)/te_stm_hosp)))

gaps_jak <- read.csv("./data/Jakarta-pre-obs-gap.csv")%>%
  left_join(read.csv("./data/indicator-Jakarta.csv")) %>%
  mutate(hosp_risk = lag(order_by(week_no, diff(daily_hosp_week)/daily_hosp_week)),
         case_risk = lag(order_by(week_no, diff(daily_case_week)/daily_case_week)),
         case_pred = lag(order_by(week_no, diff(te_stm_case)/te_stm_case)),
         hosp_pred = lag(order_by(week_no, diff(te_stm_hosp)/te_stm_hosp)))


# tweetCoord_Bangkok <- get_tweet_coords(bangkok)
# write.csv(tweetCoord_Bangkok, "tweetCoord_Bangkok.csv")
tweetCoord_Bang <-  read.csv("./data/tweetCoord_Bangkok.csv")

# tweetCoord_Delhi <- get_tweet_coords(delhi)
# write.csv(tweetCoord_Delhi, "tweetCoord_Delhi.csv")
tweetCoord_Del <-  read.csv("./data/tweetCoord_Delhi.csv")

# tweetCoord_Jak <- get_tweet_coords(jakarta)
# write.csv(tweetCoord_Jak, "tweetCoord_Jak.csv")
tweetCoord_Jak <-  read.csv("./data/tweetCoord_Jak.csv")

# tweetCoord_Mum <- get_tweet_coords(mumbai)
# write.csv(tweetCoord_Mum, "tweetCoord_Mum.csv")
tweetCoord_Mum <-  read.csv("./data/tweetCoord_Mum.csv")

# tweetBigrams_Bang <- get_bigrams(bangkok)
# write.csv(tweetBigrams_Bang, "tweetBigrams_Bang.csv")
tweetBigrams_Bang <-  read.csv("./data/tweetBigrams_Bang.csv")

# tweetBigrams_Del <- get_bigrams(delhi)
# write.csv(tweetBigrams_Del, "tweetBigrams_Del.csv")
tweetBigrams_Del <-  read.csv("./data/tweetBigrams_Del.csv")

# tweetBigrams_Jak <- get_bigrams(jakarta)
# write.csv(tweetBigrams_Jak, "tweetBigrams_Jak.csv")
tweetBigrams_Jak <-  read.csv("./data/tweetBigrams_Jak.csv")

# tweetBigrams_Mum <- get_bigrams(mumbai)
# write.csv(tweetBigrams_Mum, "tweetBigrams_Mum.csv")
tweetBigrams_Mum <-  read.csv("./data/tweetBigrams_Mum.csv")

# tweetEmo_Del <- get_emotions(delhi)
# write.csv(tweetEmo_Del, "tweetEmo_Del.csv")
tweetEmo_Del <-  read.csv("./data/tweetEmo_Del.csv")

# tweetEmo_Bang <- get_emotions(bangkok)
# write.csv(tweetEmo_Bang, "tweetEmo_Bang.csv")
tweetEmo_Bang <-  read.csv("./data/tweetEmo_Bang.csv")

# tweetEmo_Jak <- get_emotions(jakarta)
# write.csv(tweetEmo_Jak, "tweetEmo_Jak.csv")
tweetEmo_Jak <-  read.csv("./data/tweetEmo_Jak.csv")

# tweetEmo_Mum <- get_emotions(mumbai)
# write.csv(tweetEmo_Mum, "tweetEmo_Mum.csv")
tweetEmo_Mum <-  read.csv("./data/tweetEmo_Mum.csv")

assocVals_Del <- read.csv("./data/assocVals_Del.csv")

tweetEmotions_del <- read.csv("./data/tweetEmotions_Ndel.csv")

topTweets <- read.csv("./data/topTweets.csv")
