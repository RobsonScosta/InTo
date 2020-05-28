library(dplyr)
library(tidyr)
library(stringr)
# library(lubridate)
library(ggplot2)
# 
# 
# library(leaflet)
# library(plotly)
library(DT)

# library(rtweet)
# library(tidytext)

library(htmltools)

# Convert to an SPDF
# library(rgdal)
# library(KernSmooth)

# library(ggmap)
library(googleway)

ggmap_key <- "AIzaSyB055tAEERlsleH1Xf83-JqAa530V7roTk"
gradient <- c('white','yellow', 'orange', "red")

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
# krigingDel_case <- read.csv("./data/Delhi_kriging_data_case.csv") %>%
#   mutate(pred0 = ifelse(var1.pred - mean(var1.pred, na.rm = T) > 0,
#                         var1.pred - mean(var1.pred, na.rm = T),
#                         1),
#          predShow = var1.pred - mean(var1.pred, na.rm = T))
#  
krigingDel_hosp <- read.csv("./data/Delhi_kriging_data_hosp.csv") %>%
  mutate(pred0 = ifelse(var1.pred - mean(var1.pred, na.rm = T) > 0,
                        var1.pred - mean(var1.pred, na.rm = T),
                        1),
         predShow = var1.pred - mean(var1.pred, na.rm = T))

# krigingBan_case <- read.csv("./data/Bangkok_kriging_data_case.csv") %>%
#   mutate(pred0 = ifelse(var1.pred - mean(var1.pred, na.rm = T) > 0,
#                         var1.pred - mean(var1.pred, na.rm = T),
#                         0),
#          predShow = var1.pred - mean(var1.pred, na.rm = T))

krigingBan_hosp <- read.csv("./data/Bangkok_kriging_data_hosp.csv") %>%
  mutate(pred0 = ifelse(var1.pred - mean(var1.pred, na.rm = T) > 0,
                        var1.pred - mean(var1.pred, na.rm = T),
                        0),
         predShow = var1.pred - mean(var1.pred, na.rm = T))

# krigingJak_case <- read.csv("./data/Jakarta_kriging_data_case.csv") %>%
#   mutate(pred0 = ifelse(var1.pred - mean(var1.pred, na.rm = T) > 0,
#                         var1.pred - mean(var1.pred, na.rm = T),
#                         0),
#          predShow = var1.pred - mean(var1.pred, na.rm = T))

krigingJak_hosp <- read.csv("./data/Jakarta_kriging_data_hosp.csv") %>%
  mutate(pred0 = ifelse(var1.pred - mean(var1.pred, na.rm = T) > 0,
                        var1.pred - mean(var1.pred, na.rm = T),
                        0),
         predShow = var1.pred - mean(var1.pred, na.rm = T))

# krigingMum_case <- read.csv("./data/Mumbai_kriging_data_case.csv") %>%
#   mutate(pred0 = ifelse(var1.pred - mean(var1.pred, na.rm = T) > 0,
#                         var1.pred - mean(var1.pred, na.rm = T),
#                         0),
#          predShow = var1.pred - mean(var1.pred, na.rm = T))

krigingMum_hosp <- read.csv("./data/Mumbai_kriging_data_hosp.csv") %>%
  mutate(pred0 = ifelse(var1.pred - mean(var1.pred, na.rm = T) > 0,
                        var1.pred - mean(var1.pred, na.rm = T),
                        0),
         predShow = var1.pred - mean(var1.pred, na.rm = T))
# epi time series data
# epi_data_bang <- read.csv("./data/epi_data_bangkok.csv") 
# epi_data_delhi <- read.csv("./data/epi_data_new delhi.csv")
# epi_data_mumbai <- read.csv("./data/epi_data_mumbai.csv")
# epi_data_jakarta <- read.csv("./data/epi_data_jakarta.csv")

# predictability

gaps_delhi <- read.csv("./data/Delhi-pre-obs-gap.csv")%>%
  left_join(read.csv("./data/indicator-Delhi.csv")) %>%
  mutate(gap_case = lag(order_by = week_no, ),
         hosp_risk = lag(order_by(week_no, diff(daily_hosp_week)/daily_hosp_week)),
         case_risk = lag(order_by(week_no, diff(daily_case_week)/daily_case_week)),
         case_pred = round(te_stm_case,2),
         hosp_pred = round(te_stm_hosp,2)
         )

gaps_mum <- read.csv("./data/Mumbai-pre-obs-gap.csv")%>%
  left_join(read.csv("./data/indicator-Mumbai.csv")) %>%
  mutate(hosp_risk = lag(order_by(week_no, diff(daily_hosp_week)/daily_hosp_week)),
         case_risk = lag(order_by(week_no, diff(daily_case_week)/daily_case_week)),
         case_pred = round(te_stm_case,2),
         hosp_pred = round(te_stm_hosp,2))

gaps_bang <- read.csv("./data/Bangkok-pre-obs-gap.csv")%>%
  left_join(read.csv("./data/indicator-Bangkok.csv")) %>%
  mutate(hosp_risk = lag(order_by(week_no, diff(daily_hosp_week)/daily_hosp_week)),
         case_risk = lag(order_by(week_no, diff(daily_case_week)/daily_case_week)),
         case_pred = round(te_stm_case,2),
         hosp_pred = round(te_stm_hosp,2))

gaps_jak <- read.csv("./data/Jakarta-pre-obs-gap.csv")%>%
  left_join(read.csv("./data/indicator-Jakarta.csv")) %>%
  mutate(hosp_risk = lag(order_by(week_no, diff(daily_hosp_week)/daily_hosp_week)),
         case_risk = lag(order_by(week_no, diff(daily_case_week)/daily_case_week)),
         case_pred = round(te_stm_case,2),
         hosp_pred = round(te_stm_hosp,2))

# Top n grams
# tweetBigrams_Bang <-  read.csv("./data/tweetBigrams_bangkok.csv")
# tweetBigrams_Del <-  read.csv("./data/tweetBigrams_delhi.csv")
# tweetBigrams_Jak <-  read.csv("./data/tweetBigrams_jakarta.csv")
# tweetBigrams_Mum <-  read.csv("./data/tweetBigrams_mumbai.csv")

# Tweet Emotions
# tweetEmotions_del <- read.csv("./data/tweetEmo_delhi.csv")
# tweetEmotions_mum <- read.csv("./data/tweetEmo_mumbai.csv")
# tweetEmotions_ban <- read.csv("./data/tweetEmo_bangkok.csv")
# tweetEmotions_jak <- read.csv("./data/tweetEmo_jakarta.csv")

tweetCoord_Bang <-  read.csv("./data/Bangkok-daily-vol-stm.csv")
tweetCoord_Del <-  read.csv("./data/Delhi-daily-vol-stm.csv")
tweetCoord_Jak <-  read.csv("./data/Jakarta-daily-vol-stm.csv")
tweetCoord_Mum <-  read.csv("./data/Mumbai-daily-vol-stm.csv")

tweetSent_del <- read.csv("./data/tweetText_sentiment_delhi.csv") %>%
  mutate(Sent = round(Sent,2))
tweetSent_mum <- read.csv("./data/tweetText_sentiment_mumbai.csv")%>%
  mutate(Sent = round(Sent,2))
tweetSent_ban <- read.csv("./data/tweetText_sentiment_bangkok.csv")%>%
  mutate(Sent = round(Sent,2))
tweetSent_jak <- read.csv("./data/tweetText_sentiment_jakarta.csv")%>%
  mutate(Sent = round(Sent,2))

# Misinformation
## Emotions
# misinformation_tweet_emotions_mum <- read.csv("./data/misinformation_tweet_emotions_mumbai.csv")
# misinformation_tweet_emotions_jak <- read.csv("./data/misinformation_tweet_emotions_jakarta.csv")
# misinformation_tweet_emotions_del <- read.csv("./data/misinformation_tweet_emotions_delhi.csv")
# misinformation_tweet_emotions_ban <- read.csv("./data/misinformation_tweet_emotions_bangkok.csv")
## Bigrams
# misinformation_bigrams_mum <- read.csv("./data/misinformation_Bigrams_mumbai.csv")
# misinformation_bigrams_jak <- read.csv("./data/misinformation_Bigrams_jakarta.csv")
# misinformation_bigrams_del <- read.csv("./data/misinformation_Bigrams_delhi.csv")
# misinformation_bigrams_ban <- read.csv("./data/misinformation_Bigrams_bangkok.csv")

# Healthcare
## emotions
# healthcare_tweet_emotions_mum <- read.csv("./data/healthcare_tweet_emotions_mumbai.csv")
# healthcare_tweet_emotions_jak <- read.csv("./data/healthcare_tweet_emotions_jakarta.csv")
# healthcare_tweet_emotions_del <- read.csv("./data/healthcare_tweet_emotions_delhi.csv")
# healthcare_tweet_emotions_ban <- read.csv("./data/healthcare_tweet_emotions_bangkok.csv")
## bigrams
# healthcare_bigrams_mum <- read.csv("./data/healthcare_Bigrams_mumbai.csv")
# healthcare_bigrams_jak <- read.csv("./data/healthcare_Bigrams_jakarta.csv")
# healthcare_bigrams_del <- read.csv("./data/healthcare_Bigrams_delhi.csv")
# healthcare_bigrams_ban <- read.csv("./data/healthcare_Bigrams_bangkok.csv")