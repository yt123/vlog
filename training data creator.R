# Make sure to change this
setwd("/Users/Apple/Desktop/big data")
library(tidyverse)
library(tidytext)

" 
Read in the CSV file
"
personality = as.tibble(read.csv("youtube-personality/YouTube-Personality-Personality_impression_scores_train.csv", 
                        encoding = "UTF-8",
                        sep = " "))

gender = as.tibble(read.csv("youtube-personality/YouTube-Personality-gender.csv",
                            encoding = "UTF-8",
                            sep = " "))

audiovisual = as.tibble(read.csv("youtube-personality/YouTube-Personality-audiovisual_features.csv",
                                 encoding = "UTF-8",
                                 sep = " "))

transcripts = as.tibble(read.csv("youtube-personality/transcripts.csv",
                                 encoding = "UTF-8",
                                 sep = ","))

transcripts$index = as.character(transcripts$index)

transcripts =
  transcripts %>%
  rename(vlogId = index,
         transcript = X0) %>%
  mutate(vlogId = substr(vlogId, 1, nchar(vlogId)-4))

personality$vlogId = as.character(personality$vlogId)
gender$vlogId = as.character(gender$vlogId)
audiovisual$vlogId = as.character(audiovisual$vlogId)

training_data = 
  personality %>%
  left_join(gender, on = c("vlogID" = "vlogID")) %>%
  left_join(audiovisual, on = c("vlogID" = "vlogID")) %>%
  left_join(transcripts, on = c("clogID" = "vlogID"))

training_data = as.data.frame(training_data)
training_data$transcript = as.vector(training_data$transcript)

tidy_data = training_data %>% 
  unnest_tokens(word, transcript)

data(stop_words)

tidy_data <- tidy_data %>%
  anti_join(stop_words)

tidy_data %>%
  count(word, sort = TRUE) 

nrc <- get_sentiments('nrc') 
text_labeled <- inner_join(tidy_data, nrc) 
sentiment_scores <- 
  text_labeled %>%
  count(`vlogId`, sentiment)

sentiment <- sentiment_scores %>%
  spread(sentiment, n, fill = 0)

training_data <- training_data %>% 
  left_join(sentiment,on = c("vlogID" = "vlogID") )

