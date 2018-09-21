# Make sure to change this
setwd("/Users/Apple/Desktop/big data/youtube-personality")
library(tidyverse)
library(car)
library(olsrr)
library(stringr)
library(tidytext)

" 
Read in the CSV file
"
personality = as.tibble(read.csv("YouTube-Personality-Personality_impression_scores_train.csv", 
                        encoding = "UTF-8",
                        sep = " "))

gender = as.tibble(read.csv("YouTube-Personality-gender.csv",
                            encoding = "UTF-8",
                            sep = " "))

audiovisual = as.tibble(read.csv("YouTube-Personality-audiovisual_features.csv",
                                 encoding = "UTF-8",
                                 sep = " "))

transcripts = as.tibble(read.csv("transcripts.csv",
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

#tokenization
training_data$transcript <- as.vector(training_data$transcript)
tidy_data <- training_data %>% 
  unnest_tokens(word, transcript, to_lower = TRUE) %>% 
  anti_join(get_stopwords())

#count the total number of words for each person
total_words <- tidy_data %>% 
  group_by(vlogId) %>% 
  count() %>% 
  rename(total_words = n)

#count the percentage of words related to big 5
words_E = c("bar","drinks", "restaurant", "dancing", "restaurants", "grandfather", "miami", "countless", "drinking", "shots", "girls", "glorious", "pool", "crowd", "sang", "grilled")
words_A = c("wonderful", "together", "visiting", "morning", "spring", "walked", "beautiful", "staying", "felt", "share", "gray", "joy", "afternoon", "day", "moments", "hug", "glad")
words_C = c("completed", "adventure", "adventures", "enjoying", "hawaii", "it's", "deck")
words_N = c("awful", "though", "lazy", "worse", "depressing", "irony", "terrible", "stressful", "horrible", "sort", "annoying", "ashamed", "ban")
words_O = c("folk", "humans", "of", "poet", "art", "by", "universe", "poetry", "narrative", "culture", "century", "sexual", "films", "novel", "decades", "ink", "passage", "literature", "blues")
words_nE = c("other", "cats", "computer", "minor")
words_nA = c("porn", "cost", "fuck")
words_nC = c("stupid", "boring", "desperate", "saying", "utter", "it's", "extreme")
words_nN = c("road", "southern", "visited", "ground", "oldest", "invited", "completed")
words_nO = c("giveaway")

check_words <- function(wordlist){
  tidy_data %>% 
    filter(word %in% wordlist) %>% 
    group_by(vlogId) %>% 
    count() %>% 
    left_join(total_words) %>% 
    mutate(n/total_words) %>% 
    select(-n, -total_words)
}

E_words <- check_words(words_E) %>% 
  rename(words_Extraversion = 'n/total_words')

A_words <- check_words(words_A) %>% 
  rename(words_Agreeableness = 'n/total_words')

C_words <- check_words(words_C) %>% 
  rename(words_Conscientiousness = 'n/total_words')

N_words <- check_words(words_N) %>% 
  rename(words_Neuroticism = 'n/total_words')

O_words <- check_words(words_O) %>% 
  rename(words_Openness = 'n/total_words')

nE_words <- check_words(words_nE) %>% 
  rename(words_nExtraversion = 'n/total_words')

nA_words <- check_words(words_nA) %>% 
  rename(words_nAgreeableness = 'n/total_words')

nC_words <- check_words(words_nC) %>% 
  rename(words_nConscientiousness = 'n/total_words')

nN_words <- check_words(words_nN) %>% 
  rename(words_nNeuroticism = 'n/total_words')

nO_words <- check_words(words_nO) %>% 
  rename(words_nOpenness = 'n/total_words')

#putting them back to the training data
training_data <- training_data %>% 
  left_join(E_words) %>%
  left_join(A_words) %>%
  left_join(C_words) %>%
  left_join(N_words) %>%
  left_join(O_words) %>%
  left_join(nE_words) %>%
  left_join(nA_words) %>%
  left_join(nC_words) %>%
  left_join(nN_words) %>%
  left_join(nO_words) %>% 
  replace(., is.na(.), 0)

#sentiment analysis with nrc
nrc <- tidy_data %>% 
  inner_join(get_sentiments('nrc')) %>%
  count(`vlogId`, sentiment) %>%
  spread(sentiment, n, fill = 0)

training_data <- training_data %>% 
  left_join(nrc) %>%
  left_join(total_words) %>%
  mutate(anger = anger / total_words,
         anticipation = anticipation / total_words,
         disgust = disgust / total_words,
         fear = fear / total_words,
         joy = joy / total_words,
         negative = negative / total_words,
         positive = positive / total_words,
         sadness = sadness / total_words,
         surprise = surprise / total_words,
         trust = trust / total_words) %>%
  select(-total_words, -transcript)

#sentiment analysis with afinn
afinn = tidy_data %>% 
  inner_join(get_sentiments('afinn')) %>% 
  group_by(vlogId) %>% 
  summarise(sentiment = sum(score))

training_data <- training_data %>% 
  left_join(afinn) 

temp1 = 
  training_data %>%
  select(-vlogId, - gender,
         - Extr:-Open) %>%
  scale()

training_data = 
  training_data %>%
  select(vlogId, gender,
         Extr:Open) %>%
  cbind(temp1)

#Agreeableness baseline
training_data = training_data[,c(2, 4,8:53)]
A_baseline = lm(Agr ~ ., data = training_data)
summary(A_baseline)

par(mfrow = c(2,2))
plot(A_baseline)

#remove multicollinearity
vif(A_baseline)[vif(A_baseline) > 10]
modelA2 = update(A_baseline, ~. 
                -mean.conf.pitch 
                -mean.spec.entropy
                -mean.num.apeak
                -avg.voice.seg
                -negative
                -postive)
summary(modelA2)

#remove variables with p > 0.3
to_remove = ols_step_backward_p(modelA2)
paste(to_remove$removed, collapse = " - ")
modelA3 = update(modelA2, ~.
                 -mean.d.energy 
                 -mean.pitch 
                 -sd.loc.apeak 
                 -words_nExtraversion 
                 -words_nOpenness 
                 -words_nNeuroticism 
                 -mean.loc.apeak 
                 -words_Openness 
                 -avg.len.seg 
                 -sd.conf.pitch 
                 -sd.pitch 
                 -sd.spec.entropy 
                 -sd.num.apeak 
                 -hogv.median 
                 -sd.d.energy 
                 -mean.energy 
                 -anticipation 
                 -joy 
                 -words_Neuroticism 
                 -num.turns 
                 -hogv.entropy 
                 -hogv.cogR)
summary(modelA3)

#remove outliers
outlierTest(modelA3)
training_data = training_data[-322,]
modelA4 = update(modelA3)
summary(modelA4)
par(mfrow = c(2,2))
plot(modelA4)

#check linearity
new_data = 
  training_data %>%
  select(Agr, gender, mean.val.apeak, sd.val.apeak,
         sd.energy, avg.voiced.seg,
         time.speaking, voice.rate,hogv.cogC,
         words_Extraversion, words_Agreeableness,
         words_Conscientiousness, words_nAgreeableness,
         words_nConscientiousness, anger, disgust, fear,
         positive, sadness, surprise, trust, sentiment)
pairs(new_data[,c(1:5)])
pairs(new_data[,c(1,6:9)])
pairs(new_data[,c(1,10:13)])
pairs(new_data[,c(1,14:18)])
pairs(new_data[,c(1,19:21)])

#fitting a quadratic feature
modelA5 = update(modelA4, ~. + I(words_Agreeableness**2))
summary(modelA5)

#interaction
modelA6 = update(modelA5, ~. + gender:words_nConscientiousness
              + gender:positive
              + gender:time.speaking)
summary(modelA6)

