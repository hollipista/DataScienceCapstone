load(file='n_gram_prob.RData')
library(tibble)
library(dplyr)
library(tidytext)
library(tidyr)

#############

# prediction
predict_uni <- function() {
  #print('uni')
  max_prob = max(UG$prob)
  candidates=UG[UG$prob==max_prob,]
  return(sample(candidates$word1,1))
}

predict_bi <- function(w1) {
  #print('bi')
  candidates = BG[(BG$word1)==w1,c('word2','prob')]
  candidates = candidates[order(-candidates$prob),]
  candidates = candidates[!is.na(candidates$prob),]
  if (nrow(candidates) >=1){
    max_prob = max(candidates$prob)
    candidates=candidates[candidates$prob==max_prob,]
    return(sample(candidates$word2,1))
  } else 
  {return(predict_uni())}
}

predict_tri <- function(w1, w2) {
  #print('tri')
  candidates = TG[(TG$word1)==w1 & TG$word2 == w2, c('word3','prob')]
  candidates = candidates[order(-candidates$prob),]
  candidates = candidates[!is.na(candidates$prob),]
  if (nrow(candidates) >=1){
    max_prob = max(candidates$prob)
    candidates=candidates[candidates$prob==max_prob,]
    return(sample(candidates$word3,1))
  } else 
  {return(predict_bi(w2))}
  
}

getWords <- function(str){
  inputText <- tibble(text = str)
  LastBG <- mutate(inputText, text = gsub(x = text, 
                                          pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
                                          replacement = "", perl=TRUE)) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    slice_tail(n = 1) %>%
    separate(bigram, c("w1", "w2"), sep = " ")
  predict_tri(as.character(LastBG[1]),as.character(LastBG[2]))
}

getWords("what do you")