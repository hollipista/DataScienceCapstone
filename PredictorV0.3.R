set.seed(1984)
library(tibble)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(sqldf)
library(data.table)


#StopWords<-tibble(stop_words)

con<- file("bad-words.txt", "rb")
profanity <- readLines(con)
close(con)
profanity <- tibble(profanity)

#Split text input to slices
files <- list.files(path="final/en_US/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
lines<-vector(mode="character")
iter<-1
for (SourceFile in files){
  ConIn <- file(SourceFile, "rb", encoding="utf-8")
  open(ConIn)
  lines <- readLines(ConIn, n=50000, encoding="UTF-8", skipNul = TRUE)
  while(length(lines) > 0) {
    ConOut<-file(paste(paste("temp/temp",iter,sep=""),".txt",sep=""))
    writeLines(lines, ConOut, useBytes=T)
    iter<-iter+1
    close(ConOut)
    lines <- readLines(ConIn, n=50000, encoding="UTF-8", skipNul = TRUE)
  }
  close(ConIn)
}

#loop over slices
files <- list.files(path="temp/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
rm(lines)

iter<-1
for (SourceFile in files){
ConIn <- file(SourceFile, "rb", encoding="utf-8")
open(ConIn)
sample <- readLines(ConIn, encoding="UTF-8", skipNul = TRUE)
sample<-sample[which(sample!=""&str_length(sample)>1)]

#unigram
text_df <- tibble(text = sample)

UG <- mutate(text_df, text = gsub(x = text, 
  pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
  replacement = "", perl=TRUE)) %>%
  unnest_tokens(input = text, output = word1) #%>%
  #anti_join(StopWords) %>%
  #mutate(word1=gsub("\\'","",word))

#UG <- UG %>%
#  filter(!word1 %in% profanity$profanity)

UG <- UG %>% 
  count(word1, sort = TRUE)

#write.csv(UG,(paste(paste("temp/UG",iter,sep=""),".csv",sep="")), row.names = FALSE,fileEncoding = "UTF-8")
readr::write_csv(UG,(paste(paste("temp/UG",iter,sep=""),".csv",sep="")))

#bigram
BG <- mutate(text_df, text = gsub(x = text, 
  pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
  replacement = "", perl=TRUE)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

BG <- BG %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#BG <- BG %>%
#  filter(!word1 %in% c(stop_words$word,profanity$profanity)) %>%
#  filter(!word2 %in% c(stop_words$word,profanity$profanity))

BG <- BG %>% 
  count(word1, word2, sort = TRUE)

#write.csv(BG,(paste(paste("temp/BG",iter,sep=""),".csv",sep="")), row.names = FALSE)
readr::write_csv(BG,(paste(paste("temp/BG",iter,sep=""),".csv",sep="")))

#trigram
TG <- mutate(text_df, text = gsub(x = text, 
  pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
  replacement = "", perl=TRUE)) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

TG <- TG %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

#TG <- TG %>%
#  filter(!word1 %in% c(stop_words$word,profanity$profanity)) %>%
#  filter(!word2 %in% c(stop_words$word,profanity$profanity)) %>%
#  filter(!word3 %in% c(stop_words$word,profanity$profanity))

TG <- TG %>% 
  count(word1, word2, word3, sort = TRUE)

#write.csv(TG,(paste(paste("temp/TG",iter,sep=""),".csv",sep="")), row.names = FALSE,fileEncoding = "UTF-8")
readr::write_csv(TG,(paste(paste("temp/TG",iter,sep=""),".csv",sep="")))

##########
iter<-iter+1
close(ConIn)
unlink(SourceFile, force = TRUE)

}


###################summarize samples

memory.limit(size=25000)

files <- list.files(path="temp/", pattern="*.csv", full.names=TRUE, recursive=FALSE)
UGFiles<-files[grepl("UG",files)]
BGFiles<-files[grepl("BG",files)]
TGFiles<-files[grepl("TG",files)]

UG<-0
for (SourceFile in UGFiles){
  UG<-rbind(UG,read.csv(file = SourceFile, encoding = "UTF-8"))
  UG<-UG %>% group_by(word1) %>% summarise(n = sum(n))
}
readr::write_csv(UG,"temp/UG.csv")
rm(UG)

BG<-0
for (SourceFile in BGFiles){
  BG<-rbind(BG,read.csv(file = SourceFile, encoding = "UTF-8"))
  BG<-BG %>% group_by(word1,word2) %>% summarise(n = sum(n))
}
readr::write_csv(BG,"temp/BG.csv")
rm(BG)

TG<-0
counter<-1
for (SourceFile in TGFiles){
  TG<-rbind(TG,read.csv(file = SourceFile, encoding = "UTF-8"))
  TG<-TG %>% group_by(word1,word2,word3) %>% summarise(n = sum(n))
  print(counter)
  counter<-counter+1
}
readr::write_csv(TG,"temp/TG.csv")
rm(TG)

### cleaning and pruning

TG <- read.csv.sql("temp/TG.csv", 
                   sql = "select * from file where (word1 LIKE 'a%' OR
                                                    word1 LIKE 'b%' OR
                                                    word1 LIKE 'c%' OR
                                                    word1 LIKE 'd%' OR
                                                    word1 LIKE 'e%' OR
                                                    word1 LIKE 'f%' OR
                                                    word1 LIKE 'g%' OR
                                                    word1 LIKE 'h%' OR
                                                    word1 LIKE 'i%' OR
                                                    word1 LIKE 'j%' OR
                                                    word1 LIKE 'k%' OR
                                                    word1 LIKE 'l%' OR
                                                    word1 LIKE 'm%' OR
                                                    word1 LIKE 'n%' OR
                                                    word1 LIKE 'o%' OR
                                                    word1 LIKE 'p%' OR
                                                    word1 LIKE 'q%' OR
                                                    word1 LIKE 'r%' OR
                                                    word1 LIKE 's%' OR
                                                    word1 LIKE 't%' OR
                                                    word1 LIKE 'u%' OR
                                                    word1 LIKE 'v%' OR
                                                    word1 LIKE 'w%' OR
                                                    word1 LIKE 'x%' OR
                                                    word1 LIKE 'y%' OR
                                                    word1 LIKE 'z%') AND
                                                    n > '1' " )
                                                    



?read.csv.sql

### I've made some cleaning and pruning out of R
### filtered trigrams with less than 14 freq and bigrams with lt. 7
### it resulted cca. 1 million grams per type
### I should prune the same w1w2 combinations with low P

### Kneser-Kney Smoothing

UG<-read.csv("temp/UG_filtered.csv")
BG<-read.csv("temp/BG_filtered.csv",sep=";")
TG<-read.csv("temp/TG_filtered.csv",sep=";")

colnames(BG)[1] <- "word1"
colnames(TG)[1] <- "word1"

# N-Gram model probablity with Kneser Kney Smoothing
# http://computational-linguistics-class.org/slides/04-n-gram-language-models.pdf
# page 66

discount_value <- 0.75

# uni-gram
UG$prob = UG$n / sum(UG$n)

# bi-gram
BG_w1_count = aggregate(BG[,'word1'],by=list(BG[,'word1']),length)
names(BG_w1_count)=c('word1','n')

BG = merge(BG,UG[,c('word1','n')],by.x='word1',by.y='word1',all.x=TRUE)
names(BG)=c("word1","word2",'n','n_w1_ug')

BG = merge(BG,BG_w1_count,by='word1',all.x=TRUE)
names(BG)=c('word1','word2','n','n_w1_ug','n_w1_bg')

BG = merge(BG,UG[,c('word1','prob')],by.x='word2',by.y='word1',all.x=TRUE)
names(BG) = c('word2','word1','n','n_w1_ug','n_w1_bg','n_w2_ug_prob')

BG$prob = (BG$n - discount_value)/BG$n_w1_ug + discount_value/BG$n_w1_ug*BG$n_w1_bg*BG$n_w2_ug_prob
BG = BG[order(BG$word1,BG$word2),]

# tri-gram

TG_w12_count = aggregate(TG[,c('word1')],by=list(TG$word1,TG$word2),length)
names(TG_w12_count)=c('word1','word2','n')

TG = merge(TG,BG[,c('word1','word2','n')],by=c('word1','word2'),all.x=TRUE,all.y=FALSE)
names(TG)=c('word1','word2','word3','n','n_w12_bg')

TG = merge(TG,TG_w12_count,by=c('word1','word2'),all.x=TRUE)
names(TG)=c('word1','word2','word3','n','n_w12_bg','n_w12_tg')

TG = merge(TG,BG[,c('word1','word2','prob')],by.x=c('word2','word3'),by.y=c('word1','word2'),all.x=TRUE)
names(TG)=c('word2','word3','word1','n','n_w12_bg','n_w12_tg','nw23_bg_prob')

# due to manual cut of low freq bgs and tgs, there are bigger probabilities than 1. 
TG$n_w12_bg <- ifelse(TG$n_w12_bg < TG$n, TG$n, TG$n_w12_bg)

TG$prob = (TG$n - discount_value)/TG$n_w12_bg + discount_value/TG$n_w12_bg*TG$n_w12_tg*TG$nw23_bg_prob
TG$prob <- ifelse(TG$prob > 1, 1, TG$prob)

### pruning
TG <- TG %>% 
  group_by(word1, word2) %>%
  select(-c(n,n_w12_bg,n_w12_tg,nw23_bg_prob)) %>%
  filter(prob == max(prob,na.rm=TRUE)) 

BG <- BG %>% 
  group_by(word1) %>%
  select(-c(n,n_w1_ug,n_w1_bg,n_w2_ug_prob)) %>%
  filter(prob == max(prob),na.rm=TRUE)

UG <- UG %>% 
  select(-n) %>%
  top_n(n = 50, wt=prob)

save(UG,BG,TG,file='n_gram_prob.RData')

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
  if (length(grep("\\s[a-zA-Z]", str))>0){
    LastBG <- mutate(inputText, text = gsub(x = text, 
      pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)|\\s$", 
      replacement = "", perl=TRUE)) %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      slice_tail(n = 1) %>%
      separate(bigram, c("w1", "w2"), sep = " ")
      predict_tri(as.character(LastBG[1]),as.character(LastBG[2]))
  }
  else{
    LastBG <- mutate(inputText, text = gsub(x = text, 
      pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)|\\s$", 
      replacement = "", perl=TRUE))
    predict_bi(as.character(LastBG[1]))
  }
}

getWords("")
predict_bi("who")