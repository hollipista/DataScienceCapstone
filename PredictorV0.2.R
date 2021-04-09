library("vroom")
library("stringr")

files <- list.files(path="final/en_US/", pattern="*.txt", full.names=TRUE, 
                    recursive=FALSE) #get file list

#determine the length of input files (summarized)
set.seed(1984)

SourceFileLength<-0
for (SourceFile in files){
  temp<-length(vroom::vroom_lines(SourceFile, altrep_opts = TRUE, 
                                  progress = FALSE))
  SourceFileLength<-SourceFileLength+temp
}

#get an n size sample
sample<-vector(mode="character")
n<-500000  #set sample size

for (SourceFile in files){
  con <- file(SourceFile, "rb", encoding="utf-8") #binomial reading mode due to linebreak warning
  open(con)
  
  line <- readLines(con, n=1, encoding="UTF-8")
  while(length(line) > 0) {
    line<-readLines(con, 1, encoding="UTF-8")
    random<-rbinom(1,1,n/SourceFileLength) #define binomial random with n/N
    if(random==1){
      sample<-append(sample,line)
      #SourceFileName<-gsub(".txt","",str_split(SourceFile, "/", simplify = TRUE)[3])
    }
  }
  close(con)
}




####
con <- file("final/en_US/en_US.blogs.txt", "rb", encoding="utf-8")
blogs <- readLines(con)
close(con)
con <- file("final/en_US/en_US.news.txt", "rb", encoding="utf-8")
news <- readLines(con)
close(con)
con <- file("final/en_US/en_US.twitter.txt", "rb", encoding="utf-8")
twitter <- readLines(con)
close(con)

sampleHolderTwitter <- sample(length(twitter), length(twitter) * 0.1)
sampleHolderBlog <- sample(length(blogs), length(blogs) * 0.1)
sampleHolderNews <- sample(length(news), length(news) * 0.1)

US_Twitter_Sample <- twitter[sampleHolderTwitter]
US_Blogs_Sample <- blogs[sampleHolderBlog]
US_News_Sample <- news[sampleHolderNews]

sample<-c(US_Twitter_Sample,US_Blogs_Sample,US_News_Sample)

rm(blogs,news,twitter,US_Twitter_Sample,US_Blogs_Sample,US_News_Sample)
####







#1.4 Tokenization (play around tidytext)

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(tm)
StopWords<-tibble(stop_words)

#unigram
text_df <- tibble(text = sample)
rm(blogs,news,twitter,sample)

UG <- mutate(text_df, text = gsub(x = text, 
  pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
  replacement = "", perl=TRUE)) %>%
  unnest_tokens(input = text, output = word) %>%
  #anti_join(StopWords) %>%
  mutate(word1=gsub("\\'","",word))

con<- file("bad-words.txt", "rb")
profanity <- readLines(con)
close(con)
profanity <- tibble(profanity)

#UG <- UG %>%
#  filter(!word1 %in% profanity$profanity)

UG <- UG %>% 
  count(word1, sort = TRUE)

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


##########

UG <- UG %>% mutate(P = n/nrow(UG))
BG <- left_join(BG,select(UG, word1, n),by=c("word1"="word1"),suffix = c("_BG", "_UG")) %>% 
  mutate(P = n_BG/n_UG)
TG <- left_join(TG,select(BG, word1, word2, n_BG),by=c("word1"="word1","word2"="word2"),
  suffix = c("_TG", "")) %>% 
  mutate(P = n/n_BG)

temp <- TG %>% filter(word1 == "happy") %>% filter(word2 == "mothers")
temp

##########

mondat <- tibble(text = "If this isn't the cutest thing you've ever seen, then you must be")
mondat <- mutate(mondat, text = gsub(x = text, 
  pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
  replacement = "", perl=TRUE)) %>%
  unnest_tokens(input = text, output = word) %>%
  #anti_join(StopWords) %>%
  mutate(word1=gsub("\\'","",word))
mondat


temp <- TG %>% filter(word1 == "but") %>% filter(word2 == "the")
print(temp)


temp <- BG %>% filter(word1 == "follow")
temp


