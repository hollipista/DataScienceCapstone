#1.1 Obtaining the data

#download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
#              "Coursera-SwiftKey.zip","curl")
#unzip("Coursera-SwiftKey.zip", files = NULL, list = FALSE, overwrite = TRUE,
#      junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE)


#1.2 Read a sample subset

#install.packages("vroom")
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
n<-10000  #set sample size

for (SourceFile in files){
  con <- file(SourceFile, "rb", encoding="utf-8") #binomial reading mode due to linebreak warning
  open(con)

  line <- readLines(con, n=1, encoding="UTF-8")
  while(length(line) > 0) {
    line<-readLines(con, 1, encoding="UTF-8")
    random<-rbinom(1,1,n/SourceFileLength) #define binomial random with n/N
    if(random==1){
      sample<-append(sample,line)
      SourceFileName<-gsub(".txt","",str_split(SourceFile, "/", simplify = TRUE)[3])
    }
  }
  close(con)
}

head(sample)

#1.3 Quiz 1: Getting Started
#1. The en_US.blogs.txt file is how many megabytes?
file.info('final/en_US/en_US.blogs.txt')$size/1024^2
#2. The en_US.twitter.txt has how many lines of text?
length(vroom::vroom_lines('final/en_US/en_US.twitter.txt', altrep_opts = TRUE, 
                   progress = FALSE))
#3. What is the length of the longest line seen in any of the three en_US 
#data sets? 
files <- list.files(path="final/en_US/", pattern="*.txt", full.names=TRUE, 
                    recursive=FALSE) #get file list
for (SourceFile in files){
  con <- file(SourceFile, "rb") #binomial reading mode due to linebreak warning
  open(con)
  
  line <- readLines(con, n=1)
  LongestLine=nchar(line)
  while(length(line) > 0) {
    line<-readLines(con, 1)
    if(nchar(line)>LongestLine){
      LongestLine<-nchar(line)
      LongestLineFile<-SourceFile
    }
  }
  close(con)
}
LongestLine
LongestLineFile
#4. In the en_US twitter data set, if you divide the number of lines where the 
#word "love" (all lowercase) occurs by the number of lines the word "hate" (all 
#lowercase) occurs, about what do you get?
library("stringr")
con <- file('final/en_US/en_US.twitter.txt', "rb") 
open(con)
line <- readLines(con, n=1)
love<-0
hate<-0
while(length(line) > 0) {
  line<-readLines(con, 1)
  if(str_detect(line, "love")){love<-love+1}
  if(str_detect(line, "hate")){hate<-hate+1}
}
close(con)
love/hate
#5. The one tweet in the en_US twitter data set that matches the word 
#"biostats" says what?
con <- file('final/en_US/en_US.twitter.txt', "rb") 
open(con)
line <- readLines(con, n=1)
while(length(line) > 0) {
  line<-readLines(con, 1)
  if(str_detect(line, "biostats")){print(line)}
}
close(con)
#6. How many tweets have the exact characters "A computer once beat me at 
#chess, but it was no match for me at kickboxing". (I.e. the line matches those 
#characters exactly.)
temp<-0
con <- file('final/en_US/en_US.twitter.txt', "rb") 
open(con)
line <- readLines(con, n=1)
while(length(line) > 0) {
  line<-readLines(con, 1)
  if(str_detect(line, "^A computer once beat me at chess, but it was no match for me at kickboxing$")){
    temp<-temp+1
  }
}
close(con)
print(temp)


#1.4 Tokenization (play around tidytext)

library(dplyr)
library(tidyr)
#install.packages('tidytext')
library(tidytext)
library(ggplot2)
#install.packages("tm")
library(tm)
StopWords<-tibble(stop_words)

text_df <- tibble(line = 1:length(sample), SourceFile = SourceFileName, 
                  text = sample)

#remove numbers, punctuation, brackets
tidy_text <- mutate(text_df, text = gsub(x = text, 
                    pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
                    replacement = "", perl=TRUE)) %>%
  unnest_tokens(input = text, output = word) %>%
  anti_join(StopWords) %>%
  mutate(word=gsub("\\'","",word))

tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#profanity filter
url<-"https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
if(!file.exists("bad-words.txt")) {
  download.file(url = url, destfile = "./bad-words.txt")
}
con<- file("bad-words.txt", "rb")
profanity <- readLines(con)
close(con)

profanity <- tibble(profanity) %>%
  rename(word = profanity)
tidy_text<-anti_join(tidy_text,profanity)

install.packages("wordcloud2")
install.packages("wordcloud")
library(wordcloud)
library(wordcloud2)

words <- tidy_text %>% count(word, sort=TRUE)
words

wordcloud(words = words$word, freq = words$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data=words, size=1.6, color='random-dark')
tidy_text

#2.1 Exploratory Data Analysis
#bigram
tidy_text <- mutate(text_df, text = gsub(x = text, 
                                         pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
                                         replacement = "", perl=TRUE)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
tidy_text

tidy_text %>%
  count(bigram, sort = TRUE)

bigrams_separated <- tidy_text %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

text_df

install.packages("igraph")
install.packages("ggraph")
library(igraph)
library(ggraph)
bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

bigram_graph
library(ggraph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#trigram
tidy_text <- mutate(text_df, text = gsub(x = text, 
                                         pattern = "[0-9]+|(?!')[[:punct:]]|\\(.*\\)", 
                                         replacement = "", perl=TRUE)) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
tidy_text

