library(quanteda)
library(data.table)
library(ggplot2)

set.seed(1984)

files <- list.files(path="final/en_US/", pattern="*.txt", full.names=TRUE, 
                    recursive=FALSE) #get file list

SourceFileLength<-0                    
for (SourceFile in files){
  temp<-length(vroom::vroom_lines(SourceFile, altrep_opts = TRUE, 
                                            progress = FALSE))
  SourceFileLength<-SourceFileLength+temp
}

sample<-vector(mode="character")
#SourceFileName<-vector(mode="character")
n<-50000  #set sample size

for (SourceFile in files){
  con <- file(SourceFile, "rb", encoding="utf-8") 
  open(con)

  line <- readLines(con, n=1, encoding="UTF-8", skipNul = TRUE)
  while(length(line) > 0) {
    line<-readLines(con, 1, encoding="UTF-8", skipNul = TRUE)
    random<-rbinom(1,1,n/SourceFileLength) #define binomial random with n/N
    if(random==1){
      sample<-append(sample,line)
      #SourceFileName<-gsub(".txt","",str_split(SourceFile, "/", simplify = TRUE)[3])
      #With the row above you can make a vector to store the source file for each rows, supposed you defined        #SourceFileName as vector at the top of the code chunk. 
    }
  }
  close(con)
}

corp <- corpus(sample)

# the puncuations and numbers in the texts were removed as there is no need to predict punctations or numbers
master_Tokens <- tokens(
    x = tolower(corp),
    remove_punct = TRUE,
    remove_twitter = TRUE,
    remove_numbers = TRUE,
    remove_hyphens = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE
)

stemed_words <- tokens_wordstem(master_Tokens, language = "english")

con<- file("bad-words.txt", "rb")
profanity <- readLines(con)
close(con)

stemed_words<-tokens_remove(stemed_words, c(stopwords("english"), "will", "mr", "nine"))

head(profanity)
class(profanity)

head(stemed_words)

bi_gram <- tokens_ngrams(stemed_words, n = 2)
tri_gram <- tokens_ngrams(stemed_words, n = 3)

uni_DFM <- dfm(stemed_words)
bi_DFM <- dfm(bi_gram)
tri_DFM <- dfm(tri_gram)

uni_DFM <- dfm_trim(uni_DFM, 3)
bi_DFM <- dfm_trim(bi_DFM, 3)
tri_DFM <- dfm_trim(tri_DFM, 3)


# Create named vectors with counts of words 
sums_U <- colSums(uni_DFM)
sums_B <- colSums(bi_DFM)
sums_T <- colSums(tri_DFM)

# Create data tables with individual words as columns
uni_words <- data.table(word_1 = names(sums_U), count = sums_U)

bi_words <- data.table(
        word_1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
        count = sums_B)

tri_words <- data.table(
        word_1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
        word_2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
        word_3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
        count = sums_T)
        
graph.data <- uni_words[order(uni_words$count, decreasing = T), ]
graph.data <- graph.data[1:20, ]
graph.data$word_1 <- factor(graph.data$word_1, levels = graph.data$word_1)

ggplot(data=graph.data, aes(x=word_1, y=count)) + geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))        
        
        
graph.data <- bi_words[order(bi_words$count, decreasing = T), ]
graph.data <- graph.data[1:20, ]
graph.data$word <- paste(graph.data$word_1, graph.data$word_2)
graph.data$word <- factor(graph.data$word, levels = graph.data$word)

ggplot(data=graph.data, aes(x=word, y=count)) + geom_bar(stat="identity")




graph.data <- tri_words[order(tri_words$count, decreasing = T), ]
graph.data <- graph.data[1:20, ]
graph.data$word <- paste(graph.data$word_1, graph.data$word_2, graph.data$word_3)
graph.data$word <- factor(graph.data$word, levels = graph.data$word)

ggplot(data=graph.data, aes(x=word, y=count)) + geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
        
        
        
        
setkey(uni_words, word_1)
setkey(bi_words, word_1, word_2)
setkey(tri_words, word_1, word_2, word_3)







discount_value <- 0.75

######## Finding Bi-Gram Probability #################

# Finding number of bi-gram words
numOfBiGrams <- nrow(bi_words[by = .(word_1, word_2)])

# Dividing number of times word 2 occurs as second part of bigram, by total number of bigrams.  
# ( Finding probability for a word given the number of times it was second word of a bigram)
ckn <- bi_words[, .(Prob = ((.N) / numOfBiGrams)), by = word_2]
setkey(ckn, word_2)

# Assigning the probabilities as second word of bigram, to unigrams
uni_words[, Prob := ckn[word_1, Prob]]
uni_words <- uni_words[!is.na(uni_words$Prob)]

# Finding number of times word 1 occurred as word 1 of bi-grams
n1wi <- bi_words[, .(N = .N), by = word_1]
setkey(n1wi, word_1)

# Assigning total times word 1 occured to bigram cn1
bi_words[, Cn1 := uni_words[word_1, count]]

# Kneser Kney Algorithm
bi_words[, Prob := ((count - discount_value) / Cn1 + discount_value / Cn1 * 
                n1wi[word_1, N] * 
                uni_words[word_2, Prob])]

######## End of Finding Bi-Gram Probability #################




######## Finding Tri-Gram Probability #################

# Finding count of word1-word2 combination in bigram 
tri_words[, Cn2 := bi_words[.(word_1, word_2), .N]]

# Finding count of word1-word2 combination in trigram
n1w12 <- tri_words[, .(N = .N), by = .(word_1, word_2)]
setkey(n1w12, word_1, word_2)

# Kneser Kney Algorithm
#tri_words[, Prob := (count - discount_value) / Cn2 + discount_value / Cn2 * 
#              n1w12[.(word_1, word_2), N] *
#              bi_words[.(word_1, word_2), Prob]]
              
tri_words$Prob<-tri_words$count/tri_words$Cn2

######## End of Finding Tri-Gram Probability #################



# Finding the most frequently used 50 unigrmas
uni_words <- uni_words[order(-Prob)][1:50]




# function to return highly probable previous word given two successive words
triWords <- function(w1, w2, n = 5) {
    pwords <- tri_words[.(w1, w2)][order(-Prob)]
    if (any(is.na(pwords)))
        return(biWords(w2, n))
    if (nrow(pwords) > n)
        return(pwords[1:n, word_3])
    count <- nrow(pwords)
    bwords <- biWords(w2, n)[1:(n - count)]
    return(c(pwords[, word_3], bwords))
}





# function to return highly probable previous word given a word
biWords <- function(w1, n = 5) {
    pwords <- bi_words[w1][order(-Prob)]
    if (any(is.na(pwords)))
        return(uniWords(n))
    if (nrow(pwords) > n)
        return(pwords[1:n, word_2])
    count <- nrow(pwords)
    unWords <- uniWords(n)[1:(n - count)]
    return(c(pwords[, word_2], unWords))
}


# function to return random words from unigrams
uniWords <- function(n = 5) {  
    return(sample(uni_words[, word_1], size = n))
}



# The prediction app
getWords <- function(str){
    require(quanteda)
    tokens <- tokens(x = char_tolower(str))
    tokens <- char_wordstem(rev(rev(tokens[[1]])[1:2]), language = "english")
    
    words <- triWords(tokens[1], tokens[2], 5)
    chain_1 <- paste(tokens[1], tokens[2], words[1], sep = " ")

    print(words[1])
}


mondat<-tokens_wordstem(tokens("Very early observations on the Bills game: Offense still struggling but the"))
mondat<-tokens_remove(mondat, c(stopwords("english"), "will", "mr", "nine"))
mondat
getWords(mondat)