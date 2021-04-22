library(shiny)
load(file='n_gram_prob.RData')
library(tibble)
library(dplyr)
library(tidytext)
library(tidyr)
library(shinythemes)

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
    inputText <- tibble(text = tolower(str))
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

ui <- fluidPage(theme = shinytheme("slate"),
    
    titlePanel(
        h1("Data Science Capstone - Final Project", align="center")
    ),
    
    titlePanel(
        h2("Marco… Polo!", align="center")
    ),
    
    tags$br(),
    
    tags$h5("What is this about? Here you can try my text predictor Shiny App made for Data Science 
            Capstone Project on Coursera (JHU)."),
    tags$h5("In the last couple of weeks I've built a predictive model and a web
            application. This model tries to predict the next word of any given string - 
            that’s called text prediction. Let's try it! Enter any text in English 
            and see how the app tries to find out your next word!"),
    
    tags$br(),

    fluidRow(column(4, offset = 4, textInput("caption", "Let start to type your text here:", "TextInput")),
                column(4,style = "margin-top: 25px;",actionButton("reset", "Reset"))),

    tags$h5(strong("...and your extrapolated sentence is:"), align="center"),
    
    fluidRow(column(4, offset = 4,verbatimTextOutput("value"))),

    
    
    tags$h5("Let me say some words about my model and the progression I've done 
            from the first Milestone Report, that you can find"),
    tags$a(href="https://rpubs.com/hollipista/DataScienceCapstoneMilestoneReport", "HERE (click!)"),
    tags$h5("I was not satisfied with the accuracy of my first model. There were a lot of trigrams
            that were not occur in my corpus - due to limited sample size. So I decided to extend the
            corpus as large as I can. I reached the limits in terms of memory very soon but I thought 
            I have more time than memory: I tried to split the input texts to slices and process 
            them as a sequence and summarize after that. Whit this approach I was able to process 
            not just the full dataset of the course (thanks for that SwiftKey!) but also the 
            http://qwone.com/~jason/20Newsgroups/ data sets."),
    tags$h5("I took a long time but I hope worth it. The result was a lot of ten millions of bigrams 
            and trigrams, so I needed to cut the data and get rid of the combinations with very 
            low frequency. I had to do it outside of R unfortunatelly but I can tell you that 
            I kept about one million trigrams and one million bigrams (of course the most frequented 
            ones.) This is the so called pruning in order to decrease the size of model - of course 
            sacrificed some accuracy. 
            The other direction would have been to build 4-grams that can leed to a more 
            'clever' model because my actual model predict stopwords (eg. articles) as well, 
            which means in many cases the trigrams convey just unigrams' information to put it 
            very simplified. For this project, my decision was to use just trigrams."),
    tags$h5("I've applied Kneser-Ney smoothing on the data which is a bit problematic due to
            I've pruned the n-gram datasets independently that caused a smaller inconsistency 
            but it seemed to me that it has small significance regarding the model accuracy. My model 
            is a back off model: it always try to predict based on all available input (in our 
            case it means the last two tokens/words) but if there are no enought evidence, it 
            cuts back a word (so predict based on the last word). "),
    tags$br(),
    tags$h5("Please do not forget that it was just a play around of text prediction ;-) As
            you've seen my model is very simple and knows nothing about syntax or semantic,
            it's a simple model based on the occurance and order of words."),
    tags$br(),
    tags$h5("I would like to say thanks for Thiloshon Nagarajah, John O. Bonsak, Qiong Wu and Igor Hut
            for the their very informative and useful publications."),
    tags$br(),
    tags$a(href="https://github.com/hollipista/DataScienceCapstone", "Here you can find the repo of the project")
    
    )



server <- function(input, output, session) {
    
    
    pred <- reactive({getWords(input$caption)})
    
    output$value <- renderText({ paste(input$caption, pred()) })
    
    observe({
        input$reset
        updateTextInput(session, "caption", value = "")
    })
    
}



shinyApp(ui, server)


