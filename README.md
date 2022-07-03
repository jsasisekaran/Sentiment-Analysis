# Sentiment-Analysis

I used this code to conduct sentiment analysis. I used R and the nrc database for sentiment analysis on twitter data in English on the topic of "Covid-19").

#the tweets from this file (see code in the link below) were obtained, clearned, and hydrated for tweets posted on a given day in English using a code on Githhub. This code requires API authentication keys via Twitter. 
#covid19_twitter/COVID_19_dataset_Tutorial.ipynb at master · thepanacealab/covid19_twitter

a <- read.csv("hydrated_tweets_eng.csv", header = T)
head(a)

install.packages("tm")
library(tm)
corpus <- iconv(a$text)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

#This creates a word cloud of frequent words in the twitter data.

library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

install.packages("wordcloud2")
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

#This code uses nrc database to conduct sentiment analysis. 
#https://nrc.canada.ca/en/research-development/products-services/technical-advisory-services/sentiment-emotion-lexicons

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(SentimentAnalysis)

a <- read.csv("hydrated_tweets_eng.csv", header = T)
view(a)
tweets <- iconv(a$text)

s <- get_nrc_sentiment(tweets)
View(s)
a <- cbind(a,s)
View(a)
write.csv(a,'sentiment analysis.csv')

--The output file has additional new columns on emotions (anger, digust, fear, sadness, anticipation, joy, surprise, trust) and sentiments (negative and positive). The outcome variables can be used to create a Machine Learning model for predicting sentiments.
