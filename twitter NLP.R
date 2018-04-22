#Import our long list of libraries
library(wordcloud)
library(tm)
library(ggplot2)
library(SnowballC)
library(data.table)
library(stringr)
library(qdap)
library(tibble)
library(RWeka)
library(lubridate)
library(lexicon)
library(tidytext)
library(gutenbergr)
library(dplyr)
library(radarchart)


celticsTweets <- read.csv(file="/home/phil/Desktop/Stuff For School/Spring/DATA 902/listoftweets.csv", header=TRUE, sep=",")

#Now that we've loaded our csv, let's start cleaning our data

#Get rid of any non-alphanumeric character
celticsTweets$text <- iconv(celticsTweets$text, from = "UTF-8", to = "ASCII", sub = "")

#Making a corpus of a vector source 
tweetCorpus <- VCorpus(VectorSource(celticsTweets$text))
#Cleaning corpus
corpusCleaner <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_number))
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  # available stopwords
  # stopwords::stopwords()
  
  # Add any custom stop words here
  custom_stop_words <- c("rt", "rd", "retweet", "rodgers")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  # cleaned_corpus <- tm_map(cleaned_corpus, stemDocument,language = "english")
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}


tweetCorpusClean <- corpusCleaner(tweetCorpus)

#Now that our data is clean, let's put it in a TDM
tdmTweets <- TermDocumentMatrix(tweetCorpusClean)
tdmTweets_m <- as.matrix(tdmTweets)

#Establish word frequency
frequency <- rowSums(tdmTweets_m)


#Now for our lovely unigram
wordFrequency <- data.frame(term = names(frequency), num = frequency)

#Make a wordcloud
wordcloud(wordFrequency$term, wordFrequency$num,min.freq=5,max.words=2500,colors=brewer.pal(10,"BrBG"))

#Now let's repeat the problem for two word segments
token2 <- function(x)
  NGramTokenizer(x,Weka_control(min=2,max=2)) 

bigramTDM <- TermDocumentMatrix(tweetCorpusClean,control = list(tokenize=token2))
bigramTDM_m <- as.matrix(bigramTDM)

#Set the two word frequency
frequency2 <- rowSums(bigramTDM_m)
#Sort the frequency in descending order
frequency2 <- sort(frequency2,dec=TRUE)

#Create the bigram
wordFrequency2 <- data.frame(term = names(frequency2), num = frequency2)
#Create a wordcloud for the values in the bigram
wordcloud(wordFrequency2$term, wordFrequency2$num,min.freq=5,max.words=2500,colors=brewer.pal(8, "BrBG"))

#Now the three word trigram

token3 <- function(x)
  NGramTokenizer(x,Weka_control(min=3,max=3)) 

trigramTDM <- TermDocumentMatrix(tweetCorpusClean,control = list(tokenize=token3))
trigramTDM_m <- as.matrix(trigramTDM)

#Frequency
frequency3 <- rowSums(trigramTDM_m)
frequency3 <- sort(frequency3,dec=TRUE)

#Create Trigram
wordFrequency3 <- data.frame(term = names(frequency3), num = frequency3)
#Create a wordcloud for the values in trigram
wordcloud(wordFrequency3$term, wordFrequency3$num,min.freq=5,max.words=2500,colors=brewer.pal(8, "BrBG"))


#Now to move on to Term Frequencies and Inverse Document Frequencies
#Gonna comment on these less, process is very similar
# Unigram

tfidfTDM <- TermDocumentMatrix(tweetCorpusClean,control=list(weighting=weightTfIdf))
tfidfTDM_m <- as.matrix(tfidfTDM)
frequency <- rowSums(tfidfTDM_m)
frequency <- sort(frequency,dec=TRUE)
wordFrequency <- data.frame(term = names(frequency), num = frequency)
wordcloud(wordFrequency$term, wordFrequency$num,min.freq=5,max.words=2500,colors=brewer.pal(8, "Paired"))


#Bigram

tfidfTDM2 <- TermDocumentMatrix(tweetCorpusClean,control = list(tokenize=token2, weighting=weightTfIdf))
tfidfTDM2_m <- as.matrix(tfidfTDM2)
frequency2 <- rowSums(tfidfTDM2_m)
frequency2 <- sort(frequency2,dec=TRUE)
wordFrequency2 <- data.frame(term = names(frequency2), num = frequency2)
wordcloud(wordFrequency2$term, wordFrequency2$num,min.freq=5,max.words=2500,colors=brewer.pal(8, "Paired"))


#Trigram

tfidfTDM3 <- TermDocumentMatrix(tweetCorpusClean,control = list(tokenize=token3, weighting=weightTfIdf))
tfidfTDM3_m <- as.matrix(tfidfTDM3)
frequency3 <- rowSums(tfidfTDM3_m)
frequency3 <- sort(frequency3,dec=TRUE)
wordFrequency3 <- data.frame(term = names(frequency3), num = frequency3)
wordcloud(wordFrequency3$term, wordFrequency3$num,min.freq=5,max.words=2500,colors=brewer.pal(8, "Paired"))

tidyText <- tidy(TermDocumentMatrix(tweetCorpusClean))
lexiconBing <- get_sentiments("bing")
textBing <- inner_join(tidyText, lexiconBing, by = c("term" = "word"))
textBing$sentiment_n <- ifelse(textBing$sentiment=="negative", -1, 1)
textBing$sentiment_score <- textBing$count*textBing$sentiment_n
aggdata <- aggregate(textBing$sentiment_score, list(index = textBing$document), sum)
sapply(aggdata,typeof)
aggdata$index <- as.numeric(aggdata$index)

sents = levels(factor(textBing$sentiment))
labels <- lapply(sents, function(x) paste(x,format(round((length((textBing[textBing$sentiment ==x,])$term)/length(textBing$sentiment)*100),2),nsmall=2),"%"))

senlen = length(sents)
emo.docs = rep("", senlen)
for (i in 1:senlen)
{
  tmp = textBing[textBing$sentiment == sents[i],]$term
  
  emo.docs[i] = paste(tmp,collapse=" ")
}
corp = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corp)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# commonality/comparison cloud prep

celticsTweets$text <- iconv(celticsTweets$text, from = "UTF-8", to = "ASCII", sub = "")
celticsTweets$text <- removePunctuation(celticsTweets$text)

(sentiment_text<- polarity(celticsTweets$text))
celticsTweets$sent <- sentiment_text$all$polarity
text_counts <- counts(sentiment_text)
pos1 <- celticsTweets[celticsTweets[ , 17] >0.0, ]
neg1 <- celticsTweets[celticsTweets[ , 17] <0.0, ]
pos1$text
pos2<- paste(unlist(pos1$text), collapse =" ")
neg2<- paste(unlist(neg1$text), collapse =" ")
plusmin <- c(pos2,neg2)
corp2 <- VCorpus(VectorSource(plusmin))
corp2 <- clean_corpus(corp2)
tdm2 <- TermDocumentMatrix(corp2)
tdmatx <- as.matrix(tdm2)

comparison.cloud(tdm, colors = brewer.pal(senlen, "Set1"), scale = c(3,.5), random.order = FALSE, title.size = 1.5)

commonality.cloud(tdmatx, scale=c(5,1), max.words = 200,colors=brewer.pal(8, "Paired"))

textRC <- tidy(TermDocumentMatrix(tweetCorpusClean))
nrc_lex <- get_sentiments("nrc")
storyNRC <- inner_join(textRC, nrc_lex, by = c("term" = "word"))
storyNRC_noposneg <- storyNRC[!(storyNRC$sentiment %in% c("positive","negative")),]
aggdata <- aggregate(storyNRC_noposneg$count, list(index = storyNRC_noposneg$sentiment), sum)
chartJSRadar(aggdata)
