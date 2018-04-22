#Get our Packages going
library(twitteR)
library(rtweet)
library(data.table)
library(stringr)
library(tidytext)

#Set up our keys
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

conKey <- 'hqUWrh4fSHpYXPbrLgksTeFvu'
conSecret <- 'WpH7PbccesijgvF5P9PbRLEo0nUavP5XNQoj0XxTIEg0uEth3y'
accessToken <- '875723142605537281-yosLprw3vBxiav892EZUBphxCA4f5XI'
accessSecret <- 'xd2J4cKWYbB1MvvQagVNStoWcL73KcznLHE7ILNhay7rV'
setup_twitter_oauth(conKey, conSecret, accessToken, accessSecret)

#Import 2500 tweets (would import more but my laptop is hot garbage)
tweetList <- searchTwitter('celtics',n=2500)

#Convert to dataframe
tweetList.df <- twListToDF(tweetList)

#Write to CSV for later use
write.csv(tweetList.df, file = "/home/phil/Desktop/Stuff For School/Spring/DATA 902/listoftweets.csv",row.names=FALSE)
