
consumer_key <-"jME9uoqLYAJ3CYDkAWOTmADj1"
consumer_secret <- "toWLd9BEm6KIjmYgLWzgB4U4HdQLTPVI2KjJmk5CMB7Wu1AVu1"
access_token<-"1719665600-EEvvPyi08FqacoOT5c9rRSOp223xx7OsEhMse05"
access_secret <- "KHnjkS9UQKphGyXimRl4LGTeOdgXP0nOIhZ7cMi8HttJ2"

#install.packages("httr")
library(httr)

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

1# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410/"
#
#    Replace key and secret below
myapp <- oauth_app("twitter",
                   key = consumer_key,
                   secret = consumer_secret
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",config(token = twitter_token))
stop_for_status(req)
content(req)

#install.packages("twitteR") 
#and
#install.packages("ROAuth")
library(ROAuth)
library(twitteR)

#install.packages("devtools")
library(devtools)
#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")


  
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem") #downloads the certificate

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

cred <- OAuthFactory$new(consumerKey=consumer_key, 
                         consumerSecret=consumer_secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")

#Extract tweets. Example-
s9.tweets = searchTwitter("Samsung+Galaxy+s9",lang = "en", n=25000)


setwd("C:/Users/DTP/Desktop/MS Analytics/Sentimental Analysis")
neg.words = scan("negative-words.txt", what="character", comment.char=";")
pos.words = scan("positive-words.txt", what="character", comment.char=";")

#Extracting textual part of the tweets

sample=NULL #Initialising
for (tweet in s9.tweets)
sample = c(sample,tweet$getText())

#converts to data frame
df <- do.call("rbind", lapply(s9.tweets, as.data.frame))

#remove odd characters
df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) #remove URL
sample <- df$text



score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, `[[`, 2)
  nn1 = lapply(list, `[[`, 3)
  
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

# Clean the tweets
result = score.sentiment(sample, pos.words, neg.words)
#install.packages("reshape")
library(reshape)
#Creating a copy of result data frame
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1,var='Score')
qq2=melt(q2,var='Positive')
qq3=melt(q3,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)



#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

allwords<-c("Anger","disgust","Not Satisfied","Equi-Vocal","Satisfied","Joy","Extreemely Happy")
#Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10),xaxt='n',xlab = "Sentiments",ylab = "Tweets",main="Sentiments on Samsung S9")
axis(1, at = seq(-3,3,by=1), labels = allwords)

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Positive", "Negative")
#install.packages("plotrix")
library(plotrix)
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Twitter Sentiment Analysis of Samsung S9--(Release date Feb 25,2018)")
