
library(httr)

# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

1# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410/"
#
#    Replace key and secret below
myapp <- oauth_app("twitter",
                   key = "D5WAS4HL7eY4AavvCK5qFmLW0",
                   secret = "kfGBtDaXwIKNiZjRf0F3af8oZMRdobAUGtIPEDEdbB7Sxe3aG2"
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",
           config(token = twitter_token))
stop_for_status(req)
content(req)

#install.packages("httpuv")
#library(httpuv)

#install.packages("devtools")
library(devtools)
#devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")
#install.packages("twitteR")
library(stringr)
library(twitteR)
#install.packages("ROAuth")
library(ROAuth)
#install.packages("xlsx")
library(xlsx)
library(plyr)

#install.packages("base64enc",dependencies = T)
library(base64enc)


api_key<- "D5WAS4HL7eY4AavvCK5qFmLW0"
api_secret <- "kfGBtDaXwIKNiZjRf0F3af8oZMRdobAUGtIPEDEdbB7Sxe3aG2"
access_token <- "1719665600-gb6Y69aq3CV2PqVYDf8Hmf7UF3xSg1rvYPjqwy9"
access_token_secret <- "0I8mVS7OSa5hTvGxTYVsxMOzASAt44w6mLwrtj4cKs6w2"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# cred <- OAuthFactory$new(consumerKey='D5WAS4HL7eY4AavvCK5qFmLW0', consumerSecret='kfGBtDaXwIKNiZjRf0F3af8oZMRdobAUGtIPEDEdbB7Sxe3aG2',requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')
# 
# cred$handshake(cainfo="cacert.pem")

# consumer_key <-"D5WAS4HL7eY4AavvCK5qFmLW0"
# consumer_secret <- "kfGBtDaXwIKNiZjRf0F3af8oZMRdobAUGtIPEDEdbB7Sxe3aG2"
# access_token<-"1719665600-gb6Y69aq3CV2PqVYDf8Hmf7UF3xSg1rvYPjqwy9"
# access_secret <- "0I8mVS7OSa5hTvGxTYVsxMOzASAt44w6mLwrtj4cKs6w2"
# 
# setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )

cred <- OAuthFactory$new(consumerKey='AKJsxNqX2D8uTo9orgjRirvWL', consumerSecret='QOKk0ctHhbXNQ5QaipqofrZQzWM92mfkcoP60xe7HJzjSUCz6F',requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")

setwd("C:/Users/DTP/Desktop/MS Analytics/Sentimental Analysis")
neg.words = scan("negative-words.txt", what="character", comment.char=";")
pos.words = scan("positive-words.txt", what="character", comment.char=";")


score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}


tweets = searchTwitter('S9',n=500)
Tweets.text = laply(tweets,function(t)t$getText()) # gets text from Tweets

analysis = score.sentiment(Tweets.text, pos, neg) # calls sentiment function
