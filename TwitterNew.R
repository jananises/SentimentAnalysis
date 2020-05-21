################################################
#Extract from Twitter feeds
##########################################
install.packages("twitteR")
library("twitteR")

install.packages("ROAuth")
library("ROAuth")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)
### https://apps.twitter.com/

#### emotion mining ####
install.packages("syuzhet")
library("syuzhet")
install.packages("lubridate")
#######################################
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
#############################

#  Install Requried Packages
#installed.packages("SnowballC")
#installed.packages("tm")


# Load Requried Packages
#library("SnowballC")
#library("tm")


#####################################################

cred <- OAuthFactory$new(consumerKey='Qf3Gj72iwaGFsQ9KQj0P3eE9q',
                         consumerSecret='xA12FDFpllBsxlShxOUu5O3YadNIQmxeCCrGyA6fbuNn2esnrl',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#save(cred, file="twitter authentication.Rdata")
#load("twitter authentication.Rdata")

setup_twitter_oauth("Qf3Gj72iwaGFsQ9KQj0P3eE9q", 
                    "xA12FDFpllBsxlShxOUu5O3YadNIQmxeCCrGyA6fbuNn2esnrl",
                    "1248917668368805888-y8BtDgUeTUXpmXXkPi04oNQwdYLjMx", # Access token
                    "7RaQKJKiHD5POu4OfI7XKH3bDfDRXqfIpkaNrWWyFrCex")  # Access token secret key

Tweets1 <- userTimeline('RNTata2000', n = 1000)
n.tweet <- length(Tweets1)
TweetsDF <- twListToDF(Tweets1)
write.csv(TweetsDF, "Tweets_1tata.csv")


##Cleaning the Tweets for Further Analysis
head(TweetsDF)
head(TweetsDF$text)

#This contains a lot of URLs, hashtags and other twitter handles. We will remove all these using the gsub function.
tweets.df2 <- gsub("http.*","",TweetsDF$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@.*","",tweets.df2)

#Our output now looks like below:
head(tweets.df2)


#Getting sentiment score for each tweet

word.df <- as.vector(tweets.df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)

##Plots
#each sentences by eight 
example<-get_sentences(word.df)  #example
nrc_data <-get_nrc_sentiment(example) # nrc_data -> emotion.df 

sentiment_vector<-get_sentiment(example,method="bing")
sentiment_afinn<-get_sentiment(example,method="afinn")


############################
# Bar plot for emotion mining
barplot(colSums(nrc_data), las = 1, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')
##########################

sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)


plot(sentiment_vector,type='l',main ='Plot trajectory', xlab='Narative time',ylab='Emotional valence')
abline(h=0,col='red')


plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

#####################################################################





#The above output shows us the different emotions present in each of the tweets.
#Now, we will use the get_sentiment function to extract sentiment score for each of the tweets.

sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]

most.positive

#Most Negative
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

#Let us see how the score of each of the tweets has been calculated. In all, there are 127 tweets that we are evaluating, so there should be 127 positive/negative scores, one for each of the tweets.
value <- sent.value

#Segregating positive and negative tweets
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

#Negative tweet
negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

#Neutral Tweet
neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)



# Alternate way to classify as Positive, Negative or Neutral tweets
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)


category_senti2 <- cbind(tweets.df2,category_senti,value)
head(category_senti2)

#So, now we have analyzed the twitter handle of Ratan Tata and got the sentiment around tweets. The break of total number of tweets by sentiment is

table(category_senti)

