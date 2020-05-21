####Extract Snapdeal Reviews
library(rvest)
library(XML)
library(magrittr)

#### emotion mining libraries####
install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(data.table)
install.packages("tm")
library(tm)

# Snapdeal reviews #############################
########Web Scraping
surl_1 <- "https://www.snapdeal.com/product/samsung-galaxy-J3-8gb-4g/676860597612/ratedreviews?page="
surl_2 <- "&sortBy=HELPFUL&ratings=4,5#defRevPDP"
snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"samsung.txt",row.names = FALSE)
getwd()

#####Read the file#########################
#text <- paste(readLines("samsung.txt"),sep = "")
#class(text)

###################################
text <- read.delim('samsung.txt')
str(text)
dim(text)
head(text)

# Build Corpus and DTM/TDM
new_text <- text[-1,]
head(new_text)
############################

x <- as.character(new_text)

# Corpus
x <- Corpus(VectorSource(x))
inspect(x[1])


# Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x, removePunctuation)
inspect(x1[1])
x1 <- tm_map(x1, removeNumbers)
x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
dtm <- t(tdm)
tdm <- as.matrix(tdm)

# Bar plot
w <- rowSums(tdm)
w
w_sub <- subset(w, w >= 20)
w_sub
windows()
barplot(w_sub, las=2, col = rainbow(20))

# Term snapdeal, etc repeats in all most all documents
x1 <- tm_map(x1, removeWords, c('samsung','phone','device','product','item','Snapdeal'))
x1 <- tm_map(x1, stripWhitespace)
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)


# Word cloud
install.packages("wordcloud")
library(wordcloud)

windows()
wordcloud(words = names(w_sub), freq = w_sub) # wordcloud with only subset of words

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

#Better way of representation
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order = F, colors = rainbow(20), scale=c(3,1), rot.per = 0.3)

# lOADING +VE AND -VE dictonaries
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "Awesome", "accha", "nyc") # including our own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w_sub1), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
p_names <- names(freq_pos)

windows()
wordcloud(p_names,freq_pos,scale=c(4,1),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w_sub1), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
freq_neg <- na.omit(freq_neg)
n_names <- names(freq_neg)

windows()
wordcloud(n_names,freq_neg,scale=c(3,1),colors = brewer.pal(8,"Dark2"))


######################Emotion Mining, further Analysis#########################

#Perform Sentiment Analysis of Aquaman
# Read File 
text <- read.delim('samsung.txt')
reviews <- as.character(text[-1,])

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews) 
head(s)

get_nrc_sentiment('bad') #1 Anger and 1 Disgust, 1 fear, 1 sadness, 1 negative
get_nrc_sentiment('no words') #1 Anger and 1 Negative
get_nrc_sentiment('splendid')  #1 Joy and 1 Surprise, 1 positive


#each sentences by eight 
example<-get_sentences(reviews)
nrc_data<-get_nrc_sentiment(example)
head(nrc_data)


# Bar plot for emotion mining
windows()
barplot(colSums(nrc_data), las = 1, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')


#sentiment_vector<-get_sentiment(example,method="bing")
#sentiment_afinn<-get_sentiment(example,method="afinn")
sentiment_vector<-get_sentiment(example,method="nrc")

#Descriptive Statistics
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

windows()
plot(sentiment_vector,type='l',main ='Plot trajectory', xlab='Narative time',ylab='Emotional valence')
abline(h=0,col='red')

plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

##Shape smoothing and normalization using a Fourier based transformation and low pass filtering is achieved using the get_transformed_values function as shown below.
ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)
plot(
  ft_values, 
  type ="l", 
  main ="Snapdeal reviews using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
#Most Negative and Positive reviews
negative<-example[which.min(sentiment_vector)]
positive<-example[which.max(sentiment_vector)]
