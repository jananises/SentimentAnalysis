#Assignment to extract IMDB movie reviews of Movie - Aquaman
#Load all the libraries
library(rvest)
library(magrittr)
library(XML)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

 ################################################
 # IMDBReviews #############################
#Web Scraping######################
 aurl <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
 IMDB_reviews <- NULL
 for (i in 1:10){
   murl <- read_html(as.character(paste(aurl,i,sep="=")))
   rev <- murl %>%
     html_nodes(".show-more__control") %>%
     html_text()
   IMDB_reviews <- c(IMDB_reviews,rev)
 }
 length(IMDB_reviews)
 
 write.table(IMDB_reviews,"Aquaman.txt",row.names = F)
 
 
 Aquaman <- read.delim('Aquaman.txt')
 str(Aquaman)
 dim(Aquaman)
 head(Aquaman)
 
 # Build Corpus and DTM/TDM
 corpus <- Aquaman[-1,]
 head(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

 
 # Clean the text 
 corpus <- tm_map(corpus,tolower)
 inspect(corpus[1:5])
 
 # Remove Punctuation
 corpus <- tm_map(corpus,removePunctuation)
 inspect(corpus[1:5])

 # Remove numbers
 corpus <- tm_map(corpus,removeNumbers) 
 inspect(corpus[1:5])

 #Strip whitespaces
 corpus_clean<-tm_map(corpus,stripWhitespace)
 inspect(corpus[1:5])
 
 #remove stopwords
 cleanset<-tm_map(corpus,removeWords, stopwords('english'))
 inspect(cleanset[1:5]) 
 
 #remove URL
 removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
 cleanset <- tm_map(cleanset, content_transformer(removeURL))
 inspect(cleanset[1:5]) 
 
 #remove \n 
 removen <- function(x) gsub('\n','',x)
 cleanset <- tm_map(cleanset, content_transformer(removen))
 inspect(cleanset[5:7]) 
 
 #remove common words
 cleanset<-tm_map(cleanset,removeWords, c('can','film','movie','movies'))

 # Removing the word movie and movies on similar grounds - as unnecessary.
 cleanset <- tm_map(cleanset, gsub, pattern = 'character', replacement = 'characters')
 inspect(cleanset[1:5]) 

 cleanset <- tm_map(cleanset,stripWhitespace)
 inspect(cleanset[1:5]) 

 #Term Document Matrix :
 # Convert the unstructured data to structured data :
 tdm <- TermDocumentMatrix(cleanset)
 dtm <- t(tdm)
 
 # the terms indicate that there are 13649 words and 393036 documents(# of tweets) in this TDM
 # Sparsity is 97% which indicates that there are lots of zero values.
 tdm <- as.matrix(tdm)
 tdm[1:10,1:20]
 
 
 # Bar Plot 
 w <- rowSums(tdm)  # provides the no of times a particular word has been used.
 #w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
 w_sub <- subset(w, w>= 25) # Pull words that were used more than 25 times.
 
 barplot(w_sub, las = 3, col = rainbow(25))

 # Word Cloud :
 w_sub1  <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
 set.seed(123)
 wordcloud(words = names(w_sub1), freq = w_sub1, 
           max.words = 250,random.order = F,
           min.freq =  3, 
           colors = brewer.pal(8, 'Dark2'),
           scale = c(5,0.3),
           rot.per = 0.6)

 #####################################################
 
 
 # lOADING +VE AND -VE dictonaries
 pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
 neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
 
 
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
 wordcloud(n_names,freq_neg,scale=c(5,1),colors = brewer.pal(8,"Dark2"))
 
 
######################Emotion Mining, further Analysis#########################
 
#Perform Sentiment Analysis of Aquaman
# Read File 
IMDB_reviews <- read.delim('Aquaman.txt')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)


# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews) 
head(s)
reviews[2]

get_nrc_sentiment('bad') #1 Anger and 1 Disgust, 1 fear, 1 sadness, 1 negative
get_nrc_sentiment('no words') #1 Anger and 1 Negative
get_nrc_sentiment('splendid')  #1 Joy and 1 Surprise, 1 positive

# barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for Aquaman')


#Sentence Tokenization. Parses a string into a vector of sentences
example<-get_sentences(reviews)
nrc_data<-get_nrc_sentiment(example)
head(nrc_data)


sentiment_vector<-get_sentiment(example,method="nrc")

#Descriptive Statistics
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

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
  main ="Aquaman reviews using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
#Most Negative and Positive reviews
negative<-example[which.min(sentiment_vector)]
positive<-example[which.max(sentiment_vector)]
