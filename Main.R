#install the necessary packages
#install.packages("ROAuth")
#install.packages("twitteR")
#install.packages("wordcloud")
#install.packages("tm")

library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")
library("ggplot2")
library("reshape2")
library("sentiment")
#install.packages(c("devtools", "rjson", "bit64", "httr"))

#install.packages(c("httpuv"))
library("httpuv")
install_github("twitteR", username="geoffjentry")


library(twitteR)

setup_twitter_oauth("api key", "api key secret")
Kejriwal_tweets = searchTwitter("#Kejriwal", n=1500)
#str(Kejriwal_tweets)
Kejriwal_txt = sapply( unlist(Kejriwal_tweets) , function(x) '$'( x , "text"))
#dput(Kejriwal_txt)
Kejriwals = paste(Kejriwal_txt, collapse=" ")
kb_corpus = Corpus(VectorSource(Kejriwals))
kb.tf <- list(weighting = weightTf, stopwords = {},
              removePunctuation = TRUE,
              tolower = TRUE,
              minWordLength = 4,
              removeNumbers = TRUE, stripWhitespace = TRUE,
              stemDocument= TRUE)

# term-document matrix
tdm = TermDocumentMatrix(kb_corpus, control = kb.tf)

# convert as matrix
tdm = as.matrix(tdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(tdm), decreasing=TRUE)

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
p <- ggplot(subset(dm, freq>10), aes(word, freq))
p <-p+ geom_bar(stat="identity")
p <-p+ theme(axis.text.x=element_text(angle=45, hjust=1))

p

#Plot Wordcloud
Kejriwal_clean = Kejriwal_txt
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(6, "Dark2"),min.freq=10, scale=c(4,.2),rot.per=.15,max.words=100)
#Sentiment Analysis - AAM ADMI PARTY
# run model

Kejriwal_class_emo = classify_emotion(Kejriwal_clean, algorithm="bayes", prior=1.0)

# Fetch emotion category best_fit for our analysis purposes, visitors to this tutorials are encouraged to play around with other classifications as well.
emotion1 = Kejriwal_class_emo[,7]

# Replace NA’s (if any, generated during classification process) by word “unknown”
emotion1[is.na(emotion1)] = "unknown"

# Similar to above, we will classify polarity in the text
# This process will classify the text data into four categories (pos – The absolute log likelihood of the document expressing a positive sentiment, neg – The absolute log likelihood of the document expressing a negative sentimen, pos/neg  – The ratio of absolute log likelihoods between positive and negative sentiment scores where a score of 1 indicates a neutral sentiment, less than 1 indicates a negative sentiment, and greater than 1 indicates a positive sentiment; AND best_fit – The most likely sentiment category (e.g. positive, negative, neutral) for the given text)

Kejriwal_class_pol = classify_polarity(Kejriwal_clean, algorithm="bayes")

# we will fetch polarity category best_fit for our analysis purposes, and as usual, visitors to this tutorials are encouraged to play around with other classifications as well
polarity1 = Kejriwal_class_pol[,4]

# Let us now create a data frame with the above results obtained and rearrange data for plotting purposes
# creating data frame using emotion category and polarity results earlier obtained

sentiment_dataframe = data.frame(text=Kejriwal_clean, emotion=emotion1, polarity=polarity1, stringsAsFactors=FALSE)

# rearrange data inside the frame by sorting it
sentiment_dataframe = within(sentiment_dataframe, emotion1 <- factor(emotion1, levels=names(sort(table(emotion1), decreasing=TRUE))))

# In the next step we will plot the obtained results (in data frame)

# First let us plot the distribution of emotions according to emotion categories
# We will use ggplot function from ggplot2 Package (for more look at the help on ggplot) and RColorBrewer Package

ggplot(sentiment_dataframe, aes(x=emotion1)) + geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle('Sentiment Analysis of Tweets on Twitter about AAP') +
  theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')

write.csv(sentiment_dataframe,"AAP_Data.csv")
