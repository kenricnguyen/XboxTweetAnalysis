#install required packages
install.packages("httr")
install.packages("twitteR")

#load required packages
library(httr)
library(twitteR)


#Setting up the twitter oauth
my_consumer_key<-"RyjPkpeGx76LlsIbPJ9l9DqVl"
my_consumer_secret<-"Lh2iWYQwUKXkYyqRHZDYKs2iJ7tKQSbYMjBcDxizYJTz9nClGc"
setup_twitter_oauth(my_consumer_key, my_consumer_secret)

#search for 1000 tweets about Xbox
xbox.tweets= searchTwitter('@xbox ', n = 1000, lang = "en")
xbox.text = sapply(xbox.tweets, function(x) x$getText())

#combine random tweets and xbox tweets
combinedTweets.text=c(xbox.text,randomSample.text)

#remove meaningless chars
combinedTweets.corpus = Corpus(VectorSource(combinedTweets.text)) 
combinedTweets.corpus = tm_map(combinedTweets.corpus, function(x) iconv(x, to='ASCII', sub=' '))
combinedTweets.corpus = tm_map(combinedTweets.corpus, removeNumbers)
combinedTweets.corpus = tm_map(combinedTweets.corpus, removePunctuation)
combinedTweets.corpus = tm_map(combinedTweets.corpus, stripWhitespace)
combinedTweets.corpus = tm_map(combinedTweets.corpus, tolower)
combinedTweets.corpus = tm_map(combinedTweets.corpus, removeWords, stopwords())
combinedTweets.corpus = tm_map(combinedTweets.corpus, stemDocument) 

# calculate Document Term Matrix
combinedTweets.dtm <- TermDocumentMatrix(combinedTweets.corpus)
combinedTweets.dtm

#process xbox data
xbox.corpus = Corpus(VectorSource(xbox.text)) 
xbox.corpus = tm_map(xbox.corpus, function(x) iconv(x, to='ASCII', sub=' '))
xbox.corpus = tm_map(xbox.corpus, removeNumbers)
xbox.corpus = tm_map(xbox.corpus, removePunctuation)
xbox.corpus = tm_map(xbox.corpus, stripWhitespace)
xbox.corpus = tm_map(xbox.corpus, tolower)
xbox.corpus = tm_map(xbox.corpus, removeWords, stopwords())
xbox.corpus = tm_map(xbox.corpus, stemDocument) 
xbox.corpus = tm_map(xbox.corpus, removeWords, c("xbox"))

#get term document matrix analysis
xbox.tdm <- TermDocumentMatrix(xbox.corpus)
xbox.tdm

# Convert to a standard R matrix
xbox.matrix <- as.matrix(xbox.tdm)

#obtain a vector of term frequencies over all company tweets
xbox.freqs=rowSums(xbox.matrix)

#create a Company tweet data frame from term freq vector
xbox.df=data.frame(XboxDataFreq=xbox.freqs)

#obtain a vector of term frequencies over all random tweets
randomSample.freqs=rowSums(randomSample.matrix)

#create a random tweets data frame from term freq vecto
randomSample.df=data.frame(RandomDataFreq=randomSample.freqs)

#combine the above 2 tables of frequencies
combinedFreqTable=merge(randomSample.df,xbox.df,by=0,all=TRUE)
combinedFreqTable[is.na(combinedFreqTable)] = 0 
rownames(combinedFreqTable)=combinedFreqTable[,1]
combinedFreqTable[,1]=NULL

#detabulate this combinedFreqTable
word = c()
tweet = c()
for (wordIndex in (1:dim(combinedFreqTable)[1])) {
  for (tweetIndex in (1:dim(combinedFreqTable)[2])) {
    n = combinedFreqTable[wordIndex,tweetIndex]
    newTweet = rep(colnames(combinedFreqTable)[tweetIndex], n)
    newWord = rep(rownames(combinedFreqTable)[wordIndex], n)
    word = c(word, newWord)
    tweet = c(tweet, newTweet)
  }
}

chiTable = data.frame(Word = word, Tweet = tweet)
chiTable

#do chi square test
chisq.test(table(chiTable))

