#install required packages
install.packages("tm")
install.packages("RColorBrewer")
install.packages("wordcloud")

#Loading required packages
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)

#Part 1: Analysis of language in a random sample of tweets

#1. load data from randomSample.csv for processing
randomSample.text <- readLines("randomSample2016.csv")

#2. process the data to corpus to filter out random words
randomSample.corpus <- Corpus(VectorSource(randomSample.text))

# Remove random meaningless characters
randomSample.corpus <- tm_map(randomSample.corpus, removeNumbers)
randomSample.corpus <- tm_map(randomSample.corpus, removePunctuation)
randomSample.corpus <- tm_map(randomSample.corpus, stripWhitespace)
randomSample.corpus <- tm_map(randomSample.corpus, content_transformer(tolower))
randomSample.corpus <- tm_map(randomSample.corpus, stemDocument) 
randomSample.corpus <- tm_map(randomSample.corpus, removeWords, stopwords("english"))

# calculate Document Term Matrix
randomSample.dtm <- TermDocumentMatrix(randomSample.corpus)
randomSample.dtm <- removeSparseTerms(randomSample.dtm,0.999)
randomSample.dtm
randomSample.matrix <- as.matrix(randomSample.dtm)


#calculate row sum/ frequency of each word occuring in the matrix
randomSample.freqs=rowSums(randomSample.matrix)

#sort out the top 10 words from the processed randomSample
randomSample.sort <- sort(rowSums(randomSample.matrix),decreasing=TRUE)
randomSample.sort<-as.matrix(randomSample.sort)
topTenWords<-head(randomSample.sort,10)
topTenWords

topTenWords <- data.frame(word = topTenWords[,0], freq=topTenWords[,1], prop = topTenWords[,1]/sum(topTenWords))
#show the top 10 words and their proportions
topTenWords

#compile a wordcloud based on random sample data
set.seed(1234)
par(mar = rep(2, 4))
wordcloud(words = randomSample.corpus,max.words=40, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Part 2: Analysis of the company image on Twitter


#Part 3: Find the connection between public and company.

##library("twitteR")
##username = '@XboxAustralia'
##Timelinetweet<-userTimeline("XboxAustralia", n= 1000)
##require(twitteR)
##           8.3 question 1
## To access the Twitter API, we must register a program that is accessing the API. To register the program, visit this link:

##https://dev.twitter.com/apps
##and log in. Once you have logged in, click "Create a new application" and fill in the form. Ensure that the Callback URL is set to http://127.0.0.1:1410 When the application is created, it will be provided a consumer key and a consumer secret (both text sequences); these text sequences are needed soon, so create a new R script and record the values:
  
  ##key = "put key string here"
##secret = "put secret string here"

username = 'XboxAustralia'
Timelinetweet=userTimeline(username, n = 1000)
Timelinetext =timelinetext(Timelinetweet, function(x)x$getText())
#create a corpus
Timelinecorpus = Corpus(VectorSource(Timelinetext))
#remove unwanted parts from the text
Timelinecorpus = tm_map(Timelinecorpus, function(x) iconv(x, to = 'ASCII',sub = ''))
Timelinecorpus = tm_map(Timelinecorpus, removeNumbers)
Timelinecorpus = tm_map(Timelinecorpus, removePunctuation)
Timelinecorpus = tm_map(Timelinecorpus, stripWhitespace)
Timelinecorpus = tm_map(Timelinecorpus, tolower)
Timelinecorpus = tm_map(Timelinecorpus, removeWords, stopwords())
Timelinecorpus = tm_map(Timelinecorpus, PlainTextDocument)
Timelinecorpus = tm_map(Timelinecorpus, stemDocument)
# create document term matrix applying some transformations
Timetdm = TermDocumentMatrix(Timelinecorpus) # Convert to a standard R matrix
T = as.matrix(Timetdm)
print(Timelinetweet)

Timelinetweet[1:5]
##8.3 question 2
tweets = c(Timelinetext, Timelinetweet)
print(tweets)
length(tweets) ##8.3 question 3
library(twitteR)
library(tm)
library(SnowballC)
#download and load tweets.RData from week 8 lab before proceeding
#load(tweets.RData)
#df1 = twListToDF(tweets1)
#df2 = twListToDF(tweets2)
#df3 = twListToDF(tweets3)
#tweet.text = c(df1$text, df2$text, df3$text)
tweet.corpus = Corpus(VectorSource(tweets))
tweet.corpus = tm_map(tweet.corpus, function(x) iconv(x, to='ASCII'))
tweet.corpus = tm_map(tweet.corpus, removeNumbers)
tweet.corpus = tm_map(tweet.corpus, removePunctuation)
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
tweet.corpus = tm_map(tweet.corpus, tolower)
tweet.corpus = tm_map(tweet.corpus, removeWords, stopwords('english'))
#tweet.corpus = tm_map(tweet.corpus, PlainTextDocument)

tweet.corpus = tm_map(tweet.corpus, stemDocument)

tweet.dtm = DocumentTermMatrix(tweet.corpus)
tweet.wdtm = weightTfIdf(tweet.dtm)
tweet.matrix = as.matrix(tweet.wdtm)
## remove empty tweets
empties = which(rowSums(abs(tweet.matrix)) == 0)
tweet.matrix = tweet.matrix[-empties,]
print(tweet.matrix)

#determine the best num of clusters
n=15
SSW=rep(0,n)
for (a in 1:n){
  ##use nstart to redeuce the effect of the random init
  K=kmeans(tweet.matrix,a,nstart=20)
  SSW[a]=K$tot.withinss
}
plot(1:n, SSW, type="b")

#calculate k mean
 #k = kmeans(tweet.matrix, 1000)
number.of.clusters = 2
print(iris)
K = kmeans(tweet.matrix, number.of.clusters)
tweet.2d=cmdscale(dist(tweet.matrix))
plot(tweet.2d,col=K$cluster,pch=as.numeric(tweet.matrix))
legend("bottomright",levels(tweet.matrix,pch=c(1,2)))
