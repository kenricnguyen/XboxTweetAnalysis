#install required packages
install.packages("httr")
install.packages("twitteR")
install.packages("tm")

#load required packages
library(twitteR)
library(RColorBrewer)
library(wordcloud)
library(tm)


#Setting up the twitter oauth
my_consumer_key<-"RyjPkpeGx76LlsIbPJ9l9DqVl"
my_consumer_secret<-"Lh2iWYQwUKXkYyqRHZDYKs2iJ7tKQSbYMjBcDxizYJTz9nClGc"
setup_twitter_oauth(my_consumer_key, my_consumer_secret)

#1. get 1000 tweets from xbox company
username = '@Xbox'
Timelinetweet=userTimeline(username, n = 1000)
Timelinetext =sapply(Timelinetweet, function(x)x$getText())

# 2a. combine tweets about company and tweets from the company
allTweets = c(Timelinetext, xbox.text)

# 2b. cluster tweets using a clustering method

#prepare the tweet data by removing random chars

allTweets.corpus = Corpus(VectorSource(allTweets))
allTweets.corpus = tm_map(allTweets.corpus, removeNumbers)
allTweets.corpus = tm_map(allTweets.corpus, removePunctuation)
allTweets.corpus = tm_map(allTweets.corpus, stripWhitespace)
allTweets.corpus = tm_map(allTweets.corpus, tolower)
allTweets.corpus = tm_map(allTweets.corpus, removeWords, stopwords())
allTweets.corpus = tm_map(allTweets.corpus, stemDocument)
allTweets.corpus = tm_map(allTweets.corpus, PlainTextDocument)
allTweets.corpus = Corpus(VectorSource(allTweets.corpus$content))

tweet.dtm = DocumentTermMatrix(allTweets.corpus)

rowTotals = apply(tweet.dtm , 1, sum)   # Find the sum of words in each Document

tweet.dtm = tweet.dtm[rowTotals > 0, ]   # remove all docs without words
tweet.wdtm = weightTfIdf(tweet.dtm)

tweet.matrix = as.matrix(tweet.wdtm)

#2. Cluster allTweets using Multidimensional scaling with Euclidean distance method

## first normalise all tweets to unit length (divide by their norm)
norm.tweet.matrix = diag(1/sqrt(rowSums(tweet.matrix^2))) %*% tweet.matrix
## then create the distance matrix
D = dist(norm.tweet.matrix, method = "euclidean")^2/2

## perform MDS using 300 dimensions
mds.tweet.matrix <- cmdscale(D, k=300)

n = 15
SSW = rep(0, n)
for (a in 1:n) {
  ## use nstart to reduce the effect of the random initialisation
  K = kmeans(mds.tweet.matrix, a, nstart = 20)
  SSW[a] = K$tot.withinss
}
## plot the elbow
plot(1:n, SSW, type = "b")

#compute 3 clusters because of the elbow at the position
K = kmeans(mds.tweet.matrix, 3, nstart = 20)
mds2.tweet.matrix <- cmdscale(D, k=2)
#visualise the clusters in a 2D space with colours
plot(mds2.tweet.matrix, col = K$cluster)


#3 & 4 list the terms associated to each cluster and compute the proportion of company tweeets in each cluster
#cluster 1
cluster.number = 1
## find position of tweets in cluster
clusterTweetsId = which(K$cluster == cluster.number)
## extract tweets vectors for cluster
clusterTweets = tweet.matrix[clusterTweetsId,]
## combine the tweets into a mean tweet
clusterTermWeight = colMeans(clusterTweets)
## show the top 10 weighted words
sort(clusterTermWeight, decreasing = TRUE)[1:10]

#cluster 2
cluster.number = 2
## find position of tweets in cluster
clusterTweetsId = which(K$cluster == cluster.number)
## extract tweets vectors for cluster
clusterTweets = tweet.matrix[clusterTweetsId,]
## combine the tweets into a mean tweet
clusterTermWeight = colMeans(clusterTweets)
## show the top 10 weighted words
sort(clusterTermWeight, decreasing = TRUE)[1:10]

#cluster 3
cluster.number = 3
## find position of tweets in cluster
clusterTweetsId = which(K$cluster == cluster.number)
## extract tweets vectors for cluster
clusterTweets = tweet.matrix[clusterTweetsId,]
## combine the tweets into a mean tweet
clusterTermWeight = colMeans(clusterTweets)
## show the top 10 weighted words
sort(clusterTermWeight, decreasing = TRUE)[1:10]
