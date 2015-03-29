library(tm)
library(rJava)
library(RWeka)
library(wordcloud)
library(ggplot2)

.jinit(parameters="-Xmx128g")

blogsTxt <- readLines("final/en_US/en_US.blogs.txt", skipNul = T)
newsTxt <- readLines("final/en_US/en_US.news.txt", skipNul = T)
twitterTxt <- readLines("final/en_US/en_US.twitter.txt", skipNul = T)

sampleSize = 10000

set.seed(666)

blogs <- blogsTxt[sample(1:length(blogsTxt), sampleSize)]
news <- newsTxt[sample(1:length(newsTxt), sampleSize)]
twitter <- twitterTxt[sample(1:length(twitterTxt), sampleSize)]

sample <- c(blogs, news, twitter)
sample <- iconv(sample, from="latin1", to="ASCII", sub="")

#Make some Wordclouds

wordcloud(twitter, scale=c(1,2),max.words=100, colors=pal, random.color=T)
wordcloud(news, scale=c(1,2),max.words=100, colors=pal, random.color=T)
wordcloud(blogs, scale=c(1,2),max.words=100, colors=pal, random.color=T)
wordcloud(sample, scale=c(1,2),max.words=100, colors=pal, random.color=T)

#Use the tm package for data cleaning...
data <- VCorpus(VectorSource(sample))

data <- tm_map(data, tolower)
data <- tm_map(data, removePunctuation)
data <- tm_map(data, removeNumbers)
data <- tm_map(data, removeWords, stopwords("SMART"))
data <- tm_map(data, stemDocument)
data <- tm_map(data, stripWhitespace)

unigrams <- NGramTokenizer(data, control = Weka_control(min = 1, max = 1))
bigrams <- NGramTokenizer(data, control = Weka_control(min = 2, max = 2))
trigrams <- NGramTokenizer(data, control = Weka_control(min = 3, max = 3))
quadgrams <- NGramTokenizer(data, control = Weka_control(min = 4, max = 4))
quingrams <- NGramTokenizer(data, control = Weka_control(min = 5, max = 5))

uniData <- data.frame(table(unigrams))
uniData <- uniData[order(uniData$Freq,decreasing=TRUE),]
barplot(uniData[1:25,]$Freq, names.arg=uniData[1:25,]$unigrams,las=2, main="Top 25 Unigrams", cex.names=0.8, , col="red")

biData <- data.frame(table(bigrams))
biData <- biData[order(biData$Freq,decreasing=TRUE),]
par(mar=c(6,3,1,1))
barplot(biData[1:25,]$Freq, names.arg=biData[1:25,]$bigrams,las=2, main="Top 25 Bigrams", cex.names=0.8, col="blue")

triData <- data.frame(table(trigrams))
triData <- triData[order(triData$Freq,decreasing=TRUE),]
par(mar=c(8,3,1,1))
barplot(triData[1:25,]$Freq, names.arg=triData[1:25,]$trigrams,las=2, main="Top 25 Trigrams", cex.names=0.8, col="green")

quadData <- data.frame(table(quadgrams))
quadData <- quadData[order(quadData$Freq,decreasing=TRUE),]
par(mar=c(12,3,1,1))
barplot(quadData[1:25,]$Freq, names.arg=quadData[1:25,]$quadgrams,las=2, main="Top 25 Quadgrams", cex.names=0.8, col="purple")

quinData <- data.frame(table(quingrams))
quinData <- quinData[order(quinData$Freq,decreasing=TRUE),]
par(mar=c(16,3,1,1))
barplot(quinData[1:25,]$Freq, names.arg=quinData[1:25,]$quingrams,las=2, main="Top 25 Quingrams", cex.names=0.8, col="orange")


