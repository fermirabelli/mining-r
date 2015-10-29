library(twitteR)
library(plyr)
update.packages()

df.cat <- rbind(dfSub[[1]], dfSub[[2]], dfSub[[3]], dfSub[[4]])

df.union <- unique(df.cat)


require(twitteR)

pos=scan('positive-words.txt',what='character',comment.char=';')

tweets <- searchTwitter("#Elecciones2015",n=3000)
                          
tweetdataframe <- do.call("rbind",lapply(tweets,as.data.frame))
                          
write.table(tweetdataframe,"c:/users/paras/desktop/tweetsaboutexcel.txt",sep="\t")

#tweets <- userTimeline("RDataMining", n = 3200)
load(file = "rdmTweets-201306.RData")
"rdmTweets"
tweets<-rdmTweets

tweets<-rdmTweets
tweets.df <- twListToDF(tweets)

length(tweets)

# convert tweets to a data frame

for (i in c(1:2, 154)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(tweets.df$text[i], 60))
}

Text Cleaning

library(tm)

# build a corpus, and specify the source to be character vectors

myCorpus <- Corpus(VectorSource(tweets.df$text))

# convert to lower case

# tm v0.6

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# tm v0.5-10

# myCorpus <- tm_map(myCorpus, tolower)

# remove URLs

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

# tm v0.6

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# tm v0.5-10

# myCorpus <- tm_map(myCorpus, removeURL)
# remove anything other than English letters or space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove punctuation

# myCorpus <- tm_map(myCorpus, removePunctuation)

# remove numbers

# myCorpus <- tm_map(myCorpus, removeNumbers)

# add two extra stop words: "available" and "via"

myStopwords <- c(stopwords('english'), "available", "via")
myStopwords <- c(stopword ('spanish'),"available","via")
myStopwords
# remove "r" and "big" from stopwords

myStopwords <- setdiff(myStopwords, c("r", "big"))

# remove stopwords from corpus

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# remove extra whitespace

myCorpus <- tm_map(myCorpus, stripWhitespace)


# inspect the first 5 documents (tweets)

# inspect(myCorpus[1:5])

# The code below is used for to make text fit for paper width

for (i in c(1:2, 154)) {
   cat(paste0("[", i, "] "))
   writeLines(strwrap(as.character(myCorpus[[i]]), 60))
 }


# keep a copy of corpus to use later as a dictionary for stem completion

myCorpusCopy <- myCorpus

# stem words

myCorpus <- tm_map(myCorpus, stemDocument)

# inspect the first 5 documents (tweets)

# inspect(myCorpus[1:5])

# The code below is used for to make text fit for paper width

for (i in c(1:2, 154)) {
  
  cat(paste0("[", i, "] "))
  
  writeLines(strwrap(as.character(myCorpus[[i]]), 60))
  
}

# tm v0.5-10

# myCorpus <- tm_map(myCorpus, stemCompletion)

# tm v0.6

stemCompletion2 <- function(x, dictionary) {
  
  x <- unlist(strsplit(as.character(x), " "))
  
  # Unexpectedly, stemCompletion completes an empty string to
  
  # a word in dictionary. Remove empty string to avoid above issue.
  
  x <- x[x != ""]
  
  x <- stemCompletion(x, dictionary=dictionary)
  
  x <- paste(x, sep="", collapse=" ")
  
  PlainTextDocument(stripWhitespace(x))
  
}

myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)

myCorpus <- Corpus(VectorSource(myCorpus))


# count frequency of "mining"

miningCases <- lapply(myCorpusCopy,
                      
                      function(x) { grep(as.character(x), pattern = "\\<mining")} )

sum(unlist(miningCases))

## [1] 82

# count frequency of "miner"

minerCases <- lapply(myCorpusCopy,
                     
                     function(x) {grep(as.character(x), pattern = "\\<miner")} )

sum(unlist(minerCases))

## [1] 5

# replace "miner" with "mining"

myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                   
                   pattern = "miner", replacement = "mining")


tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
myCorpus
tdm

#Frequent Word and Associations 

idx <- which(dimnames(tdm)$Terms == "r")

inspect(tdm[idx + (0:5), 101:110])
# inspect frequent words

(freq.terms <- findFreqTerms(tdm, lowfreq = 70))

term.freq <- rowSums(as.matrix(tdm))

term.freq <- subset(term.freq, term.freq >= 70)

df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)

ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +  xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with 'r'?

findAssocs(tdm, "r", 0.2)

# which words are associated with 'mining'?



findAssocs(tdm, "mining", 0.25)

#library(graph)
source("https://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("Rgraphviz")

library(Rgraphviz)

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

Word Cloud

m <- as.matrix(tdm)

# calculate the frequency of words and sort it by frequency

word.freq <- sort(rowSums(m), decreasing = T)

# colors

pal <- brewer.pal(9, "BuGn")

pal <- pal[-(1:4)]

# plot word cloud

library(wordcloud)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          
          random.order = F, colors = pal)

#Clustering

# remove sparse terms

tdm2 <- removeSparseTerms(tdm, sparse = 0.95)

m2 <- as.matrix(tdm2)

# cluster terms

distMatrix <- dist(scale(m2))

fit <- hclust(distMatrix, method = "ward")

plot(fit)
rect.hclust(fit, k = 6) 
# cut tree into 6 clusters research position university analysis network social

m3 <- t(m2) # transpose the matrix to cluster documents (tweets)

set.seed(122) # set a fixed random seed

k <- 6 # number of clusters

kmeansResult <- kmeans(m3, k)

round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k) {
  
  cat(paste("cluster ", i, ": ", sep = ""))
  
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  
  cat(names(s)[1:5], "\n")
  
  # print the tweets of every cluster
  
  # print(tweets[which(kmeansResult£cluster==i)])
  
}

## cluster 1: data mining r analysis big

## cluster 2: data mining r book example

## cluster 3: network analysis social r position

## cluster 4: package mining slides university analysis

## cluster 5: r example package slides use

## cluster 6: research position data university big

#Topic Modelling

dtm <- as.DocumentTermMatrix(tdm)

library(topicmodels)

lda <- LDA(dtm, k = 8) # find 8 topics

(term <- terms(lda, 6)) # first 6 terms of every topic

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)

topic <- topics(lda, 1)

topics <- data.frame(date=as.IDate(tweets.df$created), topic)

qplot(date, ..count.., data=topics, geom="density",fill=term[topic], position="stack")


tweets <- searchTwitter("#Elecciones2015",n=3000)

tweetdataframe <- do.call("rbind",lapply(tweets,as.data.frame))

write.table(tweetdataframe,"c:/users/paras/desktop/tweetsaboutexcel.txt",sep="\t")

#tweets <- userTimeline("RDataMining", n = 3200)
load(file = "rdmTweets-201306.RData")
"rdmTweets"
tweets<-rdmTweets

tweets<-rdmTweets
testtw.df <- twListToDF(rdmTweets)

View(testtw.df)
View(tweets.df)

length(tweets)



rdmTweets
library(swirl)
swirl()
F
10

#borra todas los objetos del entorno
rm(list=ls())
