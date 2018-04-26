# import library
library(NLP)
library(tm)
library(stats)
library(proxy)
library(dplyr)
library(readtext)
library(jiebaRD)
library(jiebaR)
library(slam)
library(Matrix)
library(tidytext)

# Open the text file of President’s New Year’s Day Message
# (from The 86th year of the Republic Era to 105th)
 
rawData = readtext("*.txt")
docs = Corpus(VectorSource(rawData$text))

# data cleaning : remove Punctuation, Numbers, Whitespace anf Eng.
# I found that using RemovePunctuation function will cause some word garble. 
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
})
docs <- tm_map(docs, toSpace, "、")
docs <- tm_map(docs, toSpace, "，")
docs <- tm_map(docs, toSpace, "。")
docs <- tm_map(docs, toSpace, "！")
docs <- tm_map(docs, toSpace, "「")
docs <- tm_map(docs, toSpace, "（")
docs <- tm_map(docs, toSpace, "」")
docs <- tm_map(docs, toSpace, "）")
docs <- tm_map(docs, toSpace, "\n")
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, stripWhitespace)

# words cut 
mixseg = worker()

jieba_tokenizer = function(d){
  unlist(segment(d[[1]], mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))

d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus)
tf <- as.matrix(tdm)
DF <- tidy(tf)
# Take a look of DF
head(DF,10)

# tf-idf computation
N = tdm$ncol
tf <- apply(tdm, 2, sum)
idfCal <- function(word_doc)
{ 
  log2( N / nnzero(word_doc) ) 
}
idf <- apply(tdm, 1, idfCal)

doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}

findZeroId = as.matrix(apply(doc.tfidf, 1, sum))
tfidfnn = doc.tfidf[-which(findZeroId == 0),]
head(tfidfnn,10)
write.csv(tfidfnn, "show.csv")

# Data Visualization
# Word TF-IDF frequencics
freq=rowSums(as.matrix(tfidfnn))
tail(freq,10)
plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

# Term frequencics 

library(ggplot2)
high.freq=tail(sort(freq),n=20)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
tail(sort(freq),n=20)


ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")


x <- tail(sort(freq),n=20)
for(i in 1:length(DF$.rownames)){
  if(DF$.rownames[i]=="台灣"){
    data <- (DF[i,])
  }
}
for(i in 1:length(DF$.rownames)){
  if(DF$.rownames[i]=="臺灣"){
    data2 <- (DF[i,])
  }
}
a <- rbind(data,data2)

colnames(a) <- c('XD',88:105)

barplot(as.matrix(a[2:19]),
        main="臺灣與台灣字詞分布",
        xlab='年份',
        col=topo.colors(2)
        ) 

legend("topleft", legend=a$XD,
       col=topo.colors(2), inset=.02,horiz=TRUE, cex=0.6,box.col="green")
ql

