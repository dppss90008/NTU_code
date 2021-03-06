## 利用FacebookAPI-從靠北中興收集發文資料
library(httr)
token  = "EAACEdEose0cBAJnAZCSVG9I1hEhzrtJvqB2Nv6qX6c9QQQty1PbyYivIKSZAZA96ZAZC3pkUafpMewZBYLBzIhHjm6Bn62ySpPBOWkbooLvpkDrnspHZC1vMkqN9ZCgUxPmdCazWBZA0EsfiuM0kRAIR7d8LkwbybpZCnW6IYxMTnZCBODB4GdcIrDZB0ZA8d7U0s2pUZD"
prefex = "https://graph.facebook.com/v2.12/cowbaychunghsing/?fields=posts.limit(1000)&access_token="
url    = paste0(prefex, token)
res    = httr::GET(url)
posts  = httr::content(res)

library(magrittr)
DATA = posts$posts$data %>% do.call(rbind,.) %>% data.frame

## 資料清理 
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)

docs <- Corpus(VectorSource(DATA$message))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
  })

docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
docs <- tm_map(docs,toSpace, "#靠北中興")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

Sys.setlocale(category = "LC_ALL", locale = "cht")
mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))

# 清除單個字: Ex: 也，留下單個字以上 Ex: 不管，經過...
for(i in c(1:length(freqFrame$Var1))){
    if((freqFrame$Var1[i] %>% as.character %>% nchar) == 1){
      freqFrame[i,] <- NA
    }
    if(freqFrame$Freq[i]==1){
      freqFrame[i,] <-   }
}
freqFrame <- na.omit(freqFrame)


## 製作文字雲
library(RColorBrewer)
library(wordcloud)
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=10,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

