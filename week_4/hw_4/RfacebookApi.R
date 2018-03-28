
## 利用FacebookAPI-從靠北中興收集發文資料
library(httr)
token  = "EAACEdEose0cBAPSgCZA4LOp6DcbjhmW1WHYomFfLfBkVVMqcqPDokF62tbXVE2ZAh4gsFxqCpu75hIYcwZCBJV4rbcWxlZCRVYvPZBsuXpwylrpImZAQnIepWVeMsFe2ZCww6Wlfg20IlmBSrTVHEMiyN53zb4cHaD1jeoafJ55r24pSZBXEQKZAShZBProagtW6gZD"
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

## 製作文字雲
library(RColorBrewer)
library(wordcloud)
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=10,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

