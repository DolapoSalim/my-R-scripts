library(wordcloud2)
library(scholar)
library(stringr)
library(tm)

scholar2cloud<-function(scientist,minlength=4){
  Profile<-get_profile(scientist)
  allpapers<-get_publications(scientist)
  words<-NULL
  for(i in 1:dim(allpapers)[1]){
    words<-c(words,str_split(allpapers[i,1]," ")[[1]])
  }
  words<-tolower(words)
  length_char<-sapply(words,nchar)
  words<-words[which(length_char>minlength)]
  docs <- Corpus(VectorSource(words))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  wordcloud2(d)
  return(d)
}

scholar2cloud("EyMe2b8AAAAJ&hl", minlength = 12) #LUCA RINDI

