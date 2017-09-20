library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)

folder.path="../data/InauguralSpeeches/"
speeches=list.files(path = folder.path, pattern = "*.txt")
prez.out=substr(speeches, 6, nchar(speeches)-4)

length.speeches=rep(NA, length(speeches))
ff.all<-Corpus(DirSource(folder.path))


ff.all<-tm_map(ff.all, stripWhitespace)
ff.all<-tm_map(ff.all, content_transformer(tolower))
ff.all<-tm_map(ff.all, removeWords, stopwords("english")) # remove words that is only important to the grammar structure
ff.all<-tm_map(ff.all, removeWords, character(0))
ff.all<-tm_map(ff.all, removePunctuation)
tdm.all<-TermDocumentMatrix(ff.all) 
# didn't do stemming because we are doing word cloud
tdm.tidy=tidy(tdm.all) # convert to tidy data frame
tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))

wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
          scale=c(5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))
# identify words that is only important to a particular document, conditional on the frequency to other documents

dtm <- DocumentTermMatrix(ff.all,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
# TfIdf: term frequency in a specific piece of speech
                                                     stopwords = TRUE))
ff.dtm=tidy(dtm)
#ff.all<-tm_map(ff.all, stemDocument)

for(i in 1:length(speeches)){
#  #crude=stemDocument(ff.all[[i]])
#  crude=Corpus(VectorSource(ff.all[[i]]))
#  tdm <- TermDocumentMatrix(crude[1], list(wordLengths=c(3, Inf)))
#  m <- as.matrix(tdm)
#  v <- sort(rowSums(m),decreasing=TRUE)
#  d <- data.frame(word = names(v),freq=v)
  
  png(paste("../output/", prez.out[i], ".png", sep=""),
      width=300, height=300)
  wordcloud(ff.dtm$term[ff.dtm$document==speeches[i]],
            ff.dtm$count[ff.dtm$document==speeches[i]],
              scale=c(5,0.5),
              max.words=200,
              min.freq=1,
              random.order=FALSE,
              rot.per=0,
              use.r.layout=FALSE,
              random.color=FALSE,
              colors=brewer.pal(10,"Blues"), 
            main=prez.out[i])
  dev.off()
  
  }
                                                     
 # data needs to be perserved, but output can always be deleted and start fresh.                                                     
