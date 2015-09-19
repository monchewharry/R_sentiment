library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(base64enc)

# 1. dev.twitter.com/app
consumer_key <- "xxxx"
consumer_secret <- 'xxxx'
access_token <- 'xxxx'
access_secret <- 'xxxx'

setup_twitter_oauth(consumer_key,consumer_secret, access_token, access_secret)

jurassic = searchTwitter("Jurassic World", n=1500, lang="en")
antman = searchTwitter("Ant Man", n=1500, lang="en")
minions = searchTwitter("Minions", n=1500, lang="en")
terminator = searchTwitter("Terminator Genisys", n=1500, lang="en")
class(jurassic)


# 2. clean the text
jurassic_txt = sapply(jurassic, function(x) x$getText())
antman_txt = sapply(antman, function(x) x$getText())
minions_txt = sapply(minions, function(x) x$getText())
terminator_txt = sapply(terminator, function(x) x$getText())

clean.tweets <- function(some_txt) {
  # &is HTML for "Start of a character reference"
  # &amp is the character reference for "An ampersand".
  some_txt = gsub("&amp", "", some_txt)
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

# clean an  remove stop words

jurassic_txt <- clean.tweets(jurassic_txt)
jurassic_txt = removeWords(jurassic_txt, stopwords("english"))
jurassic_txt = jurassic_txt[!is.na(jurassic_txt)]

antman_txt <- clean.tweets(antman_txt)
antman_txt = removeWords(antman_txt, stopwords("english"))
antman_txt = antman_txt[!is.na(antman_txt)]

minions_txt <- clean.tweets(minions_txt)
minions_txt = removeWords(minions_txt, stopwords("english"))
minions_txt = minions_txt[!is.na(minions_txt)]

terminator_txt <- clean.tweets(terminator_txt)
terminator_txt = removeWords(terminator_txt, stopwords("english"))
terminator_txt = terminator_txt[!is.na(terminator_txt)]

# 3.Sentiment Analysis using R  

# 3.1 classify emotion
## The classifier is a Naive Bayes classifier trained on Carlo Strapparava and Alessandro Valitutti’s emotions lexicon. 
## A description can be found http://www.inside-r.org/packages/cran/sentiment/docs/classify_emotion  

class_emo_jurassic = classify_emotion(jurassic_txt,algorithm="bayes",prior=1.0)
class_emo_antman = classify_emotion(antman_txt,algorithm="bayes",prior=1.0)
class_emo_minions =classify_emotion(minions_txt,algorithm="bayes",prior=1.0)
class_emo_terminator = classify_emotion(terminator_txt,algorithm="bayes",prior=1.0)


# get 1500 emotion best fit for each movies
emo_jurassic = class_emo_jurassic[,7]
emo_antman = class_emo_antman[,7]
emo_minions = class_emo_minions[,7]
emo_terminator = class_emo_terminator[,7]

# substitute NA's by "unknown"
emo_jurassic[is.na(emo_jurassic)] = "unknown"
emo_antman[is.na(emo_antman)] = "unknown"
emo_minions[is.na(emo_minions)] = "unknown"
emo_terminator[is.na(emo_terminator)] = "unknown"
count_jurassic<-table(emo_jurassic)
count_antman<-table(emo_antman)
count_minions<-table(emo_minions)
count_terminator<-table(emo_terminator)

colors = c("red", "green", "orange", "violet", "blue", "yellow", "grey")
barplot(count_jurassic, main="Emotion classification of tweets about Jurassic World", 
        xlab="Emotion", ylab="Counts", col=colors)
#Same thing for other movies
barplot(count_antman, main="Emotion classification of tweets about antman", 
        xlab="Emotion", ylab="Counts", col=colors)
barplot(count_minions, main="Emotion classification of tweets about minions", 
        xlab="Emotion", ylab="Counts", col=colors)
barplot(count_terminator,main="Emotion classification of tweets about terminator", 
        xlab="Emotion", ylab="Counts", col=colors)  

# 4. polarity classification  
## the overall polarity of the tweets
class_pol_jurassic = classify_polarity(jurassic_txt, algorithm="bayes")
class_pol_antman = classify_polarity(antman_txt,algorithm="bayes")
class_pol_minions = classify_polarity(minions_txt,algorithm="bayes")
class_pol_terminator = classify_polarity(terminator_txt, algorithm="bayes")

# get polarity best fit
pol_jurassic = class_pol_jurassic[,4]
pol_antman = class_pol_antman[,4]
pol_minions = class_pol_minions[,4]
pol_terminator = class_pol_terminator[,4]

# data frame with results
df_jurassic = data.frame(text=jurassic_txt, emotion=emo_jurassic,polarity=pol_jurassic, stringsAsFactors=FALSE)
df_antman = data.frame(text=antman_txt, emotion=emo_antman,polarity=pol_antman, stringsAsFactors=FALSE)
df_minions = data.frame(text=minions_txt, emotion=emo_minions,polarity=pol_minions, stringsAsFactors=FALSE)
df_terminator = data.frame(text=terminator_txt, emotion=emo_terminator, polarity=pol_terminator, stringsAsFactors=FALSE)

count_jurassic <- data.frame(table(df_jurassic$polarity))
count_antman <- data.frame(table(df_antman$polarity))
count_minions <- data.frame(table(df_minions$polarity))
count_terminator <- data.frame(table(df_terminator$polarity))

all <- data.frame(count_jurassic$Freq, count_antman$Freq,
                     count_minions$Freq, count_terminator$Freq)
colnames(all) <- c("Jurassic World", "Ant Man", "Minions", "Terminator Genysis")
barplot(as.matrix(all), main="Polarity of tweets", ylab = "Polarity",
        cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=c("blue", "grey", "violet"), legend = c("Negative", "Neutral", "Positive"), ylim=c(0,1500))
# 5. word cloud  
# sort emotion's levels
df_jurassic = within(df_jurassic, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
df_antman = within(df_antman, emotion <- factor(emotion,levels=names(sort(table(emotion), decreasing=TRUE))))
df_minions = within(df_minions, emotion <- factor(emotion,levels=names(sort(table(emotion), decreasing=TRUE))))
df_terminator = within(df_terminator, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

 
word_cloud<-function(df_movie){
  # separating text by emotion into a vector:emo.doc
  emos = levels(factor(df_movie$emotion))
  nemo = length(emos)
  emo.docs = rep("", nemo)
  for (i in 1:nemo){
    tmp = df_jurassic$text[df_jurassic$emotion == emos[i]]
    emo.docs[i] = paste(tmp, collapse=" ")
  }
  
  
  # create corpus
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  
  # comparison word cloud
  comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                   scale = c(2.0,.5), random.order = FALSE, title.size = 1.5)
}
word_cloud(df_jurassic)
word_cloud(df_minions)
