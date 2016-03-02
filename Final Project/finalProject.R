# required pakacges
library(sentiment)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

pause <- function ()
{
  cat("Pause. Press <Enter> to continue...")
  readline()
  invisible()
}

removeExcess <- function(txt){
  
  txt <- as.data.frame(txt)
  
  result <- list()
  result$tweets <- character(nrow(txt))
  
  for(i in 1:nrow(txt)){
    # get tweet
    some_txt <- txt[i,2]
    # remove at people
    some_txt = gsub("@\\w+", "", some_txt)
    # remove retweet entities
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    # remove html links
    some_txt = gsub("http\\w+", "", some_txt)
    # remove punctuation
    some_txt = gsub("[[:punct:]]", "", some_txt)
    # remove numbers
    some_txt = gsub("[[:digit:]]", "", some_txt)
    # remove unnecessary spaces
    some_txt = gsub("[ \t]{2,}", "", some_txt)
    some_txt = gsub("^\\s+|\\s+$", "", some_txt)    
    # remove NAs in some_txt
    some_txt = some_txt[!is.na(some_txt)]
    names(some_txt) = NULL
    
    result$tweets[i] <- some_txt
  }
  
  return(result$tweets)
}

sentimentAnalysis <- function(tweets,title1,title2){
  tweets <- as.data.frame(tweets)
  
  # classify emotion
  classEmotion = classify_emotion(tweets, algorithm="bayes", prior=1.0)
  emotion = classEmotion[,7]
  emotion[is.na(emotion)] = "Not classified"
  
  # classify polarity
  classPolarity = classify_polarity(tweets, algorithm="bayes")
  polarity = classPolarity[,4]
  
  # data frame with results
  tweetsDataFrame = data.frame(text=tweets, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  tweetsDataFrame = within(tweetsDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  
  # plot distribution of emotions
  emotionGraph <- ggplot(tweetsDataFrame, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) + labs(title = title1, x="Emotions", y="Tweets") + theme(plot.title = element_text(size=12))
  
  # plot distribution of polarity
  sentimentGraph <- ggplot(tweetsDataFrame, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + labs(title = title2, x="Polarity", y="Tweets") + theme(plot.title = element_text(size=12))
  
  print(emotionGraph)
  pause()
  print(sentimentGraph)
  pause()
  
  # separating text by emotion
  emos = levels(factor(tweetsDataFrame$emotion))
  nemo = length(emos)
  emo.docs = rep("", nemo)
  for (i in 1:nemo){
    tmp = some_txt[emotion == emos[i],]
    emo.docs[i] = paste(tmp, collapse=" ")
  }
  
  emo.docs = removeWords(emo.docs, stopwords("english"))
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  
  # comparison word cloud
  comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),scale = c(3,.5), random.order = FALSE, title.size = 1.5)
}

tweets <- as.data.frame(removeExcess(read.csv("/Users/user/Desktop/tweetsForFinalProject15B.csv", header = TRUE)))
sentimentAnalysis(tweets,"Sentiment Analysis of Tweets 15min Before Kickoff\n(classification by emotion)","Sentiment Analysis of Tweets 15min Before Kickoff\n(classification by polarity)")

tweets <- as.data.frame(removeExcess(read.csv("/Users/user/Desktop/tweetsForFinalProjectQ1.csv", header = TRUE)))
sentimentAnalysis(tweets,"Sentiment Analysis of Tweets 1st Quarter\n(classification by emotion)","Sentiment Analysis of Tweets 1st Quarter\n(classification by polarity)")

tweets <- as.data.frame(removeExcess(read.csv("/Users/user/Desktop/tweetsForFinalProjectQ2.csv", header = TRUE)))
sentimentAnalysis(tweets,"Sentiment Analysis of Tweets 2nd Quarter\n(classification by emotion)","Sentiment Analysis of Tweets 2nd Quarter\n(classification by polarity)")

tweets <- as.data.frame(removeExcess(read.csv("/Users/user/Desktop/tweetsForFinalProjectQ3.csv", header = TRUE)))
sentimentAnalysis(tweets,"Sentiment Analysis of Tweets 3rd Quarter\n(classification by emotion)","Sentiment Analysis of Tweets 3rd Quarter\n(classification by polarity)")

tweets <- as.data.frame(removeExcess(read.csv("/Users/user/Desktop/tweetsForFinalProjectQ4.csv", header = TRUE)))
sentimentAnalysis(tweets,"Sentiment Analysis of Tweets 4th Quarter\n(classification by emotion)","Sentiment Analysis of Tweets 4th Quarter\n(classification by polarity)")

tweets <- as.data.frame(removeExcess(read.csv("/Users/user/Desktop/tweetsForFinalProject15A.csv", header = TRUE)))
sentimentAnalysis(tweets,"Sentiment Analysis of Tweets 15min After End Game\n(classification by emotion)","Sentiment Analysis of Tweets 15min After End Game\n(classification by polarity)")