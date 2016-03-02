#Jeremiah Crowley 
#Assignment 3

library(qdap)

pause <- function ()
{
  cat("Pause. Press <Enter> to continue...")
  readline()
  invisible()
}

findFrequentWords <- function(){
  #Get Data
  ebolaData = read.csv("/Users/user/Downloads/socMed-wdcount/Ebola_frequencies.csv", header = TRUE)
  fergusonData = read.csv("/Users/user/Downloads/socMed-wdcount/IfTheyGunnedMeDown_frequencies.csv", header = TRUE)
  baselineData = read.csv("/Users/user/Downloads/socMed-wdcount/USTop10Cities_frequencies.csv", header = TRUE)
  
  ebolaData <- as.data.frame(ebolaData)
  fergusonData <- as.data.frame(fergusonData)
  baselineData <- as.data.frame(baselineData)
    
  ebolaRowSums <- rowSums(ebolaData[, c(2, 3, 4, 5)])
  fergusonRowSums <- rowSums(fergusonData[, c(2, 3, 4, 5)])
  baselineRowSums <- rowSums(baselineData[, c(2, 3, 4, 5)])

  ebolaData$TotalFrequency <- as.numeric(as.character(ebolaRowSums))
  fergusonData$TotalFrequency <- as.numeric(as.character(fergusonRowSums))
  baselineData$TotalFrequency <- as.numeric(as.character(baselineRowSums))
  
  ebolaData <- ebolaData[order(-ebolaData$TotalFrequency),] 
  fergusonData <- fergusonData[order(-fergusonData$TotalFrequency),]
  baselineData <- baselineData[order(-baselineData$TotalFrequency),]
  
  returnList <- list(ebolaData, fergusonData, baselineData)
    
  return(returnList)
}

highestOccuringWords <- function(baselineDataFrame, ebolaDataFrame, fergusonDataFame){
  
  #2000 highest words in each dataframe
  baseline <- as.character(baselineDataFrame[1:2000,1])
  ebola <- as.character(ebolaDataFrame[1:2000,1])
  ferguson <- as.character(fergusonDataFrame[1:2000,1])
  
  ebolaMatch <- as.integer(0)
  fergusonMatch <- as.integer(0)
  
  for(i in 1:2000){
    for(j in 1:2000){
      if(baseline[i]==ebola[j]){
        ebolaMatch <- ebolaMatch + 1
      }
      if(baseline[i]==ferguson[j]){
        fergusonMatch <- fergusonMatch + 1
      }
    }
  }

  z <- matrix(c(fergusonMatch,ebolaMatch, (2000-fergusonMatch), (2000-ebolaMatch) ), nrow=2,ncol=2)
  
  #Based on top 2000 tweets, ferguson was mentioned 1.2x more than ebola
  print('Ferguson mentioned 1.2x more than Ebola')
  print(fisher.test(z, alter="greater"))
}

dynamics <- function(baseline,ebola,ferguson){
  
  #Sort data based on week one
  baselineWk1 <- baseline[order(-baseline$X2014.10.31),] 
  ebolaWk1 <- ebola[order(-ebola$X2014.10.06),] 
  fergusonWk1 <- ferguson[order(-ferguson$X2014.10.01),] 
  
  baselineWk1 <- as.character(baselineWk1[1:100,1])
  ebolaWk1 <- as.character(ebolaWk1[1:100,1])
  fergusonWk1 <- as.character(fergusonWk1[1:100,1])
  
  #Sort data based on week two
  baselineWk2 <- baseline[order(-baseline$X2014.11.07),] 
  ebolaWk2 <- ebola[order(-ebola$X2014.10.13),] 
  fergusonWk2 <- ferguson[order(-ferguson$X2014.10.08),] 
  
  baselineWk2 <- as.character(baselineWk2[1:100,1])
  ebolaWk2 <- as.character(ebolaWk2[1:100,1])
  fergusonWk2 <- as.character(fergusonWk2[1:100,1])
  
  #Sort data based on week three
  baselineWk3 <- baseline[order(-baseline$X2014.11.14),] 
  ebolaWk3 <- ebola[order(-ebola$X2014.10.20),] 
  fergusonWk3 <- ferguson[order(-ferguson$X2014.10.15),] 
  
  baselineWk3 <- as.character(baselineWk3[1:100,1])
  ebolaWk3 <- as.character(ebolaWk3[1:100,1])
  fergusonWk3 <- as.character(fergusonWk3[1:100,1])
  
  #Sort data based on week four
  baselineWk4 <- baseline[order(-baseline$X2014.11.21),] 
  ebolaWk4 <- ebola[order(-ebola$X2014.10.27),] 
  fergusonWk4 <- ferguson[order(-ferguson$X2014.10.22),] 
  
  baselineWk4 <- as.character(baselineWk4[1:100,1])
  ebolaWk4 <- as.character(ebolaWk4[1:100,1])
  fergusonWk4 <- as.character(fergusonWk4[1:100,1])
  
  baselineMatchCount <- 0
  ebolaMatchCount <- 0
  fergusonMatchCount <- 0
  
  for(i in 1:100){
    for(j in 1:100){
      if((baselineWk1[i]==baselineWk4[j]) || (baselineWk1[i]==baselineWk2[j]) || (baselineWk1[i]==baselineWk3[j])){
        baselineMatchCount = baselineMatchCount + 1
      }
      if((ebolaWk1[i]==ebolaWk4[j]) || (ebolaWk1[i]==ebolaWk2[j]) || (ebolaWk1[i]==ebolaWk3[j])){
        ebolaMatchCount = ebolaMatchCount + 1
      }
      if((fergusonWk1[i]==fergusonWk4[j]) || (fergusonWk1[i]==fergusonWk2[j]) || (fergusonWk1[i]==fergusonWk3[j])){
        fergusonMatchCount = fergusonMatchCount + 1
      }
    }
  }
  
  baselineData <- matrix(c(101,baselineMatchCount+1,1,(101-baselineMatchCount)),2)
  ebolaData <- matrix(c(101,ebolaMatchCount+1,1,(101-ebolaMatchCount)),2)
  fergusonData <- matrix(c(101,fergusonMatchCount+1,1,(101-fergusonMatchCount)),2)
  
  #315x less likely to appear in weeks 2-4
  print('Baseline: 315% Drop')
  print(fisher.test(baselineData,alter='greater'))
  
  #551x less likely to appear in weeks 2-4
  print('Ebola: 551% Drop')
  print(fisher.test(ebolaData,alter='greater'))
  
  #373x less likely to appear in weeks 2-4
  print('Ferguson: 373% Drop')
  print(fisher.test(fergusonData,alter='greater'))
}

polarityChange <- function(baseline,ebola,ferguson){  
  
  #Sort data based on week one
  baselineWk1 <- baseline[order(-baseline$X2014.10.31),] 
  ebolaWk1 <- ebola[order(-ebola$X2014.10.06),] 
  fergusonWk1 <- ferguson[order(-ferguson$X2014.10.01),] 
  
  baselineWk1 <- as.character(baselineWk1[1:100,1])
  ebolaWk1 <- as.character(ebolaWk1[1:100,1])
  fergusonWk1 <- as.character(fergusonWk1[1:100,1])
  
  #Sort data based on week two
  baselineWk2 <- baseline[order(-baseline$X2014.11.07),] 
  ebolaWk2 <- ebola[order(-ebola$X2014.10.13),] 
  fergusonWk2 <- ferguson[order(-ferguson$X2014.10.08),] 
  
  baselineWk2 <- as.character(baselineWk2[1:100,1])
  ebolaWk2 <- as.character(ebolaWk2[1:100,1])
  fergusonWk2 <- as.character(fergusonWk2[1:100,1])
  
  #Sort data based on week three
  baselineWk3 <- baseline[order(-baseline$X2014.11.14),] 
  ebolaWk3 <- ebola[order(-ebola$X2014.10.20),] 
  fergusonWk3 <- ferguson[order(-ferguson$X2014.10.15),] 
  
  baselineWk3 <- as.character(baselineWk3[1:100,1])
  ebolaWk3 <- as.character(ebolaWk3[1:100,1])
  fergusonWk3 <- as.character(fergusonWk3[1:100,1])
  
  #Sort data based on week four
  baselineWk4 <- baseline[order(-baseline$X2014.11.21),] 
  ebolaWk4 <- ebola[order(-ebola$X2014.10.27),] 
  fergusonWk4 <- ferguson[order(-ferguson$X2014.10.22),] 
  
  baselineWk4 <- as.character(baselineWk4[1:100,1])
  ebolaWk4 <- as.character(ebolaWk4[1:100,1])
  fergusonWk4 <- as.character(fergusonWk4[1:100,1])
  
  #Polarity change in ferguson
  fergusonWk1Pol <- polarity(fergusonWk1)$group$ave.polarity
  fergusonWk2Pol <- polarity(fergusonWk2)$group$ave.polarity
  fergusonWk3Pol <- polarity(fergusonWk3)$group$ave.polarity
  fergusonWk4Pol <- polarity(fergusonWk4)$group$ave.polarity
  
  #Polarity change in ebola
  ebolaWk1Pol <- polarity(ebolaWk1)$group$ave.polarity
  ebolaWk2Pol <- polarity(ebolaWk2)$group$ave.polarity
  ebolaWk3Pol <- polarity(ebolaWk3)$group$ave.polarity
  ebolaWk4Pol <- polarity(ebolaWk4)$group$ave.polarity
  
  #Polarity change in baseline
  baselineWk1Pol <- polarity(baselineWk1)$group$ave.polarity
  baselineWk2Pol <- polarity(baselineWk2)$group$ave.polarity
  baselineWk3Pol <- polarity(baselineWk3)$group$ave.polarity
  baselineWk4Pol <- polarity(baselineWk4)$group$ave.polarity

  #Ferguson sentiment started very negative, got a little more positive, neutral, then back to very negative
  barplot(cbind(fergusonWk1Pol,fergusonWk2Pol,fergusonWk3Pol,fergusonWk4Pol), xlab="Week", ylab="Sentiment")
  
  #Ebola sentiment started negative, stayed the same the second week, then changed to positive, then lastly went very negative
  barplot(cbind(ebolaWk1Pol,ebolaWk2Pol,ebolaWk3Pol,ebolaWk4Pol), xlab="Week", ylab="Sentiment")
  
  #Baseline sentiment started off negative, change to very positive, then neutral, and lastly negative again
  barplot(cbind(baselineWk1Pol,baselineWk2Pol,baselineWk3Pol,baselineWk4Pol), xlab="Week", ylab="Sentiment")
}

main <- function(baselineDataFrame,ebolaDataFrame,fergusonDataFrame){
  
  print('Words: Ebola or Ferguson mentioned more in baseline tweets?')
  pause()
  
  #Words: Ebola or Ferguson mentioned more in baseline tweets? 
  highestOccuringWords(baselineDataFrame, ebolaDataFrame, fergusonDataFrame)
  
  print('Dynamics: Top 100 words in week one in other weeks?')
  pause()
  
  #Dynamics: Top 100 words in week one in other weeks? 
  dynamics(baselineDataFrame, ebolaDataFrame, fergusonDataFrame)
  
  print('Polarity: Polarity change from week one to week four')
  pause()
  
  #Polarity: Polarity change from week one to week four 
  polarityChange(baselineDataFrame, ebolaDataFrame, fergusonDataFrame)
}

mostFrequent <- findFrequentWords()

ebolaDataFrame <- as.data.frame(mostFrequent[1])
fergusonDataFrame <- as.data.frame(mostFrequent[2])
baselineDataFrame <- as.data.frame(mostFrequent[3])

main(baselineDataFrame,ebolaDataFrame,fergusonDataFrame)