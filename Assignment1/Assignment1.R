## Jeremiah Crowley
## Homework 1

checkSimilarities <- function(data){
  #Variables for least and most similar correlation and their respective indexes
  results <- list()
  
  results$spearmanLeastSimilar <- 1
  results$spearmanLeastSimilarIndex <- 0
  results$spearmanMostSimilar <- -1
  results$spearmanMostSimilarIndex <- 0
  
  results$spearmanCorreleations <- numeric(42)
  results$pearsonCorreleations <- numeric(42)
  
  results$pearsonLeastSimilar <- 1
  results$pearsonLeastSimilarIndex <- 0
  results$pearsonMostSimilar <- -1
  results$pearsonMostSimilarIndex <- 0

  #Loop through rows of data
  for(i in 1:42){
    a <- as.numeric(data[40,8:21])
    b <- as.numeric(data[i,8:21])
    
    #Compute spearman correlation
    spearman <- max(cor(a,b, method = "spearman"))
    
    #Compute pearson correlation
    pearson <- max(cor(a,b, method = "pearson"))
    
    results$spearmanCorrelations[i] <- spearman
    results$pearsonCorrelations[i] <- pearson

    #Don't want to match with myself
    if(i != 40){      
      #Check if smaller than leastSimilar
      if(spearman < results$spearmanLeastSimilar){
        #If smaller, change respective variables
        results$spearmanLeastSimilar <- spearman
        results$spearmanLeastSimilarIndex <- i
      }
      #Do again for mostSimilar
      if(spearman > results$spearmanMostSimilar){
        results$spearmanMostSimilar <- spearman
        results$spearmanMostSimilarIndex <- i
      }
      
      #Check if smaller than leastSimilar
      if(pearson < results$pearsonLeastSimilar){
        #If smaller, change respective variables
        results$pearsonLeastSimilar <- pearson
        results$pearsonLeastSimilarIndex <- i
      }
      #Do again for mostSimilar
      if(pearson > results$pearsonMostSimilar){
        results$pearsonMostSimilar <- pearson
        results$pearsonMostSimilarIndex <- i
      }
    }
  }
  
  return(results)
}

highlight <- function(x, value, col.value, col=NA, ...){
  hst <- hist(x, ...)
  idx <- findInterval(value, hst$breaks)
  cols <- rep(col, length(hst$counts))
  cols[idx] <- col.value
  hist(x, col=cols, ...)
}

#Get data
bg.surv <- read.csv( file = "/Users/user/Desktop/big-data-survey-2014-fall-interests.csv")
rownames( bg.surv ) <- bg.surv[,1]

#Get names
names <- rownames(bg.surv)
#Run checkSimilarities function
x <- checkSimilarities(bg.surv)

#Print results
cat("Pearson Most Similar -> ",names[x$pearsonMostSimilarIndex], "Spearman Most Similar -> ",names[x$spearmanMostSimilarIndex])
cat("Pearson Least Similar -> ",names[x$pearsonLeastSimilarIndex], "Spearman Least Similar -> ",names[x$spearmanLeastSimilarIndex])

spearmanCorrelation <- x$spearmanCorrelations

#Plot histogram of correlations highlighting the most similar person
highlight(spearmanCorrelation,x$spearmanMostSimilar,"red")