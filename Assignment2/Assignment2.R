## Jeremiah Crowley
## Homework 2

require(lasso2)

modelSelection <- function(predictors, genes, index){
  genes <- as.data.frame(genes)
  predictors <- as.vector(predictors)  
  genesCopy <- genes
  
  #If the index is a predictor, then remove from the genes matrix
  if(class(index) == "character"){
    a <- any(colnames(genes) == index)
    if(a == TRUE){
      indexNumber <- grep(index, colnames(genes))
      genesCopy <- genesCopy[-indexNumber]
    }
  } 
  else{
    colIndex <- colnames(genes)
    colName <- colIndex[index]
    
    a <- any(colnames(genes) == colIndex[index])
    if(a == TRUE){
      indexNumber <- grep(colName, colnames(genes))
      genesCopy <- genesCopy[-indexNumber]
    }
  }
  
  #Convert to matrix
  genesCopy <-do.call(cbind, genesCopy)
  
  bestBound <- Inf
  lowestMeanResiduals <- Inf
  
  #Compute l1ce while changing bounds
  for(bounds in seq(0.01, 1.0, 0.05)){
    l1ceResults <- l1ce(genes[,index]~genesCopy, sweep.out = ~1, standardize = TRUE, bound = bounds, absolute.t = FALSE)
    meanResiduals <- mean(residuals(l1ceResults) **2)
    
    #Get lowest residual, which is best bound
    if( (!is.nan(meanResiduals)) && (meanResiduals<lowestMeanResiduals)){
      bestBound <- bounds
      lowestMeanResiduals <- meanResiduals
    }
  }
  
  #Best Model
  bestModel <- l1ce(genes[,index]~genesCopy, sweep.out = ~1, standardize = TRUE, bound = bestBound, absolute.t = FALSE) 
}

#Matrix of predictor genes and its values
manipulateMatrix <- function(matrix,predictors){
  List <- list()
  namesMatrix <- names(matrix)
  
  for(i in 1:length(predictors))
  {
      for(j in 1:length(namesMatrix)){
          if(predictors[i] == namesMatrix[j]){
            name <- namesMatrix[j]
            normF <- matrix[,j]
            List[[name]] <- normF
          }
      }
  }
  
  Matrix = do.call(cbind, List)
  rownames(Matrix) <- rownames(matrix)
  
  return(Matrix)
}

#Get list of predictors without "TFs"
predictorsAsList <- function(predictorsTxtFile){
  listOfPredictors <- numeric(199)
  
  conn=file(predictorsTxtFile,open="r")
  linn=readLines(conn)
  for (i in 1:length(linn)){
    line=strsplit(linn[i],"\\ ") 
    listOfPredictors[i] <- line[[1]][1]
  }
  
  close(conn)  
  return(listOfPredictors)
}

#Get Data
predictors <- predictorsAsList("/Users/user/Downloads/baa.TFs.txt")
load("/Users/user/Downloads/baa.ratios.rda")

#Convert to Matrix
genes <- as.matrix(ratios)
#Transpose the matrix
genesTranspose <- as.data.frame(t(genes))
#Get of a matrix of just the predictors
manipulatedMatrix <- manipulateMatrix(genesTranspose,predictors)

#Run main method
bestModel <- modelSelection(predictors,manipulatedMatrix,155)
print(bestModel)