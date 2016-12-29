# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis"))

# Load the required libraries
library(caret)
library(MASS)
library(neuralnet)

# Analysis parameters
stackingApproach <- c("ANN","POLR")[2]
K <- 10
nationalAnalysis <- FALSE
predictionPeriodYears <- 4
FIFAWeights <- TRUE
onlyPoisson <- FALSE

# Calculate the string parts
saveStringWeights <- ifelse(FIFAWeights,"FIFA","Equal")
poissonString <- ifelse(onlyPoisson,"Poisson","")
relaxationString <- ifelse(onlyPoisson,"P","")

# Path of the model results
targetModelFileNational <- paste0("predictProbsNat",predictionPeriodYears,
                                  saveStringWeights,"Weights",poissonString,
                                  relaxationString,".RData")
targetModelFilePL <- file.path(basePath,"PL analysis","mergedOdds.RData")

if(nationalAnalysis){
  # Load the model data
  load(targetModelFileNational)
  
  # Set the best model string and log loss
  bestModelString <- "probP_600_0"
  bestLogLoss <- -mean(log(summary[[bestModelString]]))
} else{
  # Load the model data
  load(targetModelFilePL)
  summary <- merged
  rm(merged)
  
  # Rename the outcome column
  names(summary)[names(summary)=="outcome"] <- "result"
  
  # Set the best model string and log loss
  bestModelString <- "probP_180_0.02"
  bestLogLoss <- -mean(log(summary[[bestModelString]]))
}


##############################################################################
# Stacking approach:                                                         #
#   1) Obtain different model probabilities for pH and pD                    #
#   2) Use K fold cross validation:                                          #
#         - Learn a POLR model using all data except the excluded fold       #
#         - Use the model to predict the probabilities for the excluded fold #
#   3) Compare the performance to the considered best individual model       #
##############################################################################

# Considered models:
# All half periods of 20 and 600
# all relaxations of 0 (adding others would lead to perfect multicollinearity)
# All 4 (nationalAnalysis) or 5 models
# A total of 8 or 10 models


# 1) Obtain different model probabilities for pH and pD

modelColumns <- grep("prob.*_.*_[^a-zA-Z]*$",names(summary))
halfPeriods <- sapply(names(summary)[modelColumns],function(x)
  as.numeric(unlist(strsplit(x,'_'))[2]))
relaxations <- sapply(names(summary)[modelColumns],function(x)
  as.numeric(unlist(strsplit(x,'_'))[3]))
consideredModelIds <- modelColumns[halfPeriods %in% c(20,600) &
                                   relaxations == 0]
consideredModels <- names(summary)[consideredModelIds]

# Extract the stacking model columns of interest
stackingCols <- paste0(rep(consideredModels,each=2),c("_H","_D"))
stackData <- summary[,stackingCols]
stackData$Result <- factor(summary$result)

# 2) Use K fold cross validation:                                          
#         - Learn a POLR model using all data except the excluded fold       
#         - Use the model to predict the probabilities for the excluded fold

# Set up the K folds
nbMatches <- nrow(summary)
folds <- createFolds(1:nbMatches,k=K)
predictedProbs <- rep(NA,nbMatches)

# Loop over all the folds, learn the stacked model and predict the excluded
# fold
for(i in 1:K){
  # Display the iteration
  cat("Fold",i,"of",K,"\n")
  
  # Subset on the specific fold
  foldIds <- folds[[i]]
  trainData <- stackData[-foldIds,]
  testData <- stackData[foldIds,]
  
  if(stackingApproach=="ANN"){
    n <- names(trainData)[-ncol(trainData)]
    formula <- as.formula(paste("Home + Draw ~", paste(n,collapse = " + ")))
    
    # Calculate the home and draw columns
    trainData$Home <- as.numeric(trainData$Result=="H")
    trainData$Draw <- as.numeric(trainData$Result=="D")
    
    # Fit the artificial neural network
    fitModel <- neuralnet(formula,data=trainData[1:50,],hidden=10,
                          linear.output=T)
    probs <- compute(fitModel, testData[,-ncol(testData)])$net.result
    rowMins <- apply(probs,1,min)
    probs <- 1
    
  } else{
    # Fit the POLR model
    fitModel <- polr(Result ~ ., data=trainData)
    
    # Use the model to predict the probabilities for the excluded fold
    modelPredict <- predict(fitModel,type="probs",newdata = testData)
  }
  
  # Extract and store the predicted probabilities
  predictedProbs[foldIds] <- ifelse(testData$Result=="A",modelPredict[,1],
                                         ifelse(testData$Result=="D",modelPredict[,2],
                                                modelPredict[,3]))
}

# Compare the performance to the considered best individual model
cat("Best single model log loss:",round(bestLogLoss,4),"\n")
stackedLogLoss <- -mean(log(predictedProbs))
cat("Stacked model log loss:",round(stackedLogLoss,4),"\n")