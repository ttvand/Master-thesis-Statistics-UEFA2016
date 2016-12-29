# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/PL analysis/")

# Load the summary file
# summaryFile <- "predictCompSummary.RData"
# summaryFile <- "predictCompSummaryProbsL.RData"
summaryFile <- "predictCompSummaryProbsElo.RData"
load(summaryFile)

# Extract the decay and relaxation parameters for all columns of accSum
getPredictionSummary <- function(summary){
  predictionSummary <- NULL
  for(i in 1:ncol(summary)){
    method_name <- colnames(summary)[i]
    name_parts <- strsplit(method_name,"_")[[1]]
    if(length(name_parts)==3){
      if(name_parts[1]=="prob"){
        method <- "BT strengths"
      } else{
        if(name_parts[1]=="probHA"){
          method <- "BT home away strengths"
        } else{
          if(name_parts[1]=="probPC"){
            method <- "Poisson corr strengths"
          } else{
            if(name_parts[1]=="probP"){
              method <- "Poisson strengths"
            } else{
              if(name_parts[1]=="probGD"){
                method <- "BT goal difference strengths"
              } else{
                if(name_parts[1]=="probDA"){
                  method <- "Poisson defense attack strengths"
                } else{
                  if(name_parts[1]=="probElo"){
                    method <- "Elo ratings"
                  } else{
                    method <- "METHOD UNKNOWN"
                  }
                }
              }
            }
          }
        }
      }
      
      decay <- as.numeric(name_parts[2])
      relaxation <- as.numeric(name_parts[3])
      
      pH <- summary[,i+1]
      pD <- summary[,i+2]
      pA <- abs(1-summary[,i+1]-summary[,i+2])
      
      if(any(pA<0)){
        x <- 1
      }
      
      avReturn <- mean(1/summary[,i])
      avAcc <- mean(summary[,i])
      logLoss <- -mean(log(summary[,i]))
      maxProb <- pH
      maxProb[pD>pH] <- pD[pD>pH]
      maxProb[pA>maxProb] <- pA[pA>maxProb]
      majCorrect <- mean(abs(maxProb-summary[,i])<1e-12)
      predictionSummary <- rbind(predictionSummary,
                                 data.frame(method=method,decay=decay,
                                            relaxation=relaxation,
                                            avReturn=avReturn,
                                            avAcc=avAcc,
                                            majCorrect=majCorrect,
                                            logLoss=logLoss,
                                            stringsAsFactors=FALSE))
    }
  }
  predictionSummary
}

# Calculate the prediction summary
predictionSummary <- getPredictionSummary(summary)

# Return the best model
bestId <- which.min(predictionSummary$logLoss)
bestModel <- paste(predictionSummary[bestId,1],
                   predictionSummary[bestId,2],
                   predictionSummary[bestId,3])
cat("Best model:",bestModel)
