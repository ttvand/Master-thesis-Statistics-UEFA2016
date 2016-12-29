# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis"))

# Analysis parameters
nationalAnalysis <- FALSE
predictionPeriodYears <- 4
FIFAWeights <- TRUE
onlyPoisson <- TRUE

# Calculate the string parts
saveStringWeights <- ifelse(FIFAWeights,"FIFA","Equal")
poissonString <- ifelse(onlyPoisson,"Poisson","")
relaxationString <- "P"

# Path of the model results
targetModelFileNational <- paste0("predictProbsNat",predictionPeriodYears,
                                  saveStringWeights,"Weights",poissonString,
                                  relaxationString,".RData")
targetModelFilePL <- file.path(basePath,"PL analysis","mergedOdds.RData")

if(nationalAnalysis){
  # Load the model data
  load(targetModelFileNational)
  
  # Extract the columns of the preferred model
  halfPeriod <- 1300
  relaxation <- 0
  modelString <- paste0("probP_",halfPeriod,"_",relaxation,"_")
  outcome <- summary$result
  modelSummary <- summary[,grepl(modelString,names(summary))]
  rm(summary)
  modelSummary$A <- 1-rowSums(modelSummary)
  names(modelSummary) <- c("pH","pD","pA")
  modelSummary$outcome <- outcome
} else{
  # Load the model data
  load(targetModelFilePL)
  summary <- merged
  rm(merged)
  
  # Exlclude the last two matches
  summary <- summary[summary$homeLeft>2 & summary$awayLeft>2,]
  
  # Extract the columns of the preferred model
  halfPeriod <- 300
  relaxation <- 0.015
  modelString <- paste0("probP_",halfPeriod,"_",relaxation,"_")
  outcome <- summary$outcome
  modelSummary <- summary[,grepl(modelString,names(summary))]
  rm(summary)
  modelSummary$A <- 1-rowSums(modelSummary)
  names(modelSummary) <- c("pH","pD","pA")
  modelSummary$outcome <- outcome
}

# Possible relaxation fraction constants in
# Approach two: Inflate pD and deflate pA given a certain constant
#               Fixed fraction of (pA+pD) to pD
inflateFracs <- seq(0,0.15*(2-nationalAnalysis),0.001)
nbFracs <- length(inflateFracs)
logLosses <- rep(NA,nbFracs)

# Loop over all the inflated fractions and calculate the according log losses
for(i in 1:nbFracs){
  frac <- inflateFracs[i]
  
  # Calculate the ids where there is a shift from pA to pD and from pH to pD
  pHDIds <- which(modelSummary$pA > modelSummary$pD &
                    modelSummary$pD > modelSummary$pH)
  pADIds <- which(modelSummary$pH > modelSummary$pD &
                    modelSummary$pD > modelSummary$pA)
  
  # Initialize model probabilities to the fitted model
  pH <- modelSummary$pH
  pD <- modelSummary$pD
  pA <- modelSummary$pA
  
  # Handle cases where pH>pD>pA
  ADProbDif <- modelSummary$pD[pADIds]-modelSummary$pA[pADIds]
  ADShift <- ADProbDif*frac
  pD[pADIds] <- pD[pADIds]+ADShift
  pA[pADIds] <- pA[pADIds]-ADShift
  
  # Handle cases where pA>pD>pH
  HDProbDif <- modelSummary$pD[pHDIds]-modelSummary$pH[pHDIds]
  HDShift <- HDProbDif*frac
  pD[pHDIds] <- pD[pHDIds]+HDShift
  pH[pHDIds] <- pH[pHDIds]-HDShift
  
  # Calculate the average log loss
  outcomeProb <- ifelse(modelSummary$outcome=="H",pH,
                        ifelse(modelSummary$outcome=="D",pD,pA))
  
  logLosses[i] <- -mean(log(outcomeProb))
}

# Plot the log losses versus the relaxation fraction constants
plot(inflateFracs,logLosses)
cat("Performance increase of",round((1-min(logLosses)/logLosses[1])*100,3),
    "percent")