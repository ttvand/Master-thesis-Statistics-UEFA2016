# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis"))

# Load required libraries
library(skellam) # Difference of two poisson distributions

# Simulation parameters
continueAnalysis <- TRUE
predictionPeriodYears <- 4
FIFAWeights <- TRUE
onlyPoisson <- FALSE
relaxationType <- c("probabilities","strengths")[1]

# Path to the maximum likelihood functions
mlPath <- paste0(basePath,"Common files/mlFunctions.R")

# Path to the national team match results
nationalDataPath <- paste0(basePath,"National analysis/Scraper/internationalResultsEuropeP.RData")

# General parameters
startPredictionDate <- as.Date(paste0(1992+predictionPeriodYears,"-01-01"))
# startPredictionDate <- as.Date("2016-01-01")

# Calculate the save string parts
saveStringWeights <- ifelse(FIFAWeights,"FIFA","Equal")
poissonString <- ifelse(onlyPoisson,"PoissonTest","")
relaxationString <- ifelse(relaxationType=="strengths","S","P")

# Save each saveEachLoops of match iterations
saveEachLoops <- 1

# Path to save the results
targetSaveFile <- paste0("predictProbsNat",predictionPeriodYears,
                         saveStringWeights,"Weights",poissonString,
                         relaxationString,".RData")

###########################################
# Section 1: Source the base ML functions #
###########################################

source(mlPath)


#############################################################
# Section 2: prediction function combining all ML functions #
#############################################################

predictNationalMatches <- function(startPredictionDate,predictionPeriodYears,
                                   halfPeriods,ks,targetSaveFile,
                                   continueAnalysis,
                                   methods = c("BT","BTGoalDiff",
                                               "Poisson","DefAttack"),
                                   onlyPoisson=FALSE){
  # Set the target methods
  if(onlyPoisson){
    methods <- "Poisson"
  }
  
  summary <- NULL
  nbKs <- length(ks)
  nbHalfPeriods <- length(halfPeriods)
  
  # Load the studied dataset
  load(nationalDataPath)
  data <- allMatches
  rm(allMatches)
  data <- data[order(data$Date),]
  
  # Overwrite match importance weights when not using the FIFA weights
  if(!FIFAWeights){
    data$impScore <- 1
  }
  
  # Calculate factorials beforehand to speed up the poisson ML estimation
  factorials <- factorial(0:max(c(data$hgoal,data$vgoal)))
  
  # Rename data columns
  names(data)[names(data)=="Home"] <- "home"
  names(data)[names(data)=="Away"] <- "visitor"
  
  # Add the result column
  data$result <- ifelse(data$hgoal==data$vgoal,"D",
                        ifelse(data$hgoal>data$vgoal,"H","A"))
  
  # Add the goaldif column
  data$goaldif <- data$hgoal-data$vgoal
  
  # Preprocess data
  teams <- unique(sort(c(data$home,data$visitor)))
  nb.teams <- length(teams)
  data$homeId <- match(data$home,teams)
  data$visitId <- match(data$visitor,teams)
  
  # Use saved summary as a starting point if continueAnalysis is true and
  # there are already processed matches in summary
  
  successfulLoad <- FALSE
  try({
    suppressWarnings(load(targetSaveFile))
    successfulLoad <- TRUE
  },silent=TRUE)
  
  if(continueAnalysis && successfulLoad){
    startPredictDate <- max(summary$Date)+1
  } else{
    summary <- NULL
    startPredictDate <- startPredictionDate
  }
  
  # Keep track of the iterations in order to save each saveEachLoops loops
  iteration <- 1
  
  # Initial values of the ML estimation 
  inits <<- NULL
  
  # Copy original data
  dataOrig <- data
  nDatarow <- nrow(data)
  
  while(TRUE){
    # Calculate the maximum date range that does not play the same teams twice.
    startIndex <- suppressWarnings(which(dataOrig$Date>=startPredictDate)[1])
    
    # Exit the infite loop when all matches have been processed
    if(is.na(startIndex)){
      break
    }
    
    intermediateNextDate <- dataOrig$Date[startIndex]
    endPredictDate <- dataOrig$Date[startIndex]
    
    playedTeams <- c(dataOrig$home[startIndex],dataOrig$visitor[startIndex])
    nextIndex <- startIndex+1
    
    # Split data from fixture to fixture and calculate predictions for future
    # matches. A fixture in the national team setting is defined as the maximum
    # date range that does not have the same team play twice.
    while(TRUE){
      if(nextIndex>nDatarow){
        break
      }
      if(any(c(dataOrig$home[nextIndex],dataOrig$visitor[nextIndex]) %in%
             playedTeams)){
        break
      } else{
        playedTeams <- c(playedTeams,
                         c(dataOrig$home[nextIndex],
                           dataOrig$visitor[nextIndex]))
        nextIndex <- nextIndex+1
        if(dataOrig$Date[nextIndex]>intermediateNextDate){
          endPredictDate <- intermediateNextDate
          intermediateNextDate <- dataOrig$Date[nextIndex-1]
        }
      }
    }
    
    # Display the processed date range and predicted matches count
    cat(iteration," - ","Processing date range ",
        as.character(startPredictDate)," - ",as.character(endPredictDate)," (",
        sum(dataOrig$Date>=startPredictDate & dataOrig$Date<=endPredictDate),
        ")","\n",sep="")
    
    # Calculate the modeling date range
    startModelDate <- as.POSIXlt(startPredictDate)
    startModelDate$year <- startModelDate$year-predictionPeriodYears
    startModelDate <- as.Date(startModelDate)
    data <- dataOrig[dataOrig$Date <= endPredictDate &
                       dataOrig$Date > startModelDate,]
    
    # startIndex: first index that is not used to train the models
    startIndex <- max(which(data$Date<startPredictDate))+1
    dataIds <- 1:(startIndex-1)
    
    fixId <- startIndex:nrow(data)
    data$days.passed <- as.numeric(difftime(max(
      data$Date[fixId]),data$Date,units="days"))
    
    # Generate the list to append to the summary file
    #     appendSummary <- list(
    #       Home=teams[data[fixId,"homeId"]],
    #       Away=teams[data[fixId,"visitId"]],
    #       Date=data[fixId,"Date"],
    #       # Result
    #       Outcome=data[fixId,"result"])
    appendSummary <- data[fixId,c(1:6,8:9,11:14,18)]
    appendSummary$Date <- as.Date(appendSummary$Date, format = "%m/%d/%Y")
    
    # Reset startPredictDate for the next iteration
    startPredictDate <- endPredictDate+1
    
    #####################################
    # Single team strength calculations #
    #####################################
    
    if("BT" %in% methods){
      for(j in 1:nbHalfPeriods){
        halfPeriod <- halfPeriods[j]
        
        # Calculate the ML estimates
        init <- doubleListLookup(inits,"BT",halfPeriod,nb.teams+2)
        tryCatch({
          opt.optim.MLBT <- optim(init,LBT,gr = NULL,
                                  data=data[dataIds,],
                                  halfPeriod=halfPeriod,method="BFGS")
        }, error=function(e){
          opt.optim.MLBT <<- optim(init,LBT,gr = NULL,
                                   data=data[dataIds,],
                                   halfPeriod=halfPeriod,method="CG")
        })
        
        inits[["BT"]][[as.character(halfPeriod)]] <<- opt.optim.MLBT$par
        
        # Calculate the numerator and denominator terms of all fixtures
        homeWin <- exp(opt.optim.MLBT$par[data$homeId[fixId]]+
                         opt.optim.MLBT$par[nb.teams+2])
        draw <- exp(opt.optim.MLBT$par[nb.teams+1])*
          sqrt(exp(opt.optim.MLBT$par[data$homeId[fixId]]+
                     opt.optim.MLBT$par[nb.teams+2]+
                     opt.optim.MLBT$par[data$visitId[fixId]]))
        homeLoss <- exp(opt.optim.MLBT$par[data$visitId[fixId]])
        denominatorProb <- homeWin+draw+homeLoss
        
        # Loop over the relaxation parameters
        for(k in 1:nbKs){
          
          # Calculate the outcome probabilities
          outcomeProbs <- cbind(homeWin/denominatorProb,
                                draw/denominatorProb,
                                homeLoss/denominatorProb)/(1+3*ks[k])+
            ks[k]/(1+3*ks[k])
          
          # Rescale the probabilities to the [0,1] interval with a sum of 1
          rowMins <- apply(outcomeProbs,1,min)
          outcomeProbs <- outcomeProbs-rowMins*(rowMins<0)
          outcomeProbs <- outcomeProbs/rep(rowSums(outcomeProbs),3)
          
          # Extract the predicted probability of the actual outcome
          outcomeProb <- outcomeProbs[1:nrow(outcomeProbs)+
                                        (
                                          1*as.numeric(data[fixId,"result"]=="D")+
                                            2*as.numeric(data[fixId,"result"]=="A")
                                        )*nrow(outcomeProbs)]
          
          appendSummary[[paste0("prob","_",halfPeriod,"_",ks[k])]] <- outcomeProb
          appendSummary[[paste0("prob","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbs[,1]
          appendSummary[[paste0("prob","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbs[,2]
        }
      }
    }
    
    ########################################
    # Home away team strength calculations #
    ########################################
    
    if("BTHA" %in% methods){
      for(j in 1:nbHalfPeriods){
        halfPeriod <- halfPeriods[j]
        # Calculate the ML estimates
        init <- doubleListLookup(inits,"BTHA",halfPeriod,nb.teams*2+1)
        tryCatch({
          opt.optim.MLHA <- optim(init,LBTHA,gr = NULL,
                                  data=data[dataIds,],
                                  halfPeriod=halfPeriod,method="BFGS")
        }, error=function(e){
          opt.optim.MLHA <<- optim(init,LBTHA,gr = NULL,
                                   data=data[dataIds,],
                                   halfPeriod=halfPeriod,method="CG")
        })
        
        inits[["BTHA"]][[as.character(halfPeriod)]] <<- opt.optim.MLHA$par
        
        # Calculate the numerator and denominator terms of all fixtures
        homeWin <- exp(opt.optim.MLHA$par[data$homeId[fixId]])
        draw <- exp(opt.optim.MLHA$par[nb.teams*2+1])*
          sqrt(exp(opt.optim.MLHA$par[data$homeId[fixId]]+
                     opt.optim.MLHA$par[nb.teams+data$visitId[fixId]]))
        homeLoss <- exp(opt.optim.MLHA$par[nb.teams+data$visitId[fixId]])
        
        # Calculate the geometric mean of the team strengths for matches on
        # neutral ground
        if(any(data$Neutral[fixId])){
          neutralIds <- which(data$Neutral[fixId])
          homeWin[neutralIds] <-
            sqrt(homeWin[neutralIds] *
                   exp(opt.optim.MLHA$par[nb.teams+data$homeId[fixId]])[
                     neutralIds])
          homeLoss[neutralIds] <-
            sqrt(homeLoss[neutralIds] *
                   exp(opt.optim.MLHA$par[nb.teams+data$visitId[fixId]])[
                     neutralIds])
          draw[data$Neutral[fixId]] <- exp(opt.optim.MLHA$par[nb.teams*2+1])*
            sqrt(homeWin[data$Neutral[fixId]]*homeLoss[data$Neutral[fixId]])
        }
        
        denominatorProb <- homeWin+draw+homeLoss
        
        
        
        # Loop over the relaxation parameters
        for(k in 1:nbKs){
          
          # Calculate the outcome probabilities
          outcomeProbsHA <- cbind(homeWin/denominatorProb,draw/denominatorProb,
                                  homeLoss/denominatorProb)/(1+3*ks[k])+
            ks[k]/(1+3*ks[k])
          
          # Rescale the probabilities to the [0,1] interval with a sum of 1
          rowMins <- apply(outcomeProbsHA,1,min)
          outcomeProbsHA <- outcomeProbsHA-rowMins*(rowMins<0)
          outcomeProbsHA <- outcomeProbsHA/rep(rowSums(outcomeProbsHA),3)
          
          # Extract the predicted probability of the actual outcome
          outcomeProbHA <- outcomeProbsHA[1:nrow(outcomeProbsHA)+
                                            (
                                              1*as.numeric(data[fixId,"result"]=="D")+
                                                2*as.numeric(data[fixId,"result"]=="A")
                                            )*nrow(outcomeProbsHA)]
          
          appendSummary[[paste0("probHA","_",halfPeriod,"_",ks[k])]] <- outcomeProbHA
          appendSummary[[paste0("probHA","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsHA[,1]
          appendSummary[[paste0("probHA","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsHA[,2]
        }
      }
    }
    
    #############################################################
    # Team strength calculations using weighted goal difference #
    #############################################################
    
    if("BTGoalDiff" %in% methods){
      for(j in 1:nbHalfPeriods){
        halfPeriod <- halfPeriods[j]
        
        # Calculate the ML estimates
        init <- doubleListLookup(inits,"BTGoalDiff",halfPeriod,nb.teams+2)
        MLData <- data[dataIds,]
        MLData$weights <- log2(abs(data[dataIds,"goaldif"])+1)
        tryCatch({
          opt.optim.MLBTGD <- optim(init,LBTGD,gr = NULL,
                                    data=MLData,
                                    halfPeriod=halfPeriod,method="BFGS")
        }, error=function(e){
          opt.optim.MLBTGD <<- optim(init,LBTGD,gr = NULL,
                                     data=MLData,
                                     halfPeriod=halfPeriod,method="CG")
        })
        inits[["BTGoalDiff"]][[as.character(halfPeriod)]] <<- opt.optim.MLBTGD$par
        
        # Calculate the numerator and denominator terms of all fixtures
        homeWin <- exp(opt.optim.MLBTGD$par[data$homeId[fixId]]+
                         opt.optim.MLBTGD$par[nb.teams+2])
        draw <- exp(opt.optim.MLBTGD$par[nb.teams+1])*
          sqrt(exp(opt.optim.MLBTGD$par[data$homeId[fixId]]+
                     opt.optim.MLBTGD$par[nb.teams+2]+
                     opt.optim.MLBTGD$par[data$visitId[fixId]]))
        homeLoss <- exp(opt.optim.MLBTGD$par[data$visitId[fixId]])
        denominatorProb <- homeWin+draw+homeLoss
        
        # Loop over the relaxation parameters
        for(k in 1:nbKs){
          
          # Calculate the outcome probabilities
          outcomeProbsGD <- cbind(homeWin/denominatorProb,
                                  draw/denominatorProb,
                                  homeLoss/denominatorProb)/(1+3*ks[k])+
            ks[k]/(1+3*ks[k])
          
          # Rescale the probabilities to the [0,1] interval with a sum of 1
          rowMins <- apply(outcomeProbsGD,1,min)
          outcomeProbsGD <- outcomeProbsGD-rowMins*(rowMins<0)
          outcomeProbsGD <- outcomeProbsGD/rep(rowSums(outcomeProbsGD),3)
          
          # Extract the predicted probability of the actual outcome
          outcomeProbGD <- outcomeProbsGD[1:nrow(outcomeProbsGD)+
                                            (
                                              1*as.numeric(data[fixId,"result"]=="D")+
                                                2*as.numeric(data[fixId,"result"]=="A")
                                            )*nrow(outcomeProbsGD)]
          
          appendSummary[[paste0("probGD","_",halfPeriod,"_",ks[k])]] <- outcomeProbGD
          appendSummary[[paste0("probGD","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsGD[,1]
          appendSummary[[paste0("probGD","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsGD[,2]
        }
      }
    }
    
    #####################################
    # Poisson goal scoring distribution #
    #####################################
    
    if("Poisson" %in% methods){
      for(j in 1:nbHalfPeriods){
        halfPeriod <- halfPeriods[j]
        # Calculate the ML estimates
        init <- doubleListLookup(inits,"Poisson",halfPeriod,nb.teams+2)
        tryCatch({
          opt.optim.MLP <- optim(init,LPoiss,gr = NULL,
                                 data=data[dataIds,],
                                 factorials=factorials,
                                 halfPeriod=halfPeriod,method="BFGS")
        }, error=function(e){
          opt.optim.MLP <<- optim(init,LPoiss,gr = NULL,
                                  data=data[dataIds,],
                                  factorials=factorials,
                                  halfPeriod=halfPeriod,method="CG")
        })
        inits[["Poisson"]][[as.character(halfPeriod)]] <<- opt.optim.MLP$par
        
        # Calculate the numerator and denominator terms of all fixtures
        c1 <- exp(opt.optim.MLP$par[nb.teams+1])
        H <- exp(opt.optim.MLP$par[nb.teams+2])
        lambdasHome <- c1*(H*exp(opt.optim.MLP$par[data$homeId[fixId]]-
                                   opt.optim.MLP$par[data$visitId[fixId]]))
        lambdasAway <- c1*(exp(opt.optim.MLP$par[data$visitId[fixId]]-
                                 opt.optim.MLP$par[data$homeId[fixId]])/H)
        
        homeWin <- pskellam(-1,lambdasAway,lambdasHome)
        draw <- pskellam(0,lambdasAway,lambdasHome)-homeWin
        homeLoss <- 1-homeWin-draw
        
        # Loop over the relaxation parameters
        for(k in 1:nbKs){
          
          # Calculate the outcome probabilities
          if(relaxationType=="probabilities"){
            outcomeProbsP <- cbind(homeWin,draw,homeLoss)/(1+3*ks[k])+
              ks[k]/(1+3*ks[k])
          } else{
            # Calculate relaxed lambdas
            relaxedLambdasHome <- lambdasHome^(1-5*ks[k])
            relaxedLambdasAway <- lambdasAway^(1-5*ks[k])
            
            homeWinR <- pskellam(-1,relaxedLambdasAway,relaxedLambdasHome)
            drawR <- pskellam(0,relaxedLambdasAway,relaxedLambdasHome)-homeWinR
            homeLossR <- 1-homeWinR-drawR
            
            outcomeProbsP <- cbind(homeWinR,drawR,homeLossR)
          }
          
          # Rescale the probabilities to the [0,1] interval with a sum of 1
          rowMins <- apply(outcomeProbsP,1,min)
          outcomeProbsP <- outcomeProbsP-rowMins*(rowMins<0)
          outcomeProbsP <- outcomeProbsP/rep(rowSums(outcomeProbsP),3)
          
          # Extract the predicted probability of the actual outcome
          outcomeProbP <- outcomeProbsP[1:nrow(outcomeProbsP)+
                                          (
                                            1*as.numeric(data[fixId,"result"]=="D")+
                                              2*as.numeric(data[fixId,"result"]=="A")
                                          )*nrow(outcomeProbsP)]
          
          appendSummary[[paste0("probP","_",halfPeriod,"_",ks[k])]] <- outcomeProbP
          appendSummary[[paste0("probP","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsP[,1]
          appendSummary[[paste0("probP","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsP[,2]
        }
      }
    }
    
    #############################################
    # Defensive-attacking strength calculations #
    #############################################
    
    if("DefAttack" %in% methods){
      for(j in 1:nbHalfPeriods){
        halfPeriod <- halfPeriods[j]
        # Calculate the ML estimates
        init <- doubleListLookup(inits,"DefAttack",halfPeriod,nb.teams*2+2)
        tryCatch({
          opt.optim.MLDA <- optim(init,LPoissDA,gr = NULL,
                                  data=data[dataIds,],
                                  factorials=factorials,
                                  halfPeriod=halfPeriod,method="BFGS")
        }, error=function(e){
          opt.optim.MLDA <<- optim(init,LPoissDA,gr = NULL,
                                   data=data[dataIds,],
                                   factorials=factorials,
                                   halfPeriod=halfPeriod,method="CG")
        })
        
        inits[["DefAttack"]][[as.character(halfPeriod)]] <<- opt.optim.MLDA$par
        
        # Calculate the numerator and denominator terms of all fixtures
        c1 <- exp(opt.optim.MLDA$par[nb.teams*2+1])
        H <- exp(opt.optim.MLDA$par[nb.teams*2+2])
        lambdasHome <- c1*(H*exp(opt.optim.MLDA$par[nb.teams+data$homeId[fixId]]-
                                   opt.optim.MLDA$par[data$visitId[fixId]]))
        lambdasAway <- c1*(exp(opt.optim.MLDA$par[nb.teams+data$visitId[fixId]]-
                                 opt.optim.MLDA$par[data$homeId[fixId]])/H)
        homeWin <- pskellam(-1,lambdasAway,lambdasHome)
        draw <- pskellam(0,lambdasAway,lambdasHome)-homeWin
        homeLoss <- 1-homeWin-draw
        
        # Loop over the relaxation parameters
        for(k in 1:nbKs){
          
          # Calculate the outcome probabilities
          outcomeProbsDA <- cbind(homeWin,draw,homeLoss)/(1+3*ks[k])+
            ks[k]/(1+3*ks[k])
          
          # Rescale the probabilities to the [0,1] interval with a sum of 1
          rowMins <- apply(outcomeProbsDA,1,min)
          outcomeProbsDA <- outcomeProbsDA-rowMins*(rowMins<0)
          outcomeProbsDA <- outcomeProbsDA/rep(rowSums(outcomeProbsDA),3)
          
          # Extract the predicted probability of the actual outcome
          outcomeProbDA <- outcomeProbsDA[1:nrow(outcomeProbsDA)+
                                            (
                                              1*as.numeric(data[fixId,"result"]=="D")+
                                                2*as.numeric(data[fixId,"result"]=="A")
                                            )*nrow(outcomeProbsDA)]
          
          appendSummary[[paste0("probDA","_",halfPeriod,"_",ks[k])]] <- outcomeProbDA
          appendSummary[[paste0("probDA","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsDA[,1]
          appendSummary[[paste0("probDA","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsDA[,2]
        }
      }
    }
    
    ###########################################################
    # Add all combined calculations to the summary data frame #
    ###########################################################
    summary <- rbind(summary,appendSummary)
    rownames(summary) <- 1:nrow(summary)
    
    # Save each saveEachLoops iterations
    if((iteration %% saveEachLoops)==0){
      save(summary, file = targetSaveFile)
    }
    iteration <- iteration + 1
  }
  save(summary, file = targetSaveFile)
}

# Function to look up values in a list with a default 0 vector of a given length
doubleListLookup <- function(myList,first,second,defaultLength){
  out <- rep(0,defaultLength)
  if(!is.null(myList) && !is.null(myList[[as.character(first)]][[as.character(second)]])){
    out <- myList[[as.character(first)]][[as.character(second)]]
  }
  
  out
}

# Rprof("profFile.out", line.profiling=TRUE)
# predictNationalMatches(startPredictionDate,predictionPeriodYears,
#                        20,seq(0,0.2,0.005),targetSaveFile,
#                        continueAnalysis)
predictNationalMatches(startPredictionDate,predictionPeriodYears,
                       20*(1:30),seq(0,0.2,0.005),targetSaveFile,
                       continueAnalysis,onlyPoisson=onlyPoisson,
                       methods="BTHA")
# predictNationalMatches(startPredictionDate,predictionPeriodYears,20*(1:30),
#                        seq(0,0.2,0.005),targetSaveFile,
#                        continueAnalysis)
# Rprof(NULL)
# summaryRprof("profFile.out", lines = "show")