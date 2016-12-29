# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis"))

# Simulation parameters
continueAnalysis <- TRUE
predictionPeriodYears <- 4
FIFAWeights <- TRUE

# Path to the elo update logic
eloLogicPath <- paste0(basePath,"Common files/eloUpdate.R")

# Path to the elo to outcome polr model
eloToOutcomePath <- paste0(basePath,"Common files/eloToOutcome.RData")

# Path to the national team match results
nationalDataPath <- paste0(getwd(),"/Scraper/internationalResultsEuropeP.RData")

# General parameters
startPredictionDate <- as.Date(paste0(1992+predictionPeriodYears,"-01-01"))
# startPredictionDate <- as.Date("2016-01-01")

# Calculate the save string parts
saveStringWeights <- ifelse(FIFAWeights,"FIFA","Equal")

# Save each saveEachLoops of match iterations
saveEachLoops <- 1

# Path to save the results
targetSaveFile <- paste0("predictProbsNat",predictionPeriodYears,
                         saveStringWeights,"WeightsElo.RData")


##########################################
# Section 1: Source the Elo update logic #
##########################################

source(eloLogicPath)
load(eloToOutcomePath)


######################################
# Section 2: Elo prediction function #
######################################

predictNationalMatches <- function(startPredictionDate,predictionPeriodYears,
                                   basisKs,ks,targetSaveFile,continueAnalysis){
  
  summary <- NULL
  nbKs <- length(ks)
  nbBasisKs <- length(basisKs)
  
  # Load the studied dataset
  load(nationalDataPath)
  data <- allMatches
  rm(allMatches)
  data <- data[order(data$Date),]
  
  # Overwrite match importance weights when not using the FIFA weights
  if(!FIFAWeights){
    data$impScore <- 1
  }
  
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
  nbTeams <- length(teams)
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
    
    #####################################################
    # Calculate the initial Elo ratings for all basisKs #
    #####################################################
    
    if(iteration==1){
      eloRatings <- matrix(rep(0,nbTeams*nbBasisKs),nrow=nbTeams)
      # Loop over all basis K update values and calculate the according initial
      # ratings
      for(i in 1:nbBasisKs){
        cat("K",i,"of",nbBasisKs,"\n")
        teamRatings <- eloRatings[,i]
        basisK <- basisKs[i]
        for(j in 1:nrow(dataOrig)){
          dataRow <- dataOrig[j,]
          if(dataRow$Date>=appendSummary$Date[1]){
            break
          }
          firstId <- dataRow$homeId
          secondId <- dataRow$visitId
          # Update calculation of the Elo rating of the first team
          firstEloUpdate <- eloUpdate(teamRatings[firstId],
                                      teamRatings[secondId],
                                      dataRow$impScore*basisK,
                                      dataRow$Neutral,
                                      dataRow$hgoal,
                                      dataRow$vgoal)
          teamRatings[firstId] <- teamRatings[firstId] + firstEloUpdate
          teamRatings[secondId] <- teamRatings[secondId] - firstEloUpdate
        }
        eloRatings[,i] <- teamRatings
      }
    }
    
    # Extract the team ids
    firstIds <- data[fixId,"homeId"]
    secondIds <- data[fixId,"visitId"]
    
    for(j in 1:nbBasisKs){
      basisK <- basisKs[j]
      
      # Calculate the outcome probabilities
      ratingDifferences <- eloRatings[firstIds,j] - eloRatings[secondIds,j] +
        100*(!appendSummary$Neutral)
      ratingDifferencesDf <- data.frame(eloDifference=ratingDifferences)
      polrPredict <- predict(fitPolr,type="probs",newdata = ratingDifferencesDf)
      polrPredict <- matrix(polrPredict,ncol=3)
      homeWin <- polrPredict[,1]
      draw <- polrPredict[,2]
      homeLoss <- polrPredict[,3]
      
      # Loop over the relaxation parameters
      for(k in 1:nbKs){
        # Calculate the outcome probabilities
        outcomeProbs <- cbind(homeWin,draw,homeLoss)/(1+3*ks[k])+
          ks[k]/(1+3*ks[k])
        
        # Extract the predicted probability of the actual outcome
        outcomeProb <- outcomeProbs[1:nrow(outcomeProbs)+
                                      (
                                        1*as.numeric(data[fixId,"result"]=="D")+
                                          2*as.numeric(data[fixId,"result"]=="A")
                                      )*nrow(outcomeProbs)]
        
        appendSummary[[paste0("probElo","_",basisK,"_",ks[k])]] <- outcomeProb
        appendSummary[[paste0("probElo","_",basisK,"_",ks[k],"_H")]] <- outcomeProbs[,1]
        appendSummary[[paste0("probElo","_",basisK,"_",ks[k],"_D")]] <- outcomeProbs[,2]
      }
      
      # Update the Elo ratings
      for(matchId in seq_along(fixId)){
        dataRow <- appendSummary[matchId,]
        
        # Extract the team ids
        firstId <- firstIds[matchId]
        secondId <- secondIds[matchId]
        
        # Update calculation of the Elo rating of the first team
        firstEloUpdate <- eloUpdate(eloRatings[firstId,j],
                                    eloRatings[secondId,j],
                                    dataRow$impScore*basisK,
                                    dataRow$Neutral,
                                    dataRow$hgoal,
                                    dataRow$vgoal)
        eloRatings[firstId,j] <- eloRatings[firstId,j] + firstEloUpdate
        eloRatings[secondId,j] <- eloRatings[secondId,j] - firstEloUpdate
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

predictNationalMatches(startPredictionDate,predictionPeriodYears,
                       seq(1,50,1),seq(0,0.2,0.005),targetSaveFile,
                       continueAnalysis)
# predictNationalMatches(startPredictionDate,predictionPeriodYears,20*(1:30),
#                        seq(0,0.2,0.005),targetSaveFile,
#                        continueAnalysis)