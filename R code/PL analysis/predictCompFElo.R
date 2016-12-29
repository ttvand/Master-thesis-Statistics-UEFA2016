# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"PL analysis"))

# Load english soccer data
# library(devtools)
# http://stackoverflow.com/questions/31293325/r-install-github-fails
# install_github('jalapic/engsoccerdata', username = "jalapic")
library(engsoccerdata)
library(skellam) # Difference of two poisson distributions
library(ggvis)
library(rgl)
library(MASS)

# Save file
targetSaveFile <- "predictCompSummaryProbsElo.RData"

# Path to the elo update logic
eloLogicPath <- paste0(basePath,"Common files/eloUpdate.R")

# Path to the elo to outcome polr model
eloToOutcomePath <- paste0(basePath,"Common files/eloToOutcome.RData")

# General parameters
targetSeasons <- 2000:2014
continueAnalysis <- TRUE
impScore <- 2.5

##########################################
# Section 1: Source the Elo update logic #
##########################################

source(eloLogicPath)
load(eloToOutcomePath)


######################################
# Section 2: Elo prediction function #
######################################

predictMatchesElo <- function(targetSeasons,basisKs,ks,targetSaveFile){
  summary <- NULL
  nbSeasons <- length(targetSeasons)
  nbKs <- length(ks)
  nbBasisKs <- length(basisKs)
  
  for(i in 1:nbSeasons){
    # Subset scores dataset
    targetSeason <- targetSeasons[i]
    data <- engsoccerdata2[engsoccerdata2$Season == targetSeason &
                             engsoccerdata2$division == "1",]
    data <- data[order(data$Date),]
    cat("Considered season:",targetSeason,"\n")
    
    if(nrow(data)>100){
      
      # Preprocess data
      teams <- unique(sort(c(data$home,data$visitor)))
      nbTeams <- length(teams)
      data$homeId <- match(data$home,teams)
      data$visitId <- match(data$visitor,teams)
      
      # Only consider seasons with an even number of teams
      if(nbTeams %% 2 == 0){
        
        # Split data from fixture to fixture and calculate predictions for future
        # matches
        startIndex <- (nbTeams-1)*nbTeams/2+1
        
        # Calculate initial Elo ratings
        eloRatings <- matrix(rep(0,nbTeams*nbBasisKs),nrow=nbTeams)
        # Loop over all basis K update values and calculate the according initial
        # ratings
        for(i in 1:nbBasisKs){
          # cat("K",i,"of",nbBasisKs,"\n")
          teamRatings <- eloRatings[,i]
          basisK <- basisKs[i]
          for(j in 1:(startIndex-1)){
            dataRow <- data[j,]
            firstId <- dataRow$homeId
            secondId <- dataRow$visitId
            # Update calculation of the Elo rating of the first team
            firstEloUpdate <- eloUpdate(teamRatings[firstId],
                                        teamRatings[secondId],
                                        impScore*basisK,
                                        FALSE,
                                        dataRow$hgoal,
                                        dataRow$vgoal)
            teamRatings[firstId] <- teamRatings[firstId] + firstEloUpdate
            teamRatings[secondId] <- teamRatings[secondId] - firstEloUpdate
          }
          eloRatings[,i] <- teamRatings
        }
        
        while(TRUE){
          dataIds <- 1:(startIndex-1)
          daysDiff <- as.numeric(difftime(as.Date(c(
            data$Date[-(1:(startIndex))],NA)),
            as.Date(data$Date[-(dataIds)]),units="days"))
          lastMatchId <- startIndex + which(daysDiff>1 | is.na(daysDiff))[1]-1
          
          if(is.na(lastMatchId)){
            lastMatchId <- startIndex
          }
          
          fixId <- startIndex:lastMatchId
          data$days.passed <- as.numeric(difftime(max(
            data$Date[fixId]),data$Date,units="days"))
          
          # Generate the list to append to the summary file
          appendSummary <- list(
            team1=teams[data[fixId,"homeId"]],
            team2=teams[data[fixId,"visitId"]],
            season=targetSeason)
          
          # Extract the team ids
          firstIds <- data[fixId,"homeId"]
          secondIds <- data[fixId,"visitId"]
          
          for(j in 1:nbBasisKs){
            basisK <- basisKs[j]
            
            # Calculate the outcome probabilities
            ratingDifferences <- eloRatings[firstIds,j] -
              eloRatings[secondIds,j] + 100
            ratingDifferencesDf <- data.frame(eloDifference=ratingDifferences)
            polrPredict <- predict(fitPolr,type="probs",
                                   newdata = ratingDifferencesDf)
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
              dataRow <- data[fixId[matchId],]
              
              # Extract the team ids
              firstId <- firstIds[matchId]
              secondId <- secondIds[matchId]
              
              # Update calculation of the Elo rating of the first team
              firstEloUpdate <- eloUpdate(eloRatings[firstId,j],
                                          eloRatings[secondId,j],
                                          impScore*basisK,
                                          FALSE,
                                          dataRow$hgoal,
                                          dataRow$vgoal)
              eloRatings[firstId,j] <- eloRatings[firstId,j] + firstEloUpdate
              eloRatings[secondId,j] <- eloRatings[secondId,j] - firstEloUpdate
            }
          }
          
          appendSummary[["outcome"]] <- data[fixId,"result"]
          
          ###########################################################
          # Add all combined calculations to the summary data frame #
          ###########################################################
          summary <- rbind(summary,as.data.frame(appendSummary,
                                                 stringsAsFactors=FALSE))
          
          # Increment the start match index and exit the infite loop when all
          # matches have been processed
          if(lastMatchId>=nrow(data)){
            save(summary, file = targetSaveFile)
            break
          } else{
            startIndex <- lastMatchId+1
          }
        }
      }
    }
  }
  summary <- summary[,c(1:3,ncol(summary),4:(ncol(summary)-1))]
  summary
}

# summary <- predictMatches(2014,50,0)
# summary <- predictMatches(2000:2014,226,0.02,c("BT","Poisson","DefAttack","BTGoalDiff"))
summary <- predictMatchesElo(2000:2014,seq(1,50,1),seq(0,0.3,0.005),
                             targetSaveFile)
# save(summary, file = targetSaveFile)