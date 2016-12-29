# Clear workspace
rm(list=ls())

# Set the seed in order to obtain reproducible results
set.seed(16)

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"EURO 2016 simulation study"))

# Path to the national team match results
nationalDataPath <- paste0(basePath,"Scraper/internationalResultsEuropeP.RData")

# Path to save the simulation results
simulationsSaveFile <- "simulationResults.RData"

# Load required libraries
library(skellam) # Difference of two poisson distributions
library(engsoccerdata)

# Simulation parameters
nbSims <- 1e4
refitModelsThroughoutTournament <- TRUE # Critical wrt execution time
predictionPeriodYears <- 4
FIFAWeights <- TRUE
halfPeriod <- 1320
equalStrengths <- FALSE
permuteTeams <- FALSE

# Save each saveEachSimulations of simulations
saveEachSimulations <- nbSims/100

# Path to the maximum likelihood functions
mlPath <- paste0(basePath,"Common files/mlFunctions.R")

# Path to the national team match results
nationalDataPath <- paste0(basePath,"National analysis/Scraper/internationalResultsEuropeP.RData")
load(nationalDataPath)
data <- allMatches[allMatches$Date>as.Date("01/01/2012", "%d/%m/%Y"),
                   c("Date","Home","Away","hgoal","vgoal","impScore",
                     "Neutral")]

# Load the match schedule files
teamsGroups <- read.csv("teams.csv",stringsAsFactors = FALSE)
firstRound <- read.csv("1st round.csv",stringsAsFactors = FALSE)
secondRound <- read.csv("2nd round.csv",stringsAsFactors = FALSE)
knockout <- read.csv("knockout.csv",stringsAsFactors = FALSE)


###########################################
# Section 1: Source the base ML functions #
###########################################

source(mlPath)


#####################################################
# Section 2: Process the match schedule information #
#####################################################

# Reformat the date of the 1st and knockout rounds
Sys.setlocale("LC_TIME", "English")
firstRound$Date <- as.Date(firstRound$Date, "%A %d %B %Y")
knockout$Date <- as.Date(knockout$Date, "%A %d %B %Y")


################################################
# Section 3: prepare data for simulation study #
################################################

# Rename the teams to match the historical data with the simulated matches
# Preprocess data

# Rename the Irelands in the historical data
data$Home[data$Home=="North. Ireland"] <- "Northern Ireland"
data$Away[data$Away=="North. Ireland"] <- "Northern Ireland"
data$Home[data$Home=="Rep. of Ireland"] <- "Republic of Ireland"
data$Away[data$Away=="Rep. of Ireland"] <- "Republic of Ireland"

# Calculate all european teams based on historical data
teams <- unique(sort(c(data$Home,data$Away)))
nbTeams <- length(teams)

# Calculate the team ids
data$homeId <- match(data$Home,teams)
data$visitId <- match(data$Away,teams)

# Add the team ids to the first round data
firstRound$Team1Id <- match(firstRound$Team1,teams)
firstRound$Team2Id <- match(firstRound$Team2,teams)

# Add the simulated number of goals to the first round and knockout matches
firstRound$sim1Goal <- NA
firstRound$sim2Goal <- NA
knockout$sim1Goal <- NA
knockout$sim2Goal <- NA


#################################################################
# Section 4: Calculate the base strengths of all european teams #
#################################################################

# Calculate factorials beforehand to speed up the poisson ML estimation
factorials <- factorial(0:max(c(data$hgoal,data$vgoal)))

# Tournament start date
tournamentStart <- as.Date("10/06/2016", "%d/%m/%Y")
startModelDate <- as.POSIXlt(tournamentStart)
startModelDate$year <- startModelDate$year-predictionPeriodYears
startModelDate <- as.Date(startModelDate)

# Calculate the number of days passed for all historical matches
data$days.passed <- as.numeric(difftime(tournamentStart,
                                        data$Date,units="days"))

# Calculate the ML estimates
dataIds <- which(data$Date>startModelDate)
init <- rep(0,nbTeams+2)
opt.optim.MLPI <- optim(init,LPoiss,gr = NULL, data=data[dataIds,],
                        factorials=factorials,
                        halfPeriod=halfPeriod,method="BFGS")

# Calculate the numerator and denominator terms of all fixtures
c1I <- exp(opt.optim.MLPI$par[nbTeams+1])
HI <- exp(opt.optim.MLPI$par[nbTeams+2])^(1-as.numeric(equalStrengths))

# Find the ordering of the teams using the initial model
# initStrengths <- opt.optim.MLPI$par[1:54]
# ranking <- order(initStrengths,decreasing = TRUE)
# rankedTeams <- teams[ranking]
# browser()

#########################################
# Helper functions for simulation study #
#########################################

# Function to generate a league table given a number of matches
getTable <- function(data,points){
  data$home <- data$Team1
  data$visitor <- data$Team2
  data$hgoal <- data$sim1Goal
  data$vgoal <- data$sim2Goal
  maketable(data,points=points)
}

# Helper function to return the highest of two ranked team in the first round
pickBestOfTwo <- function(consideredTeams,groupMatches,groupTable,debug=FALSE){
  # Debug the function if the debug input flag is TRUE
  if(debug) browser()
  
  # Calculate the team ids in the table
  firstTeamId <- which(groupTable$team==consideredTeams[1])
  secondTeamId <- which(groupTable$team==consideredTeams[2])
  lowestTeamId <- min(c(firstTeamId,secondTeamId))
  
  # Consider the mutual match
  mutualId <- which(groupMatches$Team1 %in% consideredTeams &
                      groupMatches$Team2 %in% consideredTeams)
  
  if(groupMatches$sim1Goal[mutualId]!=groupMatches$sim2Goal[mutualId]){
    winner <- ifelse(groupMatches$sim1Goal[mutualId]>
                       groupMatches$sim2Goal[mutualId],
                     groupMatches$Team1[mutualId],
                     groupMatches$Team2[mutualId])
    return(winner)
  }
  
  # Consider the overall goal difference
  if(groupTable$gd[firstTeamId]!=
     groupTable$gd[secondTeamId]){
    winner <- groupTable$team[lowestTeamId]
    return(winner)
  }
  
  # Consider the overall number of goals scored
  if(groupTable$gf[firstTeamId]!=
     groupTable$gf[secondTeamId]){
    winner <- groupTable$team[lowestTeamId]
    return(winner)
  }
  
  # Random assignment
  return(sample(consideredTeams,1))
}

############################
# Perform simulation study #
############################

# Store the original data as this will be modified during each simulation run
originalData <- data
originalKnockout <- knockout

# Initialize the simulations object
simulations <- list()

for(i in 1:nbSims){
  # Display the iteration id
  cat("Simulation",i,"of",nbSims,"\n")
  
  # Reset original data from before the simulation study 
  data <- originalData
  
  if(permuteTeams){
    # Shuffle first round teams
    for(j in 1:4){
      teamsToPermute <- teamsGroups[1:(6-(j==1)),j]
      resampled <- sample(teamsToPermute)
      matchIds <- lapply(teamsToPermute,function(x) x==firstRound[,2:3])
      for(team in teamsToPermute){
        # Target resample id?
        targetMatches <- matchIds[which(resampled==team)]
        firstRound[,2:3][targetMatches[[1]]] <- team
      }
    }
  }
  
  ##########################################
  # Simulate first round matches           #
  # 1) Simulate first match for each team  #
  # 2) Recalculate ML estimates            #
  # 3) Simulate second match for each team #
  # 4) Recalculate ML estimates            #
  # 5) Simulate third match for each team  #
  # 6) Recalculate ML estimates            #
  # 7) Calculate first, second and third   #
  ##########################################
  
  # Reset the ML model fit and parameters (to avoid having to refit
  # the model with no new data in the first round of next simulations)
  opt.optim.MLP <- opt.optim.MLPI
  c1 <- c1I
  H <- HI
  
  # 1-6) Simulate first match for each team and recalculate ML estimates
  for(j in 1:3){
    fixId <- 1:12 + (12*(j-1))
    nbMatches <- length(fixId)
    HM <- H^(("France"==firstRound$Team1[fixId])-1*
               ("France"==firstRound$Team2[fixId]))
    lambdas1 <- c1*(HM*exp(opt.optim.MLP$par[firstRound$Team1Id[fixId]]-
                             opt.optim.MLP$par[firstRound$Team2Id[fixId]]))
    lambdas1 <- lambdas1^(1-as.numeric(equalStrengths))
    lambdas2 <- c1*(exp(opt.optim.MLPI$par[firstRound$Team2Id[fixId]]-
                          opt.optim.MLPI$par[firstRound$Team1Id[fixId]])/HM)
    lambdas2 <- lambdas2^(1-as.numeric(equalStrengths))
    
    # Generate random outcomes
    firstRound$sim1Goal[fixId] <- rpois(nbMatches,lambdas1)
    firstRound$sim2Goal[fixId] <- rpois(nbMatches,lambdas2)
    
    ############################
    # Recalculate ML estimates #
    ############################
    
    # Append simulated data in the right format
    data <- rbind(data,data.frame(Date=firstRound$Date[fixId],
                                  Home=firstRound$Team1[fixId],
                                  Away=firstRound$Team2[fixId],
                                  hgoal=firstRound$sim1Goal[fixId],
                                  vgoal=firstRound$sim2Goal[fixId],
                                  impScore=3,
                                  Neutral=abs(HM-1)<1e-10,
                                  homeId=firstRound$Team1Id[fixId],
                                  visitId=firstRound$Team2Id[fixId],
                                  days.passed=NA,
                                  stringsAsFactors = FALSE))
    
    # Next round start date
    nextRoundStart <- c(as.Date("15/06/2016", "%d/%m/%Y"),
                        as.Date("19/06/2016", "%d/%m/%Y"),
                        as.Date("25/06/2016", "%d/%m/%Y"))[j]
    startModelDate <- as.POSIXlt(nextRoundStart)
    startModelDate$year <- startModelDate$year-predictionPeriodYears
    startModelDate <- as.Date(startModelDate)
    
    # Calculate the number of days passed for all historical matches
    data$days.passed <- as.numeric(difftime(nextRoundStart,
                                            data$Date,units="days"))
    
    # Calculate the ML estimates
    dataIds <- which(data$Date>startModelDate)
    init <- rep(0,nbTeams+2)
    if(refitModelsThroughoutTournament){
      tryCatch({
      opt.optim.MLP <- optim(init,LPoiss,gr = NULL, data=data[dataIds,],
                             factorials=factorials,
                             halfPeriod=halfPeriod,method="BFGS")
      }, error=function(e){
        opt.optim.MLP <<- optim(init,LPoiss,gr = NULL,
                                data=data[dataIds,],
                                factorials=factorials,
                                halfPeriod=halfPeriod,method="CG")
      })
    }
    
    # Calculate the numerator and denominator terms of all fixtures
    c1 <- exp(opt.optim.MLP$par[nbTeams+1])
    H <- exp(opt.optim.MLP$par[nbTeams+2])^(1-as.numeric(equalStrengths))
  }
  
  # 7) Calculate first, second and third of each group
  # According to 18.01 a through f (g and h ignored => random assignment)
  groupNames <- sort(unique(firstRound$Group))
  positions <- matrix(rep(NA,24),nrow=4,dimnames=list(1:4,groupNames))
  thirdPlaces <- matrix(rep(NA,24),nrow=4,
                        dimnames=list(c("Team","Pts","gd","gf"),groupNames))
  for(group in groupNames){
      
    # Placeholder for the league positions
    groupPositions <- rep(NA,4)
    
    # Calculate the group table
    groupMatches <- firstRound[firstRound$Group==group,] 
    groupTable <- getTable(groupMatches,points=3)
    
    # Calculate the points ex aequos
    groupPoints <- groupTable$Pts
    pointsTable <- table(groupPoints)
    pointsDraws <- as.numeric(names(pointsTable[pointsTable>1]))
    
    # Assign positions for the teams that did not have the same number of points
    # as any of the other teams
    nonDrawIds <- which(!groupPoints %in% pointsDraws)
    groupPositions[nonDrawIds] <- groupTable$team[nonDrawIds]
    
    # Assign positions for teams with an equal number of points
    for(drawnPoints in pointsDraws){
      bestPositionDrawnTeams <- 1+sum(groupPoints>drawnPoints)
      nbDrawnTeams <- sum(drawnPoints==groupPoints)
      consideredTeams <- groupTable$team[groupPoints==drawnPoints]
      drawnTeams <- consideredTeams
      if(nbDrawnTeams==2){
        # Pick the best of the two teams using the helper function
        # pickBestOfTwo
        winner <- pickBestOfTwo(consideredTeams,groupMatches,groupTable)
        
        # Assign team positions
        loser <- consideredTeams[consideredTeams != winner]
        groupPositions[bestPositionDrawnTeams] <- winner
        groupPositions[bestPositionDrawnTeams+1] <- loser
      } else{
        if(nbDrawnTeams<4){
          # Consider the mutual matches
          mutualIds <- which(groupMatches$Team1 %in% consideredTeams &
                               groupMatches$Team2 %in% consideredTeams)
          
          # Create a table for the mutual matches
          mutualMatches <- groupMatches[mutualIds,]
          mutualTable <- getTable(mutualMatches,points=3)
          
          # Consider the score in mutual matches
          mutualScores <- 1e4*t(mutualTable[,"Pts"])+
            1e2*t(mutualTable[,"gd"])+t(mutualTable[,"gf"])
          
          # Assign the rank for teams that have a different mutual score
          for(k in 1:nbDrawnTeams){
            if(!mutualScores[k] %in% mutualScores[-k]){
              consideredMutualTeam <- mutualTable$team[k]
              nbBetter <- sum(mutualScores>mutualScores[k])
              groupPositions[bestPositionDrawnTeams+nbBetter] <-
                consideredMutualTeam
              drawnTeams <- drawnTeams[drawnTeams!=consideredMutualTeam]
            }
          }
        }
        
        if(length(drawnTeams)==nbDrawnTeams){
          consideredIds <- match(consideredTeams,groupTable$team)
          
          # Consider the score in all matches
          overallScores <- 1e2*t(groupTable[consideredIds,"gd"])+
            t(groupTable[consideredIds,"gf"])
          
          # Assign the rank for teams that have a different overall score
          for(k in 1:nbDrawnTeams){
            if(!overallScores[k] %in% overallScores[-k]){
              consideredTeam <- consideredTeams[k]
              nbBetter <- sum(overallScores>overallScores[k])
              groupPositions[bestPositionDrawnTeams+nbBetter] <-
                consideredTeam
              drawnTeams <- drawnTeams[drawnTeams!=consideredTeam]
            }
          }
          
          if(length(drawnTeams)==2){
            # Pick the best of the two remaining teams using the helper function
            # pickBestOfTwo
            winner <- pickBestOfTwo(drawnTeams,groupMatches,groupTable)
            
            # Assign team positions
            loser <- drawnTeams[drawnTeams != winner]
            
            # Shift the best draw position if the first spot is occupied by the
            # third team
            bestPositionDrawnTeams <- bestPositionDrawnTeams +
              !(is.na(groupPositions[bestPositionDrawnTeams]))
            
            groupPositions[bestPositionDrawnTeams] <- winner
            groupPositions[bestPositionDrawnTeams+1] <- loser
          } else{
            if(length(drawnTeams)>0){
              if(length(unique(t(overallScores)))==1){
                # Random assignment 
                positionIds <- bestPositionDrawnTeams + 0:(length(drawnTeams)-1)
                groupPositions[positionIds] <- sample(drawnTeams)
                
              } else{
                # Four teams drawn on points but with differing overall scores
                # Three options:
                #   1) 1-2 tied, 3-4 tied
                #   2) 1-2-3 tied, 4th worst
                #   3) 1 best 2-3-4 tied
                
                # 1st case: 1-2 tied, 3-4 tied
                if(overallScores[1]==overallScores[2] &&
                   overallScores[3]==overallScores[4]){
                topTwo <- drawnTeams[1:2]
                bottomTwo <- drawnTeams[3:4]
                winner <- pickBestOfTwo(topTwo,groupMatches,groupTable)
                second <-  topTwo[topTwo != winner]
                third <- pickBestOfTwo(bottomTwo,groupMatches,groupTable)
                fourth <-  bottomTwo[bottomTwo != third]
                groupPositions <- c(winner,second,third,fourth)
                } else{
                  # Case where 1-2-3 tied, 4th worst or
                  # 1 best 2-3-4 tied
                  
                  if(is.na(groupPositions[1])){
                    consideredTeams <- groupTable$team[-4]
                    bestPositionDrawnTeams <- 1
                  } else{
                    consideredTeams <- groupTable$team[-1]
                    bestPositionDrawnTeams <- 2
                  }
                  drawnTeams <- consideredTeams
                  
                  # Consider the mutual matches
                  mutualIds <- which(groupMatches$Team1 %in% consideredTeams &
                                       groupMatches$Team2 %in% consideredTeams)
                  
                  # Create a table for the mutual matches
                  mutualMatches <- groupMatches[mutualIds,]
                  mutualTable <- getTable(mutualMatches,points=3)
                  
                  # Consider the score in mutual matches
                  mutualScores <- 1e4*t(mutualTable[,"Pts"])+
                    1e2*t(mutualTable[,"gd"])+t(mutualTable[,"gf"])
                  
                  # Assign the rank for teams that have a different mutual score
                  nbDrawnTeams <- length(mutualScores)
                  for(k in 1:nbDrawnTeams){
                    if(!mutualScores[k] %in% mutualScores[-k]){
                      consideredMutualTeam <- mutualTable$team[k]
                      nbBetter <- sum(mutualScores>mutualScores[k])
                      groupPositions[bestPositionDrawnTeams+nbBetter] <-
                        consideredMutualTeam
                      drawnTeams <- drawnTeams[drawnTeams!=consideredMutualTeam]
                    }
                  }
                  if(length(drawnTeams)>2){
                    # Assign random rankings
                    groupPositionIds <- bestPositionDrawnTeams +
                      (0:(length(drawnTeams)-1))
                    groupPositions[groupPositionIds] <- sample(consideredTeams)
                  }
                  if(length(drawnTeams)==2){
                    # Pick the best of the two remaining teams using the helper 
                    # function pickBestOfTwo
                    winner <- pickBestOfTwo(drawnTeams,groupMatches,groupTable)
                    
                    # Assign team positions
                    loser <- drawnTeams[drawnTeams != winner]
                    
                    # Find the best draw position (first NA spot)
                    bestPositionDrawnTeams <- which(is.na(groupPositions))[1]
                    
                    groupPositions[bestPositionDrawnTeams] <- winner
                    groupPositions[bestPositionDrawnTeams+1] <- loser
                  }
                }
              }
            }
          }
        } else{
          if(length(drawnTeams)==2){
            # Pick the best of the two remaining teams using the helper function
            # pickBestOfTwo
            winner <- pickBestOfTwo(drawnTeams,groupMatches,groupTable)
            
            # Assign team positions
            loser <- drawnTeams[drawnTeams != winner]
            
            # Shift the best draw position if the first spot is occupied by the
            # third team
            bestPositionDrawnTeams <- bestPositionDrawnTeams +
              !(is.na(groupPositions[bestPositionDrawnTeams]))
            
            groupPositions[bestPositionDrawnTeams] <- winner
            groupPositions[bestPositionDrawnTeams+1] <- loser
          }
        }
      }
    }
    
    # Calculate the third placed attributes (points, goal difference, goals
    # scored)
    thirdTeam <- groupPositions[3]
    thirdPlaces[,which(LETTERS==group)[1]] <-
      c(thirdTeam,t(groupTable[groupTable$team==thirdTeam,c("Pts","gd","gf")]))
    
    # Assign the group positions to the overall matrix
    positions[,which(LETTERS==group)[1]] <- groupPositions
  }
  
  # Check that each positioned team is unique
  if(max(table(positions))>1) browser() # stop("Invalid team ranking!")
  
  # Calculate the best four teams
  thirdTeamPoints <- 1e4*as.numeric(thirdPlaces[2,])+
    1e2*as.numeric(thirdPlaces[3,])+as.numeric(thirdPlaces[4,])
  names(thirdTeamPoints) <- groupNames
  orderedThirds <- sort(thirdTeamPoints,decreasing = TRUE)
  if(orderedThirds[4] != orderedThirds[5]){
    qualifyIds <- 1:4
  } else{
    drawScore <- orderedThirds[4]
    drawIds <- which(orderedThirds==orderedThirds[4])
    nbBetterThanDrawn <- sum(orderedThirds>drawScore)
    
    # Take a random subset of the drawn teams to get to the best four third
    # placed teams
    qualifyIds <- c(which(orderedThirds>drawScore),
                    sample(drawIds,4-nbBetterThanDrawn))
  }
  
  # Calculate the string of the best teams
  bestThirdTeams <- names(orderedThirds)[qualifyIds]
  bestTeamsString <- paste0(sort(bestThirdTeams),collapse="")
  
  # Set up the remaining fixtures
  knockout <- originalKnockout
  
  # Paste the third placed teams into the knockout fixtures
  thirdMatchesId <- which(bestTeamsString==secondRound$Best.teams)
  thirdMatchGroupWinnerIds <- match(paste("Winner",LETTERS[1:4]),knockout[,2])
  winnerOpponentGroups <- t(secondRound[thirdMatchesId,-1])
  winnerOpponents <- positions[3,match(winnerOpponentGroups,groupNames)]
  knockout[thirdMatchGroupWinnerIds,3] <- winnerOpponents
  
  # Paste the group winners into the knockout fixtures
  knockout[match(paste("Winner",groupNames),knockout[,2]),2] <- positions[1,]
  
  # Paste the runner ups into the knockout fixtures
  knockout[c(1,8),2] <- c(positions[2,1],positions[2,2])
  knockout[c(1,7,6,8),3] <- c(positions[2,3:6])
  
  # Create a placeholder for the team ids in the knockout rounds
  knockout$Team1Id <- NA
  knockout$Team2Id <- NA
  
  # Create a placeholder for the winners in the knockout rounds
  knockout$winner <- NA
  
  # Simulate the knockout fixtures
  for(j in 1:4){
    fixId <- (17-2^(4-j+1)):(16-2^(4-j))
    
    # Calculate the team ids
    knockout$Team1Id[fixId] <- match(knockout$Team1[fixId],teams)
    knockout$Team2Id[fixId] <- match(knockout$Team2[fixId],teams)
    
    nbMatches <- length(fixId)
    HM <- H^(("France"==knockout$Team1[fixId])-1*
               ("France"==knockout$Team2[fixId]))
    lambdas1 <- c1*(HM*exp(opt.optim.MLP$par[knockout$Team1Id[fixId]]-
                             opt.optim.MLP$par[knockout$Team2Id[fixId]]))
    lambdas1 <- lambdas1^(1-as.numeric(equalStrengths))
    lambdas2 <- c1*(exp(opt.optim.MLPI$par[knockout$Team2Id[fixId]]-
                          opt.optim.MLPI$par[knockout$Team1Id[fixId]])/HM)
    lambdas2 <- lambdas2^(1-as.numeric(equalStrengths))
    
    # Generate random outcomes
    knockout$sim1Goal[fixId] <- rpois(nbMatches,lambdas1)
    knockout$sim2Goal[fixId] <- rpois(nbMatches,lambdas2)
    
    ############################
    # Recalculate ML estimates #
    ############################
    
    # Append simulated data in the right format
    data <- rbind(data,data.frame(Date=knockout$Date[fixId],
                                  Home=knockout$Team1[fixId],
                                  Away=knockout$Team2[fixId],
                                  hgoal=knockout$sim1Goal[fixId],
                                  vgoal=knockout$sim2Goal[fixId],
                                  impScore=3,
                                  Neutral=abs(HM-1)<1e-10,
                                  homeId=knockout$Team1Id[fixId],
                                  visitId=knockout$Team2Id[fixId],
                                  days.passed=NA,
                                  stringsAsFactors = FALSE))
    
    # Next round start date
    nextRoundStart <- c(as.Date("30/06/2016", "%d/%m/%Y"),
                        as.Date("06/07/2016", "%d/%m/%Y"),
                        as.Date("10/07/2016", "%d/%m/%Y"),
                        NA)[j]
    startModelDate <- as.POSIXlt(nextRoundStart)
    startModelDate$year <- startModelDate$year-predictionPeriodYears
    startModelDate <- as.Date(startModelDate)
    
    # Calculate the number of days passed for all historical matches
    data$days.passed <- as.numeric(difftime(nextRoundStart,
                                            data$Date,units="days"))
    
    # Calculate the ML estimates
    dataIds <- which(data$Date>startModelDate)
    init <- rep(0,nbTeams+2)
    if(refitModelsThroughoutTournament && j<4){
      tryCatch({
      opt.optim.MLP <- optim(init,LPoiss,gr = NULL, data=data[dataIds,],
                             factorials=factorials,
                             halfPeriod=halfPeriod,method="BFGS")
      }, error=function(e){
        opt.optim.MLP <<- optim(init,LPoiss,gr = NULL,
                                data=data[dataIds,],
                                factorials=factorials,
                                halfPeriod=halfPeriod,method="CG")
      })
    }
    
    # Calculate the numerator and denominator terms of all fixtures
    c1 <- exp(opt.optim.MLP$par[nbTeams+1])
    H <- exp(opt.optim.MLP$par[nbTeams+2])^(1-as.numeric(equalStrengths))
    
    # Calculate the fixtures for the next round
    winIds <- knockout$sim1Goal[fixId]!=knockout$sim2Goal[fixId]
    drawIds <- which(!winIds)
    winIds <- which(winIds)
    
    # Randomly assign a winner if a draw after ninety minutes
    # To be elaborated if time allows it!!
    knockout$winner[fixId][winIds] <- ifelse(knockout$sim1Goal[fixId][winIds]>
                                               knockout$sim2Goal[fixId][winIds],
                                             knockout$Team1[fixId][winIds],
                                             knockout$Team2[fixId][winIds])
    knockout$winner[fixId][drawIds] <- ifelse(rnorm(length(drawIds))>0,
                                              knockout$Team1[fixId][drawIds],
                                              knockout$Team2[fixId][drawIds])
    
    # Set up fixtures for the next round
    if(j<4){
      homeMatchRows <- match(knockout$Match[fixId], knockout$Team1)
      validHomeMatchRows <- which(!is.na(homeMatchRows))
      knockout[homeMatchRows[validHomeMatchRows],"Team1"] <- 
        knockout$winner[fixId][validHomeMatchRows]
      
      visitMatchRows <- match(knockout$Match[fixId], knockout$Team2)
      validVisitMatchRows <- which(!is.na(visitMatchRows))
      knockout[visitMatchRows[validVisitMatchRows],"Team2"] <- 
        knockout$winner[fixId][validVisitMatchRows]
    }
  }
  
  # Append the simulation results to the output file in list format
  simulationSummary <- list(list(firstRound=firstRound,positions=positions,
                            knockout=knockout))
  simulations <- c(simulations,simulationSummary)
  
  # Save each saveEachSimulations iterations
  if(i==nbSims || (i %% saveEachSimulations)==0){
    save(simulations, file = simulationsSaveFile)
  }
}
