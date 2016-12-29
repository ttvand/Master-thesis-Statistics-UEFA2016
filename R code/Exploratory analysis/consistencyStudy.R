# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"Exploratory analysis"))

# Load the required libraries
library(ggplot2)

# Set the seed in order to obtain reproducible results
set.seed(14)

# Simulation parameters
nbSims <- 10
ns <- 10*2^c(6:11)
maxTeams <- 52
groupErrors <- TRUE

# Option to skip the analysis sections
skipBT <- TRUE
skipPoisson <- TRUE
skipElo <- FALSE

# Path to the maximum likelihood functions
mlPath <- paste0(basePath,"Common files/mlFunctions.R")

# Path to the elo update logic
eloLogicPath <- paste0(basePath,"Common files/eloUpdate.R")

# Path to the elo to outcome polr model
eloToOutcomePath <- paste0(basePath,"Common files/eloToOutcome.RData")

# Path to the national team match results
nationalDataPath <- paste0(basePath,"National analysis/Scraper/internationalResultsEuropeP.RData")

# Source the base ML functions
source(mlPath)

# Load the rating difference to outcome prob POLR model
source(eloLogicPath)
load(eloToOutcomePath)

# Extract the national team names
load(nationalDataPath)
teams <- sort(unique(c(allMatches[["Home"]],allMatches[["Away"]])))
teams <- teams[1:min(c(length(teams),maxTeams))]
nbTeams <- length(teams)


###################################
# Bradley-Terry consistency study #
###################################

if(!skipBT){
  # Generate the true strengths
  BTParameters <- lapply(1:nbSims,function(x){
    # Uniform strengths between 1/2 and 2
    strengths <- exp(runif(nbTeams,min=-3,max=3))
    K <- runif(1,min=0.8,max=1)
    H <- runif(1,min=1,max=1.5)
    c(strengths,K,H)
  })
  
  # Extract the mean absolute parameter errors 
  BTParamErrors <- lapply(ns,function(x){
    sapply(BTParameters,function(y){
      # Write the simulation step
      cat("BT n =",x,"\n")
      
      # Generate x matches with y true parameter strengths
      matches <- sapply(1:x,function(z) sample(teams,2))
      
      # Extract the parameter pieces
      strengths <- y[1:nbTeams]
      K <- y[nbTeams+1]
      H <- y[nbTeams+2]
      
      # Set up the data frame of the matches
      data <- data.frame(Team1=matches[1,],
                         Team2=matches[2,],
                         Neutral=FALSE,
                         days.passed=0,
                         impScore=1)
      
      # Add the team ids to the data
      data$homeId <- match(data$Team1,teams)
      data$visitId <- match(data$Team2,teams)
      
      # Calculate the relative outcome probabilities
      num1 <- H*strengths[data$homeId]
      num2 <- K*sqrt(H*strengths[data$homeId]*strengths[data$visitId])
      num3 <- strengths[data$visitId]
      denom <- num1 + num2 + num3
      
      # Calculate the outcome probabilities
      pH <- num1/denom
      pD <- num2/denom
      
      # Simulate random outcomes
      randomProbs <- runif(x)
      outcomes <- ifelse(randomProbs<pH,"H",
                         ifelse(randomProbs<(pH+pD),"D","A"))
      
      # Generate random outcomes
      data$result <- outcomes
      
      # Estimate the parameters
      init <- rep(0,nbTeams+2)
      tryCatch({
        opt.optim.MLBT <- optim(init,LBT,gr = NULL, data=data,
                                halfPeriod=Inf,method="BFGS")
      }, error=function(e){
        opt.optim.MLBT <<- optim(init,LBT,gr = NULL, data=data,
                                 halfPeriod=Inf,method="CG")
      })
      
      # Rescale the strength parameters
      meanMLStrengths <- mean(exp(opt.optim.MLBT$par[1:nbTeams]))
      y[1:nbTeams] <- y[1:nbTeams]/mean(y[1:nbTeams])*meanMLStrengths
      
      # Return the difference between the simulated and estimated parameters
      out <- abs(exp(opt.optim.MLBT$par)-y)
      if(groupErrors){
        out <- mean(out)
      } 
      
      list(out)
    })
  })
  
  # Convert the mean absolute parameter errors to a data frame
  BTParErrors <- data.frame(#Actual=unlist(BTParameters),
    Error=unlist(BTParamErrors),
    N=factor(rep(ns,each=nbSims*
                   (1+(nbTeams+1)*(1-groupErrors)))))
  BTParErrors$Error[BTParErrors$Error>8] <- NA
  
  # Generate the BT consistency plot
  BTPlot <- ggplot(BTParErrors,aes(x=N,y=Error,fill=N))+
    geom_boxplot(na.rm=TRUE) +
    ylab("Mean absolute parameter error") +
    expand_limits(y=0) +
    ggtitle("Bradley-Terry consistency study") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          title=element_text(size=14,face="bold"),
          axis.text.x = element_text(angle=0,hjust=0,vjust=0))
}


#############################
# Poisson consistency study #
#############################

if(!skipPoisson){
  # Generate the true strengths
  poissonParameters <- lapply(1:nbSims,function(x){
    # Uniform strengths between 1/2 and 2
    strengths <- runif(nbTeams,min=1/2,max=2)
    c1 <- runif(1,min=1,max=1.5)
    H <- runif(1,min=1,max=1.5)
    log(c(strengths,c1,H))
  })
  
  # Calculate factorials beforehand to speed up the poisson ML estimation
  factorials <- factorial(0:20)
  
  # Extract the mean absolute parameter errors 
  poissonParamErrors <- lapply(ns,function(x){
    sapply(poissonParameters,function(y){
      # Write the simulation step
      cat("Poisson n =",x,"\n")
      
      # Generate x matches with y true parameter strengths
      matches <- sapply(1:x,function(z) sample(teams,2))
      
      # Extract the parameter pieces
      y <- exp(y)
      strengths <- y[1:nbTeams]
      c1 <- y[nbTeams+1]
      H <- y[nbTeams+2]
      
      # Set up the data frame of the matches
      data <- data.frame(Team1=matches[1,],
                         Team2=matches[2,],
                         Neutral=FALSE,
                         days.passed=0,
                         impScore=1)
      
      # Add the team ids to the data
      data$homeId <- match(data$Team1,teams)
      data$visitId <- match(data$Team2,teams)
      
      # Simulate match results
      lambdas1 <- c1*H*strengths[data$homeId]/strengths[data$visitId]
      lambdas2 <- c1*strengths[data$visitId]/strengths[data$homeId]/H
      
      # Generate random outcomes
      data$hgoal <- rpois(x,lambdas1)
      data$vgoal <- rpois(x,lambdas2)
      
      # Estimate the parameters
      init <- rep(0,nbTeams+2)
      tryCatch({
        opt.optim.MLP <- optim(init,LPoiss,gr = NULL, data=data,
                               factorials=factorials,
                               halfPeriod=Inf,method="BFGS")
      }, error=function(e){
        opt.optim.MLP <<- optim(init,LPoiss,gr = NULL, data=data,
                                factorials=factorials,
                                halfPeriod=Inf,method="CG")
      })
      
      # Rescale the strength parameters
      meanMLStrengths <- mean(exp(opt.optim.MLP$par[1:nbTeams]))
      y[1:nbTeams] <- y[1:nbTeams]/mean(y[1:nbTeams])*meanMLStrengths
      
      # Return the difference between the simulated and estimated parameters
      out <- abs(exp(opt.optim.MLP$par)-y)
      if(groupErrors){
        out <- mean(out)
      } 
      
      list(out)
    })
  })
  
  # Convert the mean absolute parameter errors to a data frame
  poissonParErrors <- data.frame(#Actual=unlist(poissonParameters),
    Error=unlist(poissonParamErrors),
    N=factor(rep(ns,
                 each=nbSims*(1+(nbTeams+1)*
                                (1-groupErrors)))))
  
  # Generate the Poisson consistency plot
  poissPlot <- ggplot(poissonParErrors,aes(x=N,y=Error,fill=N))+
    geom_boxplot(na.rm=TRUE) +
    ylab("Mean absolute parameter error") +
    expand_limits(y=0) +
    ggtitle("Poisson consistency study") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          title=element_text(size=14,face="bold"),
          axis.text.x = element_text(angle=0,hjust=0,vjust=0))
}


#########################
# Elo consistency study #
#########################

if(!skipElo){
  # Generate the true Elo ratings
  EloParameters <- lapply(1:nbSims,function(x){
    # Uniform strengths between 1/2 and 2
    strengths <- runif(nbTeams,min=-300,max=300)
  })
  
  # Update constant
  basisK <- 15*2.5
  
  # Extract the mean absolute parameter errors 
  EloParamErrors <- lapply(ns,function(x){
    sapply(EloParameters,function(y){
      # Write the simulation step
      cat("Elo n =",x,"\n")
      
      # Generate x matches with the true Elo strengths
      matches <- sapply(1:x,function(z) sample(teams,2))
      
      # Extract the parameter pieces
      trueEloRatings <- y
      
      # Set up the data frame of the matches
      data <- data.frame(Team1=matches[1,],
                         Team2=matches[2,],
                         Neutral=FALSE,
                         impScore=1)
      
      # Add the team ids to the data
      data$homeId <- match(data$Team1,teams)
      data$visitId <- match(data$Team2,teams)
      
      # Simulate match results
      homeElo <- trueEloRatings[data$homeId] + 100
      awayElo <- trueEloRatings[data$visitId]
      
      # Generate random outcomes
      ratingDifferencesDf <- data.frame(eloDifference=homeElo-awayElo)
      polrPredict <- predict(fitPolr,type="probs",
                             newdata = ratingDifferencesDf)
      randomValues <- runif(nrow(data))
      randomOutcomes <- ifelse(randomValues<polrPredict[,1],"H",
                               ifelse(randomValues<polrPredict[,1]+
                                        polrPredict[,2],"D","A"))
      data$hgoal <- as.numeric(randomOutcomes!="A")
      data$vgoal <- as.numeric(randomOutcomes!="H")
      
      eloRatings <- rep(0,nbTeams)
      # Update the Elo ratings
      for(matchId in 1:nrow(data)){
        dataRow <- data[matchId,]
        
        # Extract the team ids
        firstId <- dataRow$homeId
        secondId <- dataRow$visitId
        
        # Update calculation of the Elo rating of the first team
        firstEloUpdate <- eloUpdate(eloRatings[firstId],
                                    eloRatings[secondId],
                                    dataRow$impScore*basisK,
                                    dataRow$Neutral,
                                    dataRow$hgoal,
                                    dataRow$vgoal)
        eloRatings[firstId] <- eloRatings[firstId] + firstEloUpdate
        eloRatings[secondId] <- eloRatings[secondId] - firstEloUpdate
      }
      
      # Return the difference between the simulated and estimated Elo ratings
      out <- abs(eloRatings-trueEloRatings)
      if(groupErrors){
        out <- mean(out)
      } 
      
      list(out)
    })
  })
  
  # Convert the mean absolute parameter errors to a data frame
  EloRatingErrors <- data.frame(#Actual=unlist(poissonParameters),
    Error=unlist(EloParamErrors),
    N=factor(rep(ns,
                 each=nbSims*(1+(nbTeams+1)*
                                (1-groupErrors)))))
  
  # Generate the Poisson consistency plot
  EloPlot <- ggplot(EloRatingErrors,aes(x=N,y=Error,fill=N))+
    geom_boxplot(na.rm=TRUE) +
    ylab("Mean absolute rating error") +
    expand_limits(y=0) +
    ggtitle("Elo consistency study") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          title=element_text(size=14,face="bold"),
          axis.text.x = element_text(angle=0,hjust=0,vjust=0))
}

# Group both plots
if(!skipBT) BTPlot
if(!skipPoisson) poissPlot
if(!skipElo) EloPlot
