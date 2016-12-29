# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"EURO 2016 simulation study"))

# Load the required libraries
library(ggplot2)

# Path to save the simulation results
permuted <- TRUE
if(permuted){
  simulationsSaveFile <- "simulationResultsPermuted.RData"
  permutedString <- "Perm"
  countryTitleAdd <- "(Permuted)"
} else{
  simulationsSaveFile <- "simulationResultsNoRefitMar26.RData"
  permutedString <- ""
  countryTitleAdd <- "(Actual)"
}

# Load the simulation results
load(simulationsSaveFile)

# Extract the number of simulations
nbSims <- length(simulations)
nbSimsText <- prettyNum(nbSims,big.mark=",",scientific=FALSE)

# Obtain the team names and total team count
allTeams <- sort(unique(c(simulations[[1]]$firstRound$Team1,
                          simulations[[1]]$firstRound$Team2)))
nbTeams <- length(allTeams)

# Placeholder for all simulation results
results <- vector("list", nbTeams)
names(results) <- allTeams

# Loop over all teams and calculate their simulated final outcomes
for(teamId in 1:nbTeams){
  cat("Processing team",teamId,"of",nbTeams,"\n")
  
  # National team of interest
  targetTeam <- allTeams[teamId]
  
  # Outcome of all simulations
  teamOutcomes <- factor(rep(NA,nbSims),levels=c("Fourth in group",
                                                 "Third in group",
                                                 "Round of 16",
                                                 "Quarter finals",
                                                 "Semi finals",
                                                 "Finalist",
                                                 "Winner"))
  
  # Loop over all simulations and store the final team outcomes
  for(i in 1:nbSims){
    simulation <- simulations[[i]]
    knockout <- simulation$knockout
    positions <- simulation$positions
    
    # Check if the team made it to the knockout phase
    if(targetTeam %in% c(knockout$Team1,knockout$Team2)){
      # Knockout analysis
      nbKnockoutWins <- sum(knockout$winner==targetTeam)
      outcome <- ifelse(nbKnockoutWins==0,"Round of 16",
                        ifelse(nbKnockoutWins==1,"Quarter finals",
                               ifelse(nbKnockoutWins==2,"Semi finals",
                                      ifelse(nbKnockoutWins==3,"Finalist",
                                             "Winner"))))
    } else{
      # Group phase elimination analysis
      if(targetTeam %in% positions[3,]){
        outcome <- "Third in group"
      } else{
        outcome <- "Fourth in group"
      }
    }
    teamOutcomes[i] <- outcome
  }
  results[[targetTeam]] <- teamOutcomes
}

# Plot and store the simulated outcomes for all teams
for(i in 1:nbTeams){
  # targetTeam <- c("Belgium","Germany")[1]
  targetTeam <- allTeams[i]
  targetTeamOutcomes <- data.frame(Outcome=results[[targetTeam]])
  teamPlot <- ggplot(targetTeamOutcomes,aes(x=Outcome,fill=Outcome))+
    geom_bar() +
    ylim(c(0,max(table(targetTeamOutcomes))*1.1)) +
    geom_text(stat='count',aes(label=..count..),vjust=-0.5) +
    ylab("Count") +
    ggtitle(paste0("Simulated outcomes for ",targetTeam," ",countryTitleAdd)) + #," (",nbSimsText," simulations)"
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12,face="bold"),
          legend.text=element_text(size=10),
          legend.title=element_text(size=12,face="bold"),
          legend.position="none",
          title=element_text(size=12,face="bold"),
          axis.text.x = element_text(angle=45,hjust=1))
  # if(i==1) browser()
  ggsave(file.path(getwd(),"Plots",paste0(gsub(" ","",targetTeam),
                                          permutedString,".png")),
         plot=teamPlot)
}

# Extract all winners
allWinners <- unlist(lapply(allTeams,function(x){
  outcome=results[[x]]
  rep(x,sum(outcome=="Winner"))
}))

# Extract teams that one at least once
winnerTeams <- sort(unique(allWinners))
otherTeams <- c(allTeams[!allTeams %in% winnerTeams])#,"Netherlands")

# Sort the winners based on the frequency count
allWinners <- factor(allWinners,levels=c(winnerTeams[order(table(allWinners),
                                                           decreasing = TRUE)],
                                         otherTeams))

# Display a bar chart of the simulated winners
winners <- data.frame(Winner=allWinners)
winPlot <- ggplot(winners,aes(x=Winner,fill=Winner))+
  geom_bar() +
  scale_x_discrete(drop=FALSE) +
  xlab("National team") + 
  ylab("Count") +
  ggtitle(paste0("EURO 2016 winners (Permuted draw)")) + #,nbSimsText," simulations)")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"),
        legend.position="none",
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

teamPlot
winPlot
