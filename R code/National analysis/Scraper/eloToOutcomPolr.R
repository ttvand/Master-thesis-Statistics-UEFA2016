# Clear workspace and setwd
rm(list=ls())
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis/Scraper"))

# Load required libraries
library(MASS)
library(ggplot2)

# Date limit of the Elo convergence period and the POLR model
dateLimitElo <- as.Date("1986-01-01")
dateLimitPolr <- as.Date("1992-01-01")

# Name of the results file
resultsFile <- "internationalResultsEurope1980P.RData"

# Path to the elo update logic
eloLogicPath <- paste0(basePath,"Common files/eloUpdate.R")

###############################################################################

# Load the international results file
load(resultsFile)

# Source the elo update logic
source(eloLogicPath)

# Rename data
data <- allMatches
names(data)[names(data)=="Home"] <-"home"
names(data)[names(data)=="Away"] <-"visitor"
rm(allMatches)

# Preprocess data
data$year <- as.numeric(format(data$Date,"%Y"))
teams1980 <- unique(sort(c(data$home[data$year==1980],
                           data$visitor[data$year==1980])))
data <- data[data$home %in% teams1980 & data$visitor %in% teams1980,]
teams <- unique(sort(c(data$home,data$visitor)))
nbTeams <- length(teams)
data$homeId <- match(data$home,teams)
data$visitId <- match(data$visitor,teams)
data$eloDifference <- NA
data$Outcome <- ifelse(data$hgoal>data$vgoal,"H",
                       ifelse(data$hgoal==data$vgoal,"D","A"))
data$Outcome <- factor(data$Outcome,levels=c("H","D","A"))

# Calculate the elo ratings
teamRatings <- rep(0,nbTeams)
basisK <- 15
for(j in 1:nrow(data)){
  dataRow <- data[j,]
  if(dataRow$Date>=dateLimitElo){
    break
  }
  firstId <- dataRow$homeId
  secondId <- dataRow$visitId
  # Update of the Elo rating of the first team
  firstEloUpdate <- eloUpdate(teamRatings[firstId],
                              teamRatings[secondId],
                              dataRow$impScore*basisK,
                              dataRow$Neutral,
                              dataRow$hgoal,
                              dataRow$vgoal)
  teamRatings[firstId] <- teamRatings[firstId] + firstEloUpdate
  teamRatings[secondId] <- teamRatings[secondId] - firstEloUpdate
}

# Update the elo ratings
dataPolr <- data[data$Date>=dateLimitElo,]
for(j in 1:nrow(dataPolr)){
  dataRow <- dataPolr[j,]
  if(dataRow$Date>=dateLimitPolr){
    break
  }
  firstId <- dataRow$homeId
  secondId <- dataRow$visitId
  # Update of the Elo rating of the first team
  firstEloUpdate <- eloUpdate(teamRatings[firstId],
                              teamRatings[secondId],
                              dataRow$impScore*basisK,
                              dataRow$Neutral,
                              dataRow$hgoal,
                              dataRow$vgoal)
  dataPolr$eloDifference[j] <- teamRatings[firstId]-teamRatings[secondId]+
    100*(!dataRow$Neutral)
  teamRatings[firstId] <- teamRatings[firstId] + firstEloUpdate
  teamRatings[secondId] <- teamRatings[secondId] - firstEloUpdate
}

# Fit the polr model for the available date range
dataPolr <- dataPolr[!is.na(dataPolr$eloDifference),]
eloDifferences <- -500:500
testData <- data.frame(eloDifference=eloDifferences)
fitPolr <- polr(Outcome ~ eloDifference, data=dataPolr)

# Center the model at a rating difference of zero
fitPolr$zeta <- fitPolr$zeta-mean(fitPolr$zeta)

polrPredict <- predict(fitPolr,type="probs",newdata = testData)
rownames(polrPredict) <- eloDifferences
colnames(polrPredict) <- c("homeWin","draw","homeLoss")
polrPredict <- as.data.frame(polrPredict)
polrPredict$eloDifferences <- eloDifferences

# Save the Polr model
save(fitPolr,file=paste0(basePath,"Common files/eloToOutcome.RData"))

p <- ggplot(data=polrPredict, aes(x=eloDifferences)) +
  geom_line(aes(y = homeWin, colour = "blue"), size = 2) +
  geom_line(aes(y = draw, colour = "red"), size = 2) +
  geom_line(aes(y = homeLoss, colour = "green"), size = 2) +
  ggtitle("Elo outcome probabilities versus rating difference") +
  xlab("Elo rating difference") +
  ylab("Outcome probability") +
  scale_colour_discrete(name="Match outcome",
                        breaks=c("blue", "red", "green"),
                        labels=c("Home win", "Draw", "Home loss")) +
  theme(axis.text=element_text(size=12),
        legend.text=element_text(size=11),
        axis.title=element_text(size=14), #,face="bold"
        title=element_text(size=14)) #,face="bold"

p
