# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis"))

# Load model and summary data
modelFile <- "predictProbsNat4FIFAWeightsPoissonS.RData"
# modelFile <- "predictProbsNat4FIFAWeightsPoisson.RData"
modelDataPath <- paste0(modelFile)
nationalDataPath <- paste0(basePath,"National analysis/Scraper/internationalResultsEuropeP.RData")
load(modelDataPath)
load(nationalDataPath)

# Subset the all matches data set on the columns of interest
allMatches <- allMatches[,c("Date","Home","Away")]

# Count the historical match counts for the matches
allMatches$homeMatchCount <- 0
allMatches$awayMatchCount <- 0
for(i in 2:nrow(allMatches)){
  homeTeam <- allMatches$Home[i]
  awayTeam <- allMatches$Away[i]
  homeCount <- sum(allMatches$Home[1:(i-1)]==homeTeam |
                     allMatches$Away[1:(i-1)]==homeTeam)
  awayCount <- sum(allMatches$Home[1:(i-1)]==awayTeam |
                     allMatches$Away[1:(i-1)]==awayTeam)
  allMatches$homeMatchCount[i] <- homeCount
  allMatches$awayMatchCount[i] <- awayCount
}

# Combine data frames
allMatches <- allMatches[allMatches$Date>=summary$Date[1],]
summary$homeMatchCount <- allMatches$homeMatchCount
summary$awayMatchCount <- allMatches$awayMatchCount

# Write combined file to memory
save(summary, file = modelFile)