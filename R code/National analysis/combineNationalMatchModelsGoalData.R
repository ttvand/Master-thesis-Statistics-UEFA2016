# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis"))

# Load model and summary data
modelFile <- "predictProbsNat4FIFAWeightsPoissonP.RData"
# modelFile <- "predictProbsNat4FIFAWeightsPoisson.RData"
nationalDataPath <- paste0(basePath,"Scraper/internationalResultsEuropePG.RData")
modelDataPath <- paste0(modelFile)
load(nationalDataPath)
load(modelDataPath)

# Subset summary data of all matches on matching keys
firstKey <- paste0(summary$Date,summary$home,summary$visitor)
secondKey <- paste0(allMatches$Date,allMatches$Home,allMatches$Away)
matchIds <- match(firstKey,secondKey)
allMatches <- allMatches[matchIds,]

# Combine data frames
dropColumnsSummary <- 1:13
summaryNames <- names(summary)[dropColumnsSummary]
summary <- cbind(allMatches,summary[,-dropColumnsSummary])
names(summary)[dropColumnsSummary] <- summaryNames

# Write combined file to memory
save(summary, file = modelFile)
