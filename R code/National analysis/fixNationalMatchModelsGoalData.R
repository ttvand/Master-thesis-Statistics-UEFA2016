# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis"))

# Load model and summary data
modelFile <- "predictProbsNat4FIFAWeightsPoissonP.RData"
# modelFile <- "predictProbsNat4FIFAWeightsPoisson.RData"
modelDataPath <- paste0(modelFile)
nationalDataPath <- paste0(basePath,"Scraper/internationalResultsEuropeP.RData")
load(modelDataPath)
load(nationalDataPath)
allMatches <- allMatches[allMatches$Date %in% summary$Date,]

# Combine data frames
summaryColumns <- 1:16
summaryNames <- names(summary)[summaryColumns]
summary[,7:8] <- summary[,8:9]
summary[,9] <- summary[,11]
summary[,10] <- summary[,12]
summary[,11] <- allMatches$impScore
summary[,12] <- summary[,13]
summary[,13] <- ifelse(summary$hgoal>summary$vgoal,"H",
                       ifelse(summary$vgoal>summary$hgoal,"A","D"))

summary <- summary[,-(17:19)]

# Write combined file to memory
save(summary, file = modelFile)
