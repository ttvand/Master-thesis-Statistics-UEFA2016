# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/National analysis"
setwd(basePath)

# Load model and summary data
model1File <- "predictProbsNat4FIFAWeights.RData"
model2File <- "predictProbsNatEBT4FIFAWeightsS.RData"
# modelFile <- "predictProbsNat4FIFAWeightsPoisson.RData"
load(model1File)
summary1 <- summary
rm(summary)
load(model2File)
summary2 <- summary
rm(summary)

# Combine data frames
summary <- cbind(summary1,summary2[,-(1:13)])

# Write combined file to memory
save(summary, file = model1File)