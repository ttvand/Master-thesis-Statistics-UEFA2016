# rm(list=ls())
# 
# basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
# nationalDataPath <- paste0(basePath,"Scraper/internationalResultsEuropeP.RData")
# modelDataPath <- paste0(basePath,"National analysis/predictProbsNat4FIFAWeights.RData")
# 
# load(nationalDataPath)
# allMatches <- allMatches[-(1:659),c(1:6,8:9,11:14)]
# allMatches$result <- ifelse(allMatches$hgoal>allMatches$vgoal,"H",
#                             ifelse(allMatches$hgoal==allMatches$vgoal,"D","A"))
# names(allMatches)[c(4,6)] <- c("home","visitor")
# 
# load(modelDataPath)
# summary$key <- NULL
# 
# summary <- cbind(allMatches,summary[,-(1:4)])
# targetSaveFile <- "predictProbsNat4FIFAWeights.RData"
# save(summary, file = targetSaveFile)

# rm(list=ls())
# basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/National analysis/"
# load(paste0(basePath,"predictProbsNat4FIFAWeights.RData"))
# a <- summary
# a <- a[,c(4,6,1,13,14:ncol(a))]
# names(a)[1:4] <- c("team1","team2","Date","result")
# load(paste0(basePath,"predictProbsNat4Part1.RData"))
# b <- summary
# load(paste0(basePath,"predictProbsNat4Part2.RData"))
# c <- summary
# c <- c[-(1:781),]
# 
# d <- rbind(b,c)
# d$key <- paste0(d$team1,d$team2,d$Date)
# a$key <- paste0(a$home,a$visitor,a$Date)
# e <- rbind(d,a[which(!a$key %in% d$key),])
# summary <- e[order(e$Date),]
# targetSaveFile <- "predictProbsNat4FIFAWeights.RData"
# save(summary, file = targetSaveFile)