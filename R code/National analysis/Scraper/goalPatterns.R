# Script to analyze goal scoring patterns

# Clear workspace and setwd
rm(list=ls())
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/Scraper")

# Name of the loaded file
resultsFile <- "internationalResultsEuropePG.RData"

# Load required libraries
library(maps)

###############################################################################

# Load the international results file
load(resultsFile)

# Extract the timing of all goals
timings <- lapply(allMatches$Goals,function(x){
  if(is.na(x)){
    out <- c()
  } else{
    parts <- unlist(strsplit(x,","))
    parts <- gsub("\\(pen\\.\\)|\\(own goal\\)|\\(golden goal\\)","",parts)
    out <- gsub(".*\\(|\\.\\).*","",parts)
  }
  out
})

# Calculate the timing of the last goal
lastGoal <- sapply(timings,function(x) max(as.numeric(gsub("\\+.*$","",x))))
allMatches$ID <- 1:nrow(allMatches)
allMatches$lastGoal <- lastGoal

#######################
# Study goal patterns #
#######################

# Study tournament goals
tournamentGoals <- timings[allMatches$Importance=="High"]

# Analyze goals when dropping extra time goals (at least according to the data)
goals <- as.numeric(gsub("+","",unlist(tournamentGoals)))
goals <- goals[!is.na(goals) & goals <=90]
hist(goals,50)
barplot(table(goals))

# Compare goals in first half to goals in second half excluding 45th and 90th
# minutes
firstGoals <- sum(goals<45)
secondGoals <- sum(goals>45 & goals<90)
pbinom(firstGoals,firstGoals+secondGoals,0.5) # First and second not independent

# Study non tournament goals (typically no extra time)
nonTournamentGoals <- timings[allMatches$Importance!="High" &
                                allMatches$CompType != "Other friendly"]

# Analyze goals when dropping extra time goals (at least according to the data)
goals <- as.numeric(gsub("+","",unlist(nonTournamentGoals)))
goals <- goals[!is.na(goals) & goals <=90]
hist(goals,50)
barplot(table(goals))

# Compare goals in first half to goals in second half excluding 45th and 90th
# minutes
firstGoals <- sum(goals<45)
secondGoals <- sum(goals>45 & goals<90)
pbinom(firstGoals,firstGoals+secondGoals,0.5) # First and second not independent