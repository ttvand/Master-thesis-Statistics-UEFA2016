# Script to preprocess the international results file

# Clear workspace and setwd
rm(list=ls())
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/National analysis/Scraper")

# Date limit of periods of interest
dateLimit <- as.Date("1980-01-01")

# Name of the results file
resultsFile <- "internationalResultsEurope.RData"

# Name of the processed results file
processedResultsFile <- "internationalResultsEurope1980P.RData"

# Load required libraries
library(maps)
library(beepr)

###############################################################################

# Load the international results file
load(resultsFile)

# Convert Date column to date format
allMatches$Date <- as.Date(allMatches$Date,format="%d.%m.%Y")

# Remove matches of * teams (youth)
allMatches <- allMatches[!grepl("[*]",allMatches$Home) &
                           !grepl("[*]",allMatches$Away),]

# Remove all matches from non EU teams
euTeams <- unique(allMatches[allMatches$Comp %in% c("QE-15","QW-20"),"Home"])
allMatches <- allMatches[allMatches$Home %in% euTeams &
                           allMatches$Away %in% euTeams,]

# Extract home and away goals
allMatches$hgoal <- as.numeric(gsub(":.*","",allMatches$Result))
allMatches$vgoal <- as.numeric(gsub(".*:","",allMatches$Result))

# Inspect non duplicated matches - all good!
allMatches$key <- paste0(allMatches$Home,allMatches$Away,
                         allMatches$Date)
sortedKeys <- sort(table(allMatches$key))
head(sortedKeys)

# Delete duplicate match entries
allMatches <- allMatches[order(allMatches$key),]
allMatches <- allMatches[c(TRUE,allMatches$key[1:(nrow(allMatches)-1)] !=
                             allMatches$key[2:nrow(allMatches)]),]

# Order on the Date column
allMatches <- allMatches[order(allMatches$Date),]

# Remove entries older than a desired date
allMatches <- allMatches[allMatches$Date>=dateLimit,]

# Calculate the competition type
allMatches$CompType <- NA
allMatches$CompType[allMatches$Comp == "Friendly"] <- "Friendly"
allMatches$CompType[grepl("QE-.*",allMatches$Comp)] <- "EU Qual"
allMatches$CompType[grepl("EC-.*",allMatches$Comp)] <- "EU"
allMatches$CompType[grepl("QW-.*",allMatches$Comp)] <- "WC Qual"
allMatches$CompType[grepl("WC-.*",allMatches$Comp)] <- "WC"

# sort(table(allMatches$Comp[is.na(allMatches$CompType)]))
# Remainders: nordic championship, Cyprus international tournament,
# Rothmans tournament, Carling nations cup and other special friendlies
# Coded as friendlies!
allMatches$CompType[is.na(allMatches$CompType)] <- "Other friendly"
table(allMatches$CompType)

# Calculate the match importance
confCupIds <- grepl("^CCF-",allMatches$Comp)
allMatches$CompType[confCupIds] <- "Confederations cup"
allMatches$Importance <- "Low"
allMatches$Importance[grepl("Qual",allMatches$CompType)] <- "Medium"
allMatches$Importance[allMatches$CompType %in% c("EU","WC") |
                        confCupIds] <- "High"
table(allMatches$Importance)

# Use the FIFA scheme to calculate importance weights:
#   - Friendly match (including small competitions) 1.0
#   - FIFA World Cup qualifier or confederation-level qualifier 2.5
#   - Confederation-level final competition or FIFA Confederations Cup 3.0
#   - FIFA World Cup final competition 4.0
allMatches$impScore <- 1
allMatches$impScore[allMatches$CompType=="WC"] <- 4
allMatches$impScore[allMatches$CompType=="EU"] <- 3
allMatches$impScore[allMatches$Importance=="Medium"] <- 2.5
allMatches$impScore[confCupIds] <- 3

# Detect matches on neutral terrain. Approach: assume neutral terrain if home
# team has not played at the ground in one of the qualifiers.
# Try to match remaining matches using the cities database
allMatches$Neutral <- TRUE
for(i in 1:length(euTeams)){
  team <- euTeams[i]
  homeQualIds <- allMatches$Importance!="High" & allMatches$Home==team &
    allMatches$CompType != "Other friendly"
  homeGrounds <- unique(allMatches[homeQualIds,"Venue"])
  allMatches[allMatches$Home==team & (allMatches$Venue %in% homeGrounds),
             "Neutral"] <- FALSE
}
cities <- world.cities
neutralIds <- which(allMatches$Neutral)
sapply(neutralIds,function(x){
  homeCountry <- allMatches$Home[x]
  matchCity <- allMatches$Venue[x]
  cityIds <- which(matchCity==cities$name)
  if(homeCountry %in% cities$country.etc[cityIds]){
    allMatches$Neutral[x] <<- FALSE
  }
})
table(allMatches$Neutral)

# Add competition and goals variables
allMatches$link <- NA
allMatches$Competition <- NA
allMatches$Goals <- NA

# Save processed results file
rownames(allMatches) <- 1:nrow(allMatches)
save(allMatches, file = processedResultsFile)
beep(sound="fanfare")