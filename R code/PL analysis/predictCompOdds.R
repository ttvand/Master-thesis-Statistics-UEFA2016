# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/PL analysis/")

# summaryFile <- "predictComp.RData"
summaryFile <- "predictCompSummaryProbs.RData"
summaryFile <- "predictCompSummaryProbsElo.RData"

# Odds folder
oddsFolder <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/Literature/Premier league odds"

# Name of the odds summary csv file
oddsSummaryFile <- "summary.csv"

# Name of the bookmakers file
bookmakersFile <- "bookmakers.txt"

# Team names file (short and long format)
teamNamesFile <- "teamNames.csv" 

# File name of the merged predictions and bookmaker odds
saveFileName <- "mergedOddsElo.RData"

# Initial columns without odds data
initCols <- 29

###############################################################################

# Read in the prediction data
load(summaryFile)

# Read in the odds data
odds <- read.csv(file.path(oddsFolder,oddsSummaryFile))

# Calculate the bookmaker abbreviations
bookmakersNames <- names(odds[-(1:initCols)])
a <- data.frame(data=bookmakersNames,stringsAsFactors = FALSE)
bookmakers <- unique(substr(a$data,1,nchar(a$data)-1))
bookmakersRaw <- read.table(file.path(oddsFolder,bookmakersFile),
                            sep = "=", stringsAsFactors = FALSE)
bookmakersRaw[,1] <- gsub(" ","",bookmakersRaw[,1])
bookmakersRaw[,2] <- gsub(" ","",bookmakersRaw[,2])
bookmakerIds <- match(bookmakers,bookmakersRaw[,1])

nbBookmakers <- length(bookmakers)

# Calculate the season of the odds data
odds$Date <- as.Date(odds$Date,format="%Y-%m-%d")
odds$Season <- as.numeric(format(odds$Date,'%Y')) + 
  (as.numeric(format(odds$Date,'%m'))>6)-1

# Add the probability of actual outcomes to the summary
for(i in 1:nbBookmakers){
  safetyMargin <- rowSums(1/odds[,paste0(bookmakers[i],c("H","D","A"))])
  probabilities <- 1/odds[,paste0(bookmakers[i],c("H","D","A"))]/safetyMargin
  outcomeProbs <- ifelse(odds$FTR=="H",probabilities[,1],
                         ifelse(odds$FTR=="D",probabilities[,2],
                                probabilities[,3]))
  odds[[paste0("outcome",bookmakers[i])]] <- outcomeProbs
}

# Convert the odds team names to the long format
teamNames <- read.csv(teamNamesFile)
odds$HomeTeam <- teamNames[match(odds$HomeTeam,teamNames[,1]),2]
odds$AwayTeam <- teamNames[match(odds$AwayTeam,teamNames[,1]),2]

# Adjust the names of the summary file
names(summary)[names(summary) %in% c("team1","team2","season")] <-
  c("HomeTeam","AwayTeam","Season")

# Drop majority columns of summary
summary <- summary[,!grepl("majProb",names(summary))]

# Merge odds and prediction data files
merged <- merge(odds,summary)

# Flag the season matches left for each team for each season
merged <- merged[order(merged$Date),]
merged$homeLeft <- NA
merged$awayLeft <- NA
seasons <- as.numeric(names(table(merged$Season)))
for(i in 1:length(seasons)){
  season <- seasons[i]
  teams <- names(table(as.character(merged[merged$Season==season,"HomeTeam"])))
  for(j in 1:length(teams)){
    team <- teams[j]
    gameId <- which(merged$Season == season &
                      (team==merged$HomeTeam | team==merged$AwayTeam))
    nbGames <- length(gameId)
    gameHId <- merged$HomeTeam[gameId]==team
    merged$homeLeft[gameId[gameHId]] <- seq(nbGames,1,-1)[gameHId]
    merged$awayLeft[gameId[!gameHId]] <- seq(nbGames,1,-1)[!gameHId]
  }
}

save(merged, file = saveFileName)

####################
# Model comparison #
####################

# Compare best bookmaker to current best model
bestBookie <- "SJ"
worstBookie <- "IW"
# bestModel <- "probP_180_0.02"
bestModel <- "probElo_26_0.16"

# Find the model with the highest correlation with the best bookie
correlations <- sapply(names(summary)[grep("_0$",names(summary))],function(x){
  cor(merged[,x],merged[,paste0("outcome",bestBookie)],use="complete.obs")
})
correlations <- data.frame(correlations) 

# Drop irrelevant columns of the merged file 
comparison <- merged[,c(1:7,which(names(merged)==paste0("outcome",bestBookie)),
                        which(names(merged)==paste0("outcome",worstBookie)),
                        which(names(merged)==bestModel))]

# Drop rows with missing data
comparison <- comparison[!(is.na(comparison[,8])|is.na(comparison[,9])),]

# Compare predicted outcomes
plot(comparison[,c(8,9)],xlim=c(0,1),ylim=c(0,1))
plot(comparison[,c(8,10)],xlim=c(0,1),ylim=c(0,1))
cor(comparison[,c(8,9,10)])

sapply(comparison[,c(8,9,10)],function(x){-mean(log(x))})
sapply(comparison[,c(8,9,10)],function(x){mean(x)})

# Study matches with the largest difference between the bookie and best model
difference <- comparison[,10]-comparison[,8]
plot(difference)
diffOrder <- order(abs(difference),decreasing = TRUE)

ordered <- comparison[diffOrder,]

# Can we beat the best bookmaker using a smart strategy?
oddsH <- merged[,paste0(bestBookie,"H")]
oddsD <- merged[,paste0(bestBookie,"D")]
oddsA <- merged[,paste0(bestBookie,"A")]
expProfitH <- oddsH*merged[,paste0(bestModel,"_H")]-1
expProfitD <- oddsD*merged[,paste0(bestModel,"_D")]-1
expProfitA <- oddsA*(1-merged[,paste0(bestModel,"_H")]-
                       merged[,paste0(bestModel,"_D")])-1

minProfitToBet <- 0.2
maxOddsToBet <- 5
possibleBetIds <- apply(cbind(oddsH,oddsD,oddsA),1,max)<maxOddsToBet
possibleBetIds[is.na(possibleBetIds)] <- FALSE
possibleBetRows <- which(possibleBetIds)
betHIds <- possibleBetRows[expProfitH[possibleBetIds]>minProfitToBet]
betDIds <- possibleBetRows[expProfitD[possibleBetIds]>minProfitToBet]
betAIds <- possibleBetRows[expProfitA[possibleBetIds]>minProfitToBet]

# Assess the actual return for the selected bets
hProfit <- sum(oddsH[betHIds]*(merged$FTR[betHIds]=="H"))-length(betHIds)
dProfit <- sum(oddsD[betDIds]*(merged$FTR[betDIds]=="D"))-length(betDIds)
aProfit <- sum(oddsA[betAIds]*(merged$FTR[betAIds]=="A"))-length(betAIds)
