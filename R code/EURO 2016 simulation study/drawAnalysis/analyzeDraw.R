# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"EURO 2016 simulation study/drawAnalysis"))

# Load the required libraries
library(ggplot2)

# Path to save the simulation results
simulationsFileActual <- "simulationResultsNoRefitMar26.RData"
simulationsFilePermuted <- "simulationResultsPermuted.RData"

# Load the simulation results
load(simulationsFileActual)
simulationsActual <- simulations
load(simulationsFilePermuted)
simulationsPermuted <- simulations
rm(simulations)

# Extract the number of simulations
nbSimsActual <- length(simulationsActual)
nbSimsPermuted <- length(simulationsPermuted)
nbSimsText <- prettyNum(nbSimsPermuted,big.mark=",",scientific=FALSE)

# Obtain the team names and total team count
allTeams <- sort(unique(c(simulationsActual[[1]]$firstRound$Team1,
                          simulationsActual[[1]]$firstRound$Team2)))
nbTeams <- length(allTeams)

# Extract the winners from both simulation files
winnersActual <- unlist(lapply(simulationsActual,function(x)
  x$knockout$winner[15]))
winnersPermuted <- unlist(lapply(simulationsPermuted,function(x)
  x$knockout$winner[15]))

# Extract the winner counts
winnersActualT <- sapply(allTeams,function(x){
  sum(winnersActual==x)
})
winnersPermutedT <- sapply(allTeams,function(x){
  sum(winnersPermuted==x)
})

# P value of comparing the permuted to the actual proportions
chisq.test(winnersPermutedT,p=winnersActualT/nbSimsActual)

# P-value of pairwise similar proportions
pairwisePs <- sapply(allTeams,function(x){
  winActual <- sum(winnersActual==x)
  winPermuted <- sum(winnersPermuted==x)
  prop.test(c(winActual,winPermuted),c(nbSimsActual,nbSimsPermuted))$p.value
})

# Plot the proportion p-values on a logarithmic scale
sortedPairwisePs <- sort(log10(pairwisePs), decreasing=FALSE)
sortedPairwisePsTeams <- factor(names(sortedPairwisePs),
                                levels=names(sortedPairwisePs))
pairwisePsDf <- data.frame(Team=sortedPairwisePsTeams,
                           PValue=sortedPairwisePs)
pwPlot <- ggplot(pairwisePsDf,aes(x=Team,y=PValue,fill=Team))+
  geom_bar(stat="identity") +
  xlab("National team") +
  ylab("Log10 of p-value") +
  ggtitle("Win proportion tests of permuted versus actual draw") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"),
        legend.position="None",
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

# Win ratio calculation = winnersActual/winnersPermuted
sortedRatios <- sort(winnersActualT/winnersPermutedT, decreasing=TRUE)
sortedRatioTeams <- factor(names(sortedRatios),levels=names(sortedRatios))
winratios <- data.frame(Team=sortedRatioTeams,WinRatio=sortedRatios)
ratioPlot <- ggplot(winratios,aes(x=Team,y=WinRatio,fill=Team))+
  geom_bar(stat="identity") +
  ylab("Actual win / permuted win ratio") +
  ggtitle(paste0("EURO 2016 winners (",nbSimsText," simulations)")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

pwPlot
ratioPlot
