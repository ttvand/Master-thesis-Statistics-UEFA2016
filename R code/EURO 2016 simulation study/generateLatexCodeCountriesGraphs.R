# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"EURO 2016 simulation study"))

# References to the original and permuted files
permutedStrings <- c("","Perm")

# Load the required libraries
library(ggplot2)

# Path to save the simulation results
simulationsSaveFile <- "simulationResultsMar25.RData"

# Target txt file
targetLatexTxtFile <- "simulationCountryGraphs.txt"

# Load the simulation results
load(simulationsSaveFile)

# Obtain the team names and total team count
allTeams <- sort(unique(c(simulations[[1]]$firstRound$Team1,
                          simulations[[1]]$firstRound$Team2)))
nbTeams <- length(allTeams)
rm(simulations)

# Calculate the draw description
drawDescriptions <- c(" using the actual draw. ", " using permuted draws. ")

# Sink to the target text file
sink(targetLatexTxtFile)

for(i in 1:nbTeams){
  team <- allTeams[i]
  
  cat("\\begin{figure}[H]\n")
  cat("\\begin{center}\n")
  cat(paste0("\\includegraphics[width=1.0\\linewidth]{",
             gsub(" ","",team),permutedStrings[1],".png}\n"))
  cat("\\end{center}\n")
  cat(paste0("\\caption*{Simulated outcomes for ",
             team, drawDescriptions[1], team,
             " came out as the winner of the tournament in ",
             "X"," of the 100,000 simulations.}\n"))
  cat("\\end{figure}\n\n")
  
  cat("\\begin{figure}[H]\n")
  cat("\\begin{center}\n")
  cat(paste0("\\includegraphics[width=1.0\\linewidth]{",
             gsub(" ","",team),permutedStrings[2],".png}\n"))
  cat("\\end{center}\n")
  cat(paste0("\\caption*{Simulated outcomes for ",
             team, drawDescriptions[2], team,
             " came out as the winner of the tournament in ",
             "X"," of the 100,000 simulations.}\n"))
  cat("\\end{figure}\n\n\n")
}
sink()

