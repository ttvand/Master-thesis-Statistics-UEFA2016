# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/Exploratory analysis")

# Load english soccer data
# library(devtools)
# install_github('jalapic/engsoccerdata', username = "jalapic")
library(engsoccerdata)

# General parameters
targetSeasons <- 1888:2014

# Subset the EPL data
allData <- engsoccerdata2[engsoccerdata2$Season %in% targetSeasons &
                          engsoccerdata2$division == "1",]

# Exclude 1939 - Only 33 matches
allData <- allData[allData$Season != 1939,]

# Calculate aggregate season measures
summary <- aggregate(cbind(allData$totgoal,allData$result=="D"),
                     list(Season = allData$Season),
                     mean)
tableSizes <- (1+sqrt(1+4*aggregate(allData$home,
                                   list(allData$Season),length)[,2]))/2
names(summary)[-1] <- c("Average goals/game","Fraction draws")

# Plot aggregate season measures
plot(summary[,1],tableSizes,xlab=names(summary)[1],ylab="Table size",
     pch=16,col="grey4")
lines(summary[,1],tableSizes,xlab=names(summary)[1],ylab="Table size")
abline(v=1980.5,col="purple")
plot(summary[,1],summary[,2],xlab=names(summary)[1],ylab=names(summary)[2],
     pch=16,col="grey4")
lines(summary[,1],summary[,2],xlab=names(summary)[1],ylab=names(summary)[2])
abline(v=1980.5,col="purple")

plot(summary[,1],summary[,3],xlab=names(summary)[1],ylab=names(summary)[3],
     pch=16,col="grey4")
lines(summary[,1],summary[,3],xlab=names(summary)[1],ylab=names(summary)[3])
abline(v=1980.5,col="purple")