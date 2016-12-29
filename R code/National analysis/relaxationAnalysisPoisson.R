# Conclusion: Given that the home team is favorite, the Poisson model predicts
# a higher underdog win than a surprise draw compared to the average bookmaker
# probabilities
# => Research if inflating the draw probabilities when keeping the favorite 
# probabilities fixed help in reducing the average log loss 
#   - Approach one: derive pD and pA given pH (if pH> other probabilities)
#                   from a fixed relationship
#   - Approach two: Inflate pD and deflate pA given a certain constant
#                   Fixed fraction of (pA+pD) to pD
#   - Approach three: Find best fitten home and away strengths using a BT like
#                     model
# Approach two seems like the best option, the other two are considered as
# possible future research

# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"National analysis"))

# Load the required libraries
library(ggplot2)
library(plotly)

# Analysis parameters
predictionPeriodYears <- 4
FIFAWeights <- TRUE
onlyPoisson <- TRUE

# Path to the odds data
oddsDataPath <- paste0(basePath,"PL analysis/mergedOdds.RData")

# Calculate the string parts
saveStringWeights <- ifelse(FIFAWeights,"FIFA","Equal")
poissonString <- ifelse(onlyPoisson,"Poisson","")
relaxationString <- "P"

# Path of the model results
targetModelFile <- paste0("predictProbsNat",predictionPeriodYears,
                         saveStringWeights,"Weights",poissonString,
                         relaxationString,".RData")


##########################
# Poisson model analysis #
##########################

# Load the model data
load(targetModelFile)

# Extract the columns of the preferred model
halfPeriod <- 1300
relaxation <- 0
modelString <- paste0("probP_",halfPeriod,"_",relaxation,"_")
modelSummary <- summary[,grepl(modelString,names(summary))]
rm(summary)
modelSummary$A <- 1-rowSums(modelSummary)
names(modelSummary) <- paste0(c("pH","pD","pA"),"Model")

######################
# Bookmaker analysis #
######################

# Load the odds data
load(oddsDataPath)
odds <- merged[,31:57]
rm(merged)

# Extract the bookie names
nbBookmakers <- ncol(odds)/3
bookmakers <- sapply(names(odds)[3*(1:nbBookmakers)],function(x)
  substr(x,1,nchar(x)-1))

# Calculate the bookie probabilities
for(i in 1:nbBookmakers){
  safetyMargin <- rowSums(1/odds[,paste0(bookmakers[i],c("H","D","A"))])
  probabilities <- 1/odds[,paste0(bookmakers[i],c("H","D","A"))]/safetyMargin
  odds[[paste0("pH",bookmakers[i])]] <- probabilities[,1]
  odds[[paste0("pD",bookmakers[i])]] <- probabilities[,2]
  odds[[paste0("pA",bookmakers[i])]] <- probabilities[,3]
}

# Calculate the average probabilities
odds$pHBookAvg <- rowMeans(odds[,grepl("^pH",names(odds))],na.rm=TRUE)
odds$pDBookAvg <- rowMeans(odds[,grepl("^pD",names(odds))],na.rm=TRUE)
odds$pABookAvg <- rowMeans(odds[,grepl("^pA",names(odds))],na.rm=TRUE)


##############################################################################
# Compare the bookmaker relation versus the poisson relation of PH versus pD #
# and pA                                                                     #
##############################################################################

# Cut the data in equal size slices
nbSlices <- 75 # 25

# Look at the bookmaker relations of pH, pD and pA
homeGroup <- cut(c(odds$pHBookAv,modelSummary$pHModel),nbSlices)
odds$homeGroup <- homeGroup[1:length(odds$pHBookAv)]
plot(odds$pDBookAvg ~ odds$homeGroup)
plot(odds$pABookAvg ~ odds$homeGroup)

# Look at the poisson relations of pH, pD and pA
modelSummary$homeGroup <- homeGroup[-(1:length(odds$pHBookAv))]
plot(modelSummary$pDModel ~ modelSummary$homeGroup)
plot(modelSummary$pAModel ~ modelSummary$homeGroup)

# Overlay the relations using ggplot
homeVsDraw <-
  ggplot(odds,aes(x=homeGroup, y=pDBookAvg)) +
  geom_boxplot(colour="green") +
  geom_boxplot(data=modelSummary,aes(x=homeGroup, y=pDModel),colour="red") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.position="none",
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

homeVsAway <-
  ggplot(odds,aes(x=homeGroup, y=pABookAvg)) +
  geom_boxplot(colour="green") +
  geom_boxplot(data=modelSummary,aes(x=homeGroup, y=pAModel),colour="red") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.position="none",
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

homeVsDrawVsAway <-
  ggplot(odds,aes(x=homeGroup, y=pDBookAvg)) +
  geom_boxplot(colour="green") +
  geom_boxplot(aes(x=homeGroup, y=pABookAvg),colour="green") +
  geom_boxplot(data=modelSummary,aes(x=homeGroup, y=pDModel),colour="red") +
  geom_boxplot(data=modelSummary,aes(x=homeGroup, y=pAModel),colour="red") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.position="none",
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

# Generated plots (static and interactive graphs)
homeVsDraw
homeVsAway
homeVsDrawVsAway
ggplotly(homeVsDraw)
ggplotly(homeVsAway)
ggplotly(homeVsDrawVsAway)
