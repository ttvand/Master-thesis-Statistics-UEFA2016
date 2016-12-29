# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/PL analysis")

# Load required libraries
library(nnet)
library(car)
library(MASS)

# File name of the merged predictions and bookmaker odds
oddsFileName <- "mergedOdds.RData"

# Load the merged odds
load(oddsFileName)

# Subset on the predictor column ids
predictorIds <- 13:30
predictors <- merged[,predictorIds]

# Drop predictors that have missing values
missingCounts <- sapply(predictors,function(x) sum(is.na(x)))
predictors <- predictors[,missingCounts==0]

# Pair the predictors
for(i in 1:(ncol(predictors)/2)){
  predictors[,1+(i-1)*2] <- predictors[,1+(i-1)*2]-predictors[,i*2]
  names(predictors)[1+(i-1)*2] <- paste0(names(predictors)[1+(i-1)*2],
                                          " - ",
                                          names(predictors)[i*2])
}
predictors <- predictors[,2*(1:(ncol(predictors)/2))-1]

# Combine the predictors with the outcome
data <- cbind(merged$outcome,predictors)
names(data)[1] <- "Result"



##################################################
# 1) Proportional odds logistic regression model #
##################################################
fitPolr <- polr(Result ~ (`HS - AS` + `HST - AST` + `HC - AC` + `HF - AF` +
                  `HY - AY` + `HR - AR`)^2, data=data)
summary(fitPolr)
Anova(fitPolr)
polrPredict <- predict(fitPolr,type="probs")
polrPredictProb <- ifelse(data$Result=="A",polrPredict[,1],
                              ifelse(data$Result=="D",polrPredict[,2],
                                     polrPredict[,3]))
polrLogLoss <- -mean(log(polrPredictProb))

###################################
# 2) Multinomial regression model #
###################################
fitMultMain <- multinom(Result ~ ., data=data)

summary(fitMultMain)
Anova(fitMultMain)
multinomPredict <- predict(fitMultMain,type="probs")
multinomPredictProb <- ifelse(data$Result=="A",multinomPredict[,1],
                              ifelse(data$Result=="D",multinomPredict[,2],
                                     multinomPredict[,3]))
multinomLogLoss <- -mean(log(multinomPredictProb))

###############################
# Summary of categorical fits #
###############################
cat("Log losses polr and multinom:",round(c(polrLogLoss,multinomLogLoss),3),
    "\n")
cat("Mean correct polr and multinom:",round(c(mean(polrPredictProb),
                                              mean(multinomPredictProb)),3),
    "\n")
