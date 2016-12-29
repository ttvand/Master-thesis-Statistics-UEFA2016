# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/Shiny/Odds analysis PL")

# Load required libraries
library(shiny)
library(DT)
library(shinythemes)
library(plotly)
library(car)
library(MASS)

# Use ggplotly or plotly directly?
ggplot2plotly <- TRUE

# File with the merged odds and bookmaker data
EloAnalysis <- !TRUE
mergedFile <- paste0("mergedOdds",ifelse(EloAnalysis,"Elo",""),".RData")

# X and Y variables for the models comparison
xModels <- c("Half period","Relaxation")
yModels <- c("Average bet return","Average accuracy","Majority correct",
             "Average log loss")

# K in K-fold cross validation of the match parameter model
K <- 10