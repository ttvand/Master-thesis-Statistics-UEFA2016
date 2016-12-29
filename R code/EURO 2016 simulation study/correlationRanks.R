# Clear workspace
rm(list=ls())

# Set working directory
basePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
setwd(paste0(basePath,"EURO 2016 simulation study"))

# Load required libraries
library(psych)

# Load rank data
data <- read.csv("correlationRanks.csv")
data <- data[1:24,]

# Calculate correlations
cor.test(data$Bookmaker,data$Simstudy)
cor.test(data$Bookmaker,data$FIFA)

# Test if difference in correlations is significant
paired.r(cor(data$Bookmaker, data$Simstudy),
         cor(data$Bookmaker, data$FIFA), n=24)
