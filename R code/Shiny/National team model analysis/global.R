# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/Shiny/National team model analysis")

# Load required libraries
library(shiny)
library(ggvis)
library(DT) # devtools::install_github('rstudio/htmltools');  devtools::install_github('rstudio/DT')
library(shinythemes)
library(plotly)
library(shinyjs)

# File with the merged odds and bookmaker data
baseFilePath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/"
EloAnalysis <- !TRUE
predictionPeriodYears <- 4
FIFAWeights <- TRUE

# Path to the population file
populationPath <- paste0(baseFilePath,"Common files/population.csv")
population <- read.csv(populationPath,stringsAsFactors = FALSE)

onlyPoisson <- TRUE
relaxationType <- c("probabilities","strengths")[2]
poissonString <- ifelse(onlyPoisson,ifelse(relaxationType=="strengths",
                                           "PoissonS","PoissonP"),"")

weightsString <- ifelse(FIFAWeights,"FIFA","Equal")
if(EloAnalysis){
  dataFile <- paste0(baseFilePath,
                     "National analysis/predictProbsNat4FIFAWeightsElo.RData")
} else{
  dataFile <- paste0(baseFilePath,"National analysis/predictProbsNat",predictionPeriodYears,
                     weightsString,"Weights",poissonString,".RData")
}
dataString <- paste("National team prediction over a period of",
                    predictionPeriodYears,
                    "years with",
                    ifelse(FIFAWeights,"FIFA","equal"),
                    "weights")

# Use ggplotly or plotly directly?
ggplot2plotly <- TRUE

# X and Y variables for the models comparison
xModels <- c("Half period","Relaxation")
yModels <- c("Average bet return","Average accuracy","Majority correct",
             "Average log loss")

aboutString <- "This app was developed by Tom Van de Wiele and relates to the selection of the best ML model to predict national team matches <br/><br/>The author can be contacted on tvdwiele@gmail.com"


#######################
# Preprocess raw data #
#######################

# Load the national team match summary data
load(dataFile)

# summary <- summary[1:100,]
rownames(summary) <- paste0("ID:",1:nrow(summary))

# Extract the model names, half periods and relaxation parameters
modelColumns <- grep("prob.*_.*_[^a-zA-Z]*$",names(summary))
modelNames <- sapply(names(summary)[modelColumns],function(x)
  unlist(strsplit(x,'_'))[1])
modelNames <- ifelse(modelNames=="prob","BT",
                     ifelse(modelNames=="probHA","EBT",
                            ifelse(modelNames=="probP","Poisson",
                                   ifelse(modelNames=="probGD","Combined BTP",
                                          ifelse(modelNames=="probDA","Extended Poisson","Elo")
                                   )
                            )
                     )
)
halfPeriods <- sapply(names(summary)[modelColumns],function(x)
  as.numeric(unlist(strsplit(x,'_'))[2]))
relaxations <- round(sapply(names(summary)[modelColumns],function(x)
  as.numeric(unlist(strsplit(x,'_'))[3])),4)