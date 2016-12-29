# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/")

# Load english soccer data
# library(devtools)
# install_github('jalapic/engsoccerdata', username = "jalapic")
library(engsoccerdata)
library(skellam) # Difference of two poisson distributions
library(ggvis)
library(rgl)

# General parameters
loadSummary <- TRUE
targetSeasons <- 2000:2014
nbSeasons <- length(targetSeasons)

# Exponential decaying parameters
halfPeriods <- c(Inf,seq(100,10,-10))
nbHalfs <- length(halfPeriods)

# Relaxation parameters to suppress low predicted probabilities
genKs <- c(Inf,seq(100,10,-10))
ks <- 1/(genKs-3)
nbKs <- length(ks)

# Summary column format: method_halfPeriod_relaxationDegree
summary <- NULL
summaryFile <- "predictComp.RData"

if(loadSummary && file.exists(summaryFile)){
  load(summaryFile)
} else{
  for(i in 1:nbSeasons){
    # Subset scores dataset
    targetSeason <- targetSeasons[i]
    cat("Considered season:",targetSeason,"\n")
    data <- engsoccerdata2[engsoccerdata2$Season == targetSeason &
                             engsoccerdata2$division == "1",]
    data <- data[order(data$Date),]
    
    if(nrow(data)>100){
      
      # Preprocess data
      teams <- unique(sort(c(data$home,data$visitor)))
      nb.teams <- length(teams)
      data$homeId <- match(data$home,teams)
      data$visitId <- match(data$visitor,teams)
      
      # Only consider seasons with an even number of teams
      if(nb.teams %% 2 == 0){
        
        # Split data from fixture to fixture and calculate predictions for future
        # matches
        startIndex <- (nb.teams-1)*nb.teams/2+1
        while(TRUE){
          daysDiff <- as.numeric(difftime(as.Date(c(
            data$Date[-(1:(startIndex))],NA)),
            as.Date(data$Date[-(1:(startIndex-1))]),units="days"))
          lastMatchId <- startIndex + which(daysDiff>1 | is.na(daysDiff))[1]-1
          
          if(is.na(lastMatchId)){
            lastMatchId <- startIndex
          }
          
          fixId <- startIndex:lastMatchId
          data$days.passed <- as.numeric(difftime(max(
            data$Date[fixId]),data$Date,units="days"))
          
          # Generate the list to append to the summary file
          appendSummary <- list(
            team1=teams[data[fixId,"homeId"]],
            team2=teams[data[fixId,"visitId"]],
            season=targetSeason)
          
          #####################################
          # Single team strength calculations #
          #####################################
          
          for(j in 1:nbHalfs){
            # (W)ML function
            L <- function(parameters, data, halfPeriod = Inf){
              # All parameters should be strictly positive - enforced by exp
              # transformation
              parameters <- exp(parameters)
              
              # Split up parameters
              n <- length(parameters)
              strengths <- parameters[1:(n-2)]
              K <- parameters[n-1]
              H <- parameters[n]
              
              # Calculate the relative outcome probabilities
              num1 <- H*strengths[data$homeId]
              num2 <- K*sqrt(H*strengths[data$homeId]*strengths[data$visitId])
              num3 <- strengths[data$visitId]
              denom <- num1 + num2 + num3
              
              # Calculate the time effect
              timeEffect <- exp(-data$days.passed*log(2)/halfPeriod)
              # cat("\n\n",timeEffect)
              
              # Calculate the ML ratios
              ratio1 <- num1/denom
              ratio2 <- num2/denom
              ratio3 <- num3/denom
              
              # Calculate negative log likelihood (so that a minimum corresponds with
              # the maximum likelihood)
              out <- -log(prod(ratio1^((data$result == "H") * timeEffect) *
                                 ratio2^((data$result == "D") * timeEffect) *
                                 ratio3^((data$result == "A") * timeEffect)))
            }
            
            # Calculate the ML estimates
            init <- rep(0,nb.teams+2)
            opt.optim.ML <- optim(init,L,gr = NULL,data=data[1:(startIndex-1),],
                                  halfPeriod = halfPeriods[j],method="BFGS")
            
            # Calculate the numerator and denominator terms of all fixtures
            homeWin <- exp(opt.optim.ML$par[data$homeId[fixId]]+
                             opt.optim.ML$par[nb.teams+2])
            draw <- exp(opt.optim.ML$par[nb.teams+1])*
              sqrt(exp(opt.optim.ML$par[data$homeId[fixId]]+
                         opt.optim.ML$par[nb.teams+2]+
                         opt.optim.ML$par[data$visitId[fixId]]))
            homeLoss <- exp(opt.optim.ML$par[data$visitId[fixId]])
            denominatorProb <- homeWin+draw+homeLoss
            
            # Loop over the relaxation parameters
            for(k in 1:nbKs){
              
              # Calculate the outcome probabilities
              outcomeProbs <- cbind(homeWin/denominatorProb,
                                    draw/denominatorProb,
                                    homeLoss/denominatorProb)/(1+3*ks[k])+
                ks[k]/(1+3*ks[k])
              
              # Extract the predicted probability of the actual outcome
              outcomeProb <- outcomeProbs[1:nrow(outcomeProbs)+
                                            (
                                              1*as.numeric(data[fixId,"result"]=="D")+
                                                2*as.numeric(data[fixId,"result"]=="A")
                                            )*nrow(outcomeProbs)]
              maxGameProbs <- apply(outcomeProbs,1,max)
              majProb <- ifelse(outcomeProbs[,1]==maxGameProbs,"H",
                                ifelse(outcomeProbs[,2]==maxGameProbs,"D","A"))
              
              appendSummary[[paste0("prob","_",halfPeriods[j],"_",genKs[k])]] <- outcomeProb
              appendSummary[[paste0("majProb","_",halfPeriods[j],"_",genKs[k])]] <- majProb
            }
          }
          
          ########################################
          # Home away team strength calculations #
          ########################################
          
          for(j in 1:nbHalfs){
            # ML function
            LHA <- function(parameters, data, halfPeriod = Inf){
              # All parameters should be strictly positive - enforced by exp
              # transformation
              parameters <- exp(parameters)
              
              # Split up parameters
              n <- length(parameters)
              nb.teams <- (n-1)/2
              
              strengthsHome <- parameters[1:nb.teams]
              strengthsAway <- parameters[nb.teams+(1:nb.teams)]
              K <- parameters[n]
              
              # Calculate the relative outcome probabilities
              num1 <- strengthsHome[data$homeId]
              num3 <- strengthsAway[data$visitId]
              num2 <- K*sqrt(num1*num3)
              denom <- num1 + num2 + num3
              
              # Calculate the ML ratios
              ratio1 <- num1/denom
              ratio2 <- num2/denom
              ratio3 <- num3/denom
              
              # Calculate the time effect
              timeEffect <- exp(-data$days.passed*log(2)/halfPeriod)
              
              # Calculate negative log likelihood (so that a minimum corresponds with
              # the maximum likelihood)
              out <- -log(prod(ratio1^((data$result == "H") * timeEffect) *
                                 ratio2^((data$result == "D") * timeEffect) *
                                 ratio3^((data$result == "A") * timeEffect)))
            }
            
            # Calculate the ML estimates
            init <- rep(0,nb.teams*2+1)
            opt.optim.ML <- optim(init,LHA,gr = NULL,data=data[1:(startIndex-1),],
                                  halfPeriod=halfPeriods[j],method="BFGS")
            
            # Calculate the numerator and denominator terms of all fixtures
            homeWin <- exp(opt.optim.ML$par[data$homeId[fixId]])
            draw <- exp(opt.optim.ML$par[nb.teams*2+1])*
              sqrt(exp(opt.optim.ML$par[data$homeId[fixId]]+
                         opt.optim.ML$par[nb.teams+data$visitId[fixId]]))
            homeLoss <- exp(opt.optim.ML$par[nb.teams+data$visitId[fixId]])
            denominatorProb <- homeWin+draw+homeLoss
            
            # Loop over the relaxation parameters
            for(k in 1:nbKs){
              
              # Calculate the outcome probabilities
              outcomeProbsHA <- cbind(homeWin/denominatorProb,draw/denominatorProb,
                                      homeLoss/denominatorProb)/(1+3*ks[k])+
                ks[k]/(1+3*ks[k])
              
              # Extract the predicted probability of the actual outcome
              outcomeProbHA <- outcomeProbsHA[1:nrow(outcomeProbsHA)+
                                                (
                                                  1*as.numeric(data[fixId,"result"]=="D")+
                                                    2*as.numeric(data[fixId,"result"]=="A")
                                                )*nrow(outcomeProbsHA)]
              maxGameProbsHA <- apply(outcomeProbsHA,1,max)
              majProbHA <- ifelse(outcomeProbsHA[,1]==maxGameProbsHA,"H",
                                  ifelse(outcomeProbsHA[,2]==maxGameProbsHA,"D","A"))
              
              appendSummary[[paste0("probHA","_",halfPeriods[j],"_",genKs[k])]] <- outcomeProbHA
              appendSummary[[paste0("majProbHA","_",halfPeriods[j],"_",genKs[k])]] <- majProbHA
            }
          }
          
          #####################################
          # Poisson goal scoring distribution #
          #####################################
          
          for(j in 1:nbHalfs){
            # (W)ML function
            LP <- function(parameters, data, halfPeriod = Inf){
              # All parameters should be strictly positive - enforced by exp
              # transformation
              parameters <- exp(parameters)
              
              # Split up parameters
              n <- length(parameters)
              nb.teams <- n-2
              strengths <- parameters[1:nb.teams]
              c1 <- parameters[n-1]
              H <- parameters[n]
              
              # Calculate lambdas for all matches and both teams
              lambdasHome <- c1*(H*strengths[data$homeId]/strengths[data$visitId])
              lambdasAway <- c1*(strengths[data$visitId]/strengths[data$homeId]/H)
              
              # Calculate the time effect
              timeEffect <- exp(-data$days.passed*log(2)/halfPeriod)
              if(mean(timeEffect)<0.99){
                # cat(timeEffect,"\n")
              }
              
              # Calculate negative log likelihood (so that a minimum corresponds with
              # the maximum likelihood)
              out <- -log(prod((lambdasHome^data$hgoal*exp(-lambdasHome)*
                                  factorial(data$hgoal)*
                                  lambdasAway^data$vgoal*exp(-lambdasAway)*
                                  factorial(data$vgoal))^
                                 timeEffect))
            }
            
            # Calculate the ML estimates
            init <- rep(0,nb.teams+2)
            opt.optim.ML <- optim(init,LP,gr = NULL,data=data[1:(startIndex-1),],
                                  halfPeriod=halfPeriods[j],method="BFGS")
            
            # Calculate the numerator and denominator terms of all fixtures
            c1 <- exp(opt.optim.ML$par[nb.teams+1])
            H <- exp(opt.optim.ML$par[nb.teams+2])
            lambdasHome <- c1*(H*exp(opt.optim.ML$par[data$homeId[fixId]]-opt.optim.ML$par[data$visitId[fixId]]))
            lambdasAway <- c1*(exp(opt.optim.ML$par[data$visitId[fixId]]-opt.optim.ML$par[data$homeId[fixId]])/H)
            
            homeWin <- pskellam(-1,lambdasAway,lambdasHome)
            draw <- pskellam(0,lambdasAway,lambdasHome)-homeWin
            homeLoss <- 1-homeWin-draw
            
            goalsCorr <- cor(data[1:(startIndex-1),"hgoal"],
                             data[1:(startIndex-1),"vgoal"])
            corCorrLambda <- -goalsCorr*sqrt(lambdasAway*lambdasHome)
            homeWinC <- pskellam(-1,lambdasAway+corCorrLambda,
                                 lambdasHome+corCorrLambda)
            drawC <- pskellam(0,lambdasAway+corCorrLambda,
                              lambdasHome+corCorrLambda)-homeWin
            homeLossC <- 1-homeWinC-drawC
            
            # Loop over the relaxation parameters
            for(k in 1:nbKs){
              
              # Calculate the outcome probabilities
              outcomeProbsP <- cbind(homeWin,draw,homeLoss)/(1+3*ks[k])+
                ks[k]/(1+3*ks[k])
              outcomeProbsPC <- cbind(homeWinC,drawC,homeLossC)/(1+3*ks[k])+
                ks[k]/(1+3*ks[k])
              
              # Extract the predicted probability of the actual outcome
              outcomeProbP <- outcomeProbsP[1:nrow(outcomeProbsP)+
                                              (
                                                1*as.numeric(data[fixId,"result"]=="D")+
                                                  2*as.numeric(data[fixId,"result"]=="A")
                                              )*nrow(outcomeProbsP)]
              outcomeProbPC <- outcomeProbsPC[1:nrow(outcomeProbsPC)+
                                              (
                                                1*as.numeric(data[fixId,"result"]=="D")+
                                                  2*as.numeric(data[fixId,"result"]=="A")
                                              )*nrow(outcomeProbsPC)]
              maxGameProbsP <- apply(outcomeProbsP,1,max)
              maxGameProbsPC <- apply(outcomeProbsPC,1,max)
              majProbP <- ifelse(outcomeProbsP[,1]==maxGameProbsP,"H",
                                 ifelse(outcomeProbsP[,2]==maxGameProbsP,"D","A"))
              majProbPC <- ifelse(outcomeProbsPC[,1]==maxGameProbsPC,"H",
                                 ifelse(outcomeProbsPC[,2]==maxGameProbsPC,"D","A"))
              
              appendSummary[[paste0("probP","_",halfPeriods[j],"_",genKs[k])]] <- outcomeProbP
              appendSummary[[paste0("majProbP","_",halfPeriods[j],"_",genKs[k])]] <- majProbP
              
              appendSummary[[paste0("probPC","_",halfPeriods[j],"_",genKs[k])]] <- outcomeProbPC
              appendSummary[[paste0("majProbPC","_",halfPeriods[j],"_",genKs[k])]] <- majProbPC
            }
          }
          
          appendSummary[["outcome"]] <- data[fixId,"result"]
          
          ###########################################################
          # Add all combined calculations to the summary data frame #
          ###########################################################
          summary <- rbind(summary,as.data.frame(appendSummary,
                                                 stringsAsFactors=FALSE))
          
          # Increment the start match index and exit the infite loop when all
          # matches have been processed
          if(lastMatchId>=nrow(data)){
            break
          } else{
            startIndex <- lastMatchId+1
          }
        }
        
        # Save summary on last run of loop
        if(i == nbSeasons){
          save(summary,file = summaryFile)
        }
      }
    }
  }
}

# Subset on the years of interest
summary <- summary[summary$season %in% targetSeasons,]

# Note: 1891 - Sunderland wins all home matches

# Distribution of the modeled outcome probabilities
hist(summary$prob_Inf_Inf,50)
hist(summary$probHA_Inf_Inf,50)
hist(summary$probP_Inf_Inf,50)

# Plots to assess the quality of the predictions
# plot(sort(summary$prob_Inf_Inf),sort(summary$probHA_Inf_Inf),pch=16,col="white")
# lines(sort(summary$prob_Inf_Inf),sort(summary$probHA_Inf_Inf))
# abline(0,1,col="blue")

# Calculate summary measures of the predictive accuracy of the different models
dropNames <- c("team1","team2","season","outcome")
probNames <- setdiff(names(summary),dropNames)
probCols <- probNames[sapply(summary[,probNames],function(x) is.numeric(x))]
accSumm <- sapply(probCols,function(colName){
  x <- summary[[colName]]
  y <- summary[[paste0("majP",substring(colName,2))]]
  avBetReturn <- round(mean(1/x),4)
  avAcc <- round(mean(x),4)
  avMajCorrect <- round(mean(y==summary$outcome),4)
  avLogLoss <- round(mean(-log(x)),4)
  c(avBetReturn,avAcc,avMajCorrect,avLogLoss)
})
accSumm[accSumm>1e5] <- Inf
rownames(accSumm) <- c("Average bet return","Average accuracy",
                       "Majority correct","Average log loss")
print(accSumm)

# Extract the decay and relaxation parameters for all columns of accSum
predictionSummary <- NULL
for(i in 1:ncol(accSumm)){
  method_name <- colnames(accSumm)[i]
  name_parts <- strsplit(method_name,"_")[[1]]
  if(name_parts[1]=="prob"){
    method <- "BT team strengths"
  } else{
    if(name_parts[1]=="probHA"){
      method <- "Home away strengths"
    } else{
      if(name_parts[1]=="probP"){
        method <- "Poisson corr goal strengths"
      } else{
        method <- "Poisson goal strengths"
      }
    }
  }
  
  decay <- as.numeric(name_parts[2])
  relaxation <- as.numeric(name_parts[3])
  
  predictionSummary <<- rbind(predictionSummary,cbind(
    data.frame(method=method,decay=decay,relaxation=relaxation,
               stringsAsFactors=FALSE),
    as.data.frame(as.list(accSumm[,i]))))
}
names(predictionSummary)[-(1:3)] <- rownames(accSumm)
print(predictionSummary)

# Limiting accuracy boundaries
cat("Home win fraction:", round(mean(summary$outcome=="H"),4),"\n")
cat("Correct fraction majority rule team strengths:", round(
  mean(summary$majProb_Inf_Inf==summary$outcome),4),"\n")
cat("Correct fraction majority rule HA strengths:", round(
  mean(summary$majProbHA_Inf_Inf==summary$outcome),4),"\n\n")
cat("Correct fraction majority rule Poisson strengths:", round(
  mean(summary$majProbP_Inf_Inf==summary$outcome),4),"\n\n")

predictionSummary$decay[predictionSummary$decay==Inf] <- 110
predictionSummary$relaxation[predictionSummary$relaxation==Inf] <- 110
predictionSummary[predictionSummary[,4]>4,4] <- 4
predictionSummary[predictionSummary[,7]>1,7] <- 1

# Filter the predictionSummary
predictionSummary <- predictionSummary[grepl("Poisson",predictionSummary$method),]

predictionSummary %>%
  ggvis(input_select(names(predictionSummary)[2:3], map = as.name),
        input_select(names(predictionSummary)[-(1:3)], map = as.name)) %>%
  group_by(method) %>%
  layer_points(fill = ~method)
