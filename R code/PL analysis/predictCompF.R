# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/PL analysis")

# Path to the maximum likelihood functions
mlPath <- "C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/Common files/mlFunctions.R"

# Load english soccer data
# library(devtools)
# install_github('jalapic/engsoccerdata', username = "jalapic")
library(engsoccerdata)
library(skellam) # Difference of two poisson distributions
library(ggvis)
library(rgl)

# General parameters
targetSeasons <- 2000:2014

###########################################
# Section 1: Source the base ML functions #
###########################################

source(mlPath)


#############################################################
# Section 2: prediction function combining all ML functions #
#############################################################

predictMatches <- function(targetSeasons,halfPeriods,ks,
                           methods = c("BT","BTHA","BTGoalDiff","Poisson",
                                       "DefAttack")){
  summary <- NULL
  nbSeasons <- length(targetSeasons)
  nbKs <- length(ks)
  nbHalfPeriods <- length(halfPeriods)
  
  for(i in 1:nbSeasons){
    # Subset scores dataset
    targetSeason <- targetSeasons[i]
    data <- engsoccerdata2[engsoccerdata2$Season == targetSeason &
                             engsoccerdata2$division == "1",]
    data <- data[order(data$Date),]
    cat("Considered season:",targetSeason,"\n")
    
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
          dataIds <- 1:(startIndex-1)
          daysDiff <- as.numeric(difftime(as.Date(c(
            data$Date[-(1:(startIndex))],NA)),
            as.Date(data$Date[-(dataIds)]),units="days"))
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
          
          if("BT" %in% methods){
            for(j in 1:nbHalfPeriods){
              halfPeriod <- halfPeriods[j]
              
              # Calculate the ML estimates
              init <- rep(0,nb.teams+2)
              opt.optim.MLBT <- optim(init,LBT,gr = NULL,data=data[dataIds,],
                                    halfPeriod = halfPeriod, method="BFGS")
              
              # Calculate the numerator and denominator terms of all fixtures
              homeWin <- exp(opt.optim.MLBT$par[data$homeId[fixId]]+
                               opt.optim.MLBT$par[nb.teams+2])
              draw <- exp(opt.optim.MLBT$par[nb.teams+1])*
                sqrt(exp(opt.optim.MLBT$par[data$homeId[fixId]]+
                           opt.optim.MLBT$par[nb.teams+2]+
                           opt.optim.MLBT$par[data$visitId[fixId]]))
              homeLoss <- exp(opt.optim.MLBT$par[data$visitId[fixId]])
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
                
                appendSummary[[paste0("prob","_",halfPeriod,"_",ks[k])]] <- outcomeProb
                appendSummary[[paste0("prob","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbs[,1]
                appendSummary[[paste0("prob","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbs[,2]
              }
            }
          }
          
          ########################################
          # Home away team strength calculations #
          ########################################
          
          if("BTHA" %in% methods){
            for(j in 1:nbHalfPeriods){
              halfPeriod <- halfPeriods[j]
              # Calculate the ML estimates
              init <- rep(0,nb.teams*2+1)
              opt.optim.MLHA <- optim(init,LBTHA,gr = NULL,data=data[dataIds,],
                                    halfPeriod=halfPeriod,method="BFGS")
              
              # Calculate the numerator and denominator terms of all fixtures
              homeWin <- exp(opt.optim.MLHA$par[data$homeId[fixId]])
              draw <- exp(opt.optim.MLHA$par[nb.teams*2+1])*
                sqrt(exp(opt.optim.MLHA$par[data$homeId[fixId]]+
                           opt.optim.MLHA$par[nb.teams+data$visitId[fixId]]))
              homeLoss <- exp(opt.optim.MLHA$par[nb.teams+data$visitId[fixId]])
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
                
                appendSummary[[paste0("probHA","_",halfPeriod,"_",ks[k])]] <- outcomeProbHA
                appendSummary[[paste0("probHA","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsHA[,1]
                appendSummary[[paste0("probHA","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsHA[,2]
              }
            }
          }
          
          #############################################################
          # Team strength calculations using weighted goal difference #
          #############################################################
          
          if("BTGoalDiff" %in% methods){
            for(j in 1:nbHalfPeriods){
              halfPeriod <- halfPeriods[j]
              
              # Calculate the ML estimates
              init <- rep(0,nb.teams+2)
              MLData <- data[dataIds,]
              MLData$weights <- log2(abs(data[dataIds,"goaldif"])+1)
              opt.optim.MLBTGD <- optim(init,LBTGD,gr = NULL,data=MLData,
                                        halfPeriod = halfPeriod,method="BFGS")
              
              # Calculate the numerator and denominator terms of all fixtures
              homeWin <- exp(opt.optim.MLBTGD$par[data$homeId[fixId]]+
                               opt.optim.MLBTGD$par[nb.teams+2])
              draw <- exp(opt.optim.MLBTGD$par[nb.teams+1])*
                sqrt(exp(opt.optim.MLBTGD$par[data$homeId[fixId]]+
                           opt.optim.MLBTGD$par[nb.teams+2]+
                           opt.optim.MLBTGD$par[data$visitId[fixId]]))
              homeLoss <- exp(opt.optim.MLBTGD$par[data$visitId[fixId]])
              denominatorProb <- homeWin+draw+homeLoss
              
              # Loop over the relaxation parameters
              for(k in 1:nbKs){
                
                # Calculate the outcome probabilities
                outcomeProbsGD <- cbind(homeWin/denominatorProb,
                                      draw/denominatorProb,
                                      homeLoss/denominatorProb)/(1+3*ks[k])+
                  ks[k]/(1+3*ks[k])
                
                # Extract the predicted probability of the actual outcome
                outcomeProbGD <- outcomeProbsGD[1:nrow(outcomeProbsGD)+
                                              (
                                                1*as.numeric(data[fixId,"result"]=="D")+
                                                  2*as.numeric(data[fixId,"result"]=="A")
                                              )*nrow(outcomeProbsGD)]
                
                appendSummary[[paste0("probGD","_",halfPeriod,"_",ks[k])]] <- outcomeProbGD
                appendSummary[[paste0("probGD","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsGD[,1]
                appendSummary[[paste0("probGD","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsGD[,2]
              }
            }
          }
          
          #####################################
          # Poisson goal scoring distribution #
          #####################################
          
          if("Poisson" %in% methods || "Skellam" %in% methods){
            for(j in 1:nbHalfPeriods){
              halfPeriod <- halfPeriods[j]
              # Calculate the ML estimates
              init <- rep(0,nb.teams+2)
              opt.optim.MLP <- optim(init,LPoiss,gr = NULL,
                                    data=data[dataIds,],
                                    halfPeriod=halfPeriod,method="BFGS")
              
              # Calculate the numerator and denominator terms of all fixtures
              c1 <- exp(opt.optim.MLP$par[nb.teams+1])
              H <- exp(opt.optim.MLP$par[nb.teams+2])
              lambdasHome <- c1*(H*exp(opt.optim.MLP$par[data$homeId[fixId]]-
                                         opt.optim.MLP$par[data$visitId[fixId]]))
              lambdasAway <- c1*(exp(opt.optim.MLP$par[data$visitId[fixId]]-
                                       opt.optim.MLP$par[data$homeId[fixId]])/H)
              
              homeWin <- pskellam(-1,lambdasAway,lambdasHome)
              draw <- pskellam(0,lambdasAway,lambdasHome)-homeWin
              homeLoss <- 1-homeWin-draw
              
              goalsCorr <- cor(data[dataIds,"hgoal"],
                               data[dataIds,"vgoal"])
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
                
                if("Poisson" %in% methods){
                  appendSummary[[paste0("probP","_",halfPeriod,"_",ks[k])]] <- outcomeProbP
                  appendSummary[[paste0("probP","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsP[,1]
                  appendSummary[[paste0("probP","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsP[,2]
                }
                if("Skellam" %in% methods){
                  appendSummary[[paste0("probPC","_",halfPeriod,"_",ks[k])]] <- outcomeProbPC
                  appendSummary[[paste0("probPC","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsPC[,1]
                  appendSummary[[paste0("probPC","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsPC[,2]
                }
              }
            }
          }
          
          #############################################
          # Defensive-attacking strength calculations #
          #############################################
          
          if("DefAttack" %in% methods){
            for(j in 1:nbHalfPeriods){
              halfPeriod <- halfPeriods[j]
              # Calculate the ML <<stimates
              init <- rep(0,nb.teams*2+2)
              opt.optim.MLDA <- optim(init,LPoissDA,gr = NULL,
                                    data=data[dataIds,],
                                    halfPeriod=halfPeriod,method="BFGS")
              
              # Calculate the numerator and denominator terms of all fixtures
              c1 <- exp(opt.optim.MLDA$par[nb.teams*2+1])
              H <- exp(opt.optim.MLDA$par[nb.teams*2+2])
              lambdasHome <- c1*(H*exp(opt.optim.MLDA$par[nb.teams+data$homeId[fixId]]-
                                         opt.optim.MLDA$par[data$visitId[fixId]]))
              lambdasAway <- c1*(exp(opt.optim.MLDA$par[nb.teams+data$visitId[fixId]]-
                                       opt.optim.MLDA$par[data$homeId[fixId]])/H)
              homeWin <- pskellam(-1,lambdasAway,lambdasHome)
              draw <- pskellam(0,lambdasAway,lambdasHome)-homeWin
              homeLoss <- 1-homeWin-draw
              
              # Loop over the relaxation parameters
              for(k in 1:nbKs){
                
                # Calculate the outcome probabilities
                outcomeProbsDA <- cbind(homeWin,draw,homeLoss)/(1+3*ks[k])+
                  ks[k]/(1+3*ks[k])
                
                # Extract the predicted probability of the actual outcome
                outcomeProbDA <- outcomeProbsDA[1:nrow(outcomeProbsDA)+
                                                  (
                                                    1*as.numeric(data[fixId,"result"]=="D")+
                                                      2*as.numeric(data[fixId,"result"]=="A")
                                                  )*nrow(outcomeProbsDA)]
                
                appendSummary[[paste0("probDA","_",halfPeriod,"_",ks[k])]] <- outcomeProbDA
                appendSummary[[paste0("probDA","_",halfPeriod,"_",ks[k],"_H")]] <- outcomeProbsDA[,1]
                appendSummary[[paste0("probDA","_",halfPeriod,"_",ks[k],"_D")]] <- outcomeProbsDA[,2]
              }
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
      }
    }
  }
  summary <- summary[,c(1:3,ncol(summary),4:(ncol(summary)-1))]
  summary
}

# summary <- predictMatches(2014,50,0)
# summary <- predictMatches(2000:2014,226,0.02,c("BT","Poisson","DefAttack","BTGoalDiff"))
summary <- predictMatches(1960:2014,20*(1:30),seq(0,0.2,0.005))
save(summary, file = "predictCompSummaryProbsL.RData")