# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/")

library(engsoccerdata)

# (W)ML function - Bradley-Terry model
LBT <- function(parameters, data, halfPeriod = Inf){
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

# Study why the BT model agrees with the 2 point ranking
data <- data.frame(homeId=c(1,1,2,2,3,3),visitId=c(2,3,3,1,1,2),
                   days.passed=0,result=c("H","H","D","H","A","H"),
                   stringsAsFactors = FALSE)
data$home <- data$homeId
data$visitor <- data$visitId
data$hgoal <- 1*(data$result=="H")
data$vgoal <- 1*(data$result=="A")
ranking <- maketable(data,points=2)

# Calculate the ML estimates
nb.teams <- (1+sqrt(1+4*nrow(data)))/2
init <- rep(0,nb.teams+2)
opt.optim.MLBT <- optim(init,LBT,gr = NULL,data=data, method="BFGS")
