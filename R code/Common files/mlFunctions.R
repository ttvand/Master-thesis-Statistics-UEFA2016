################################
# Section 1: base ML functions #
################################

# IMPORTANT: ML estimators consistent up to a scale parameter of the strengths!

# (W)ML function - Bradley-Terry model
LBT <- function(parameters, data, halfPeriod = Inf, debug=FALSE){
  # All parameters should be strictly positive - enforced by exp
  # transformation
  parameters <- exp(parameters)
  
  # Split up parameters
  n <- length(parameters)
  strengths <- parameters[1:(n-2)]
  K <- parameters[n-1]
  H <- parameters[n]
  H <- H^(as.numeric(!data$Neutral))
  
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
  
  # Extract the match importance score
  importance <- data$impScore
  
  # Calculate negative log likelihood (so that a minimum corresponds with
  # the maximum likelihood)
  if(debug) browser()
  out <- -sum(log(ratio1^((data$result == "H") * timeEffect * importance) *
                    ratio2^((data$result == "D") * timeEffect * importance) *
                    ratio3^((data$result == "A") * timeEffect * importance)))
  
  out
}

# (W)ML function - Bradley-Terry with home and away strengths
LBTHA <- function(parameters, data, halfPeriod = Inf){
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
  
  # Recalculate team strengths for neutral ground matches
  neutralIds <- which(data$Neutral)
  num1[neutralIds] <- sqrt(strengthsHome[data$homeId][neutralIds]*
                             strengthsAway[data$homeId][neutralIds])
  num3[neutralIds] <- sqrt(strengthsHome[data$visitId][neutralIds]*
                             strengthsAway[data$visitId][neutralIds])
  
  num2 <- K*sqrt(num1*num3)
  denom <- num1 + num2 + num3
  
  # Calculate the ML ratios
  ratio1 <- num1/denom
  ratio2 <- num2/denom
  ratio3 <- num3/denom
  
  # Calculate the time effect
  timeEffect <- exp(-data$days.passed*log(2)/halfPeriod)
  
  # Extract the match importance score
  importance <- data$impScore
  
  # Calculate negative log likelihood (so that a minimum corresponds with
  # the maximum likelihood)
  out <- -sum(log(ratio1^((data$result == "H") * timeEffect * importance) *
                    ratio2^((data$result == "D") * timeEffect * importance) *
                    ratio3^((data$result == "A") * timeEffect * importance)))
}

# (W)ML function - Bradley-Terry model using weights according to the goal
# difference
LBTGD <- function(parameters, data, halfPeriod = Inf){
  # All parameters should be strictly positive - enforced by exp
  # transformation
  parameters <- exp(parameters)
  
  # Calculate the home and away normalization factors
  homeWinNorm <- mean(data$weights[data$goaldif>0])
  awayWinNorm <- mean(data$weights[data$goaldif<0])
  
  # Split up parameters
  n <- length(parameters)
  strengths <- parameters[1:(n-2)]
  K <- parameters[n-1]
  H <- parameters[n]
  H <- H^(as.numeric(!data$Neutral))
  
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
  
  # Extract the match importance score
  importance <- data$impScore
  
  # Calculate the goal difference effect
  goalDiffEffect <- data$weights/homeWinNorm
  goalDiffEffect[data$result == "A"] <- goalDiffEffect[data$result == "A"]*
    homeWinNorm/awayWinNorm
  
  # Calculate negative log likelihood (so that a minimum corresponds with
  # the maximum likelihood)
  out <- -sum(log(ratio1^((data$result == "H") * timeEffect *
                            goalDiffEffect * importance) *
                    ratio2^((data$result == "D") * timeEffect * importance) *
                    ratio3^((data$result == "A") * timeEffect *
                              goalDiffEffect * importance)))
}

# (W)ML function for Poisson goal scoring distribution
LPoiss <- function(parameters, data, factorials, halfPeriod = Inf,
                   debug=FALSE){
  # All parameters should be strictly positive - enforced by exp
  # transformation
  parameters <- exp(parameters)
  
  # Split up parameters
  n <- length(parameters)
  nb.teams <- n-2
  strengths <- parameters[1:nb.teams]
  c1 <- parameters[n-1]
  H <- parameters[n]
  H <- H^(as.numeric(!data$Neutral))
  
  # Calculate lambdas for all matches and both teams
  lambdasHome <- c1*(H*strengths[data$homeId]/strengths[data$visitId])
  lambdasAway <- c1*(strengths[data$visitId]/strengths[data$homeId]/H)
  
  # Calculate the time effect
  timeEffect <- exp(-data$days.passed*log(2)/halfPeriod)
  
  # Extract the match importance score
  importance <- data$impScore
  
  # Calculate negative log likelihood (so that a minimum corresponds with
  # the maximum likelihood)
  if(debug) browser()
  out <- -sum(log((lambdasHome^data$hgoal*exp(-lambdasHome)/
                     1*#factorials[data$hgoal+1]*
                     lambdasAway^data$vgoal*exp(-lambdasAway)/
                     1)^ #factorials[data$vgoal+1])^
                    (timeEffect*importance)))
}

# (W)ML function for the poisson goal scoring distribution using defensive and
# attacking strengths
LPoissDA <- function(parameters, data, factorials, halfPeriod = Inf){
  # All parameters should be strictly positive - enforced by exp
  # transformation
  parameters <- exp(parameters)
  
  # Split up parameters
  n <- length(parameters)
  nb.teams <- (n-2)/2
  defStrengths <- parameters[1:nb.teams]
  attStrengths <- parameters[nb.teams+(1:nb.teams)]
  c1 <- parameters[n-1]
  H <- parameters[n]
  H <- H^(as.numeric(!data$Neutral))
  
  # Calculate lambdas for all matches and both teams
  lambdasHome <- c1*(H*attStrengths[data$homeId]/defStrengths[data$visitId])
  lambdasAway <- c1*(defStrengths[data$visitId]/attStrengths[data$homeId]/H)
  
  # Calculate the time effect
  timeEffect <- exp(-data$days.passed*log(2)/halfPeriod)
  
  # Extract the match importance score
  importance <- data$impScore
  
  # Calculate negative log likelihood (so that a minimum corresponds with
  # the maximum likelihood)
  out <- -sum(log((lambdasHome^data$hgoal*exp(-lambdasHome)*
                     1 * # factorials[data$hgoal+1]*
                     lambdasAway^data$vgoal*exp(-lambdasAway)*
                     1 # factorials[data$vgoal+1]
  )^
    (timeEffect*importance)))
  
  out
}