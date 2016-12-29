SactTable <- matrix(c(47,50,51,52,52.5,53,15,16,17,18,19,20,8,8.9,9.8,10.7,11.6,
                      12.5,4,4.8,5.6,6.4,7.2,8,3,3.7,4.4,5.1,5.8,6.5,2,2.6,3.2,
                      3.8,4.4,5,1,1.5,2,2.5,3,3.5)/100,nrow=6)

# Elo update calculation
eloUpdate <- function(firstRating,secondRating,K,neutral,firstGoal,secondGoal){
  x <- (firstRating-secondRating+100*(!neutral))/200
  Sexp <- 1/(1+10^(-x/2))
  win <- firstGoal>secondGoal
  if(win){
    firstGoalBU <- firstGoal
    firstGoal <- secondGoal
    secondGoal <- firstGoalBU
  }
  goalsScoredSactRow <- min(6,1+firstGoal)
  goalDifScoredSactCol <- min(7,1+secondGoal-firstGoal)
  Sact <- SactTable[goalsScoredSactRow,goalDifScoredSactCol]
  if(win){
    Sact <- 1-Sact
  }
  
  # Calculate the change in Elo for the first team
  change <- K*(Sact-Sexp)
  
  change
}