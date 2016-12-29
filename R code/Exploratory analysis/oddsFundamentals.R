# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Sta dat Tom/Thesis/R/")

# What is the average log loss return for a c(p1,p2,1-p1-p2) game?
avLogLoss <- function(realProbs,ks){
  nbKs <- length(ks)
  shrinkProbs <- matrix((rep(realProbs,each=nbKs)+rep(ks,3))/(1+3*rep(ks,3)),
                        ncol=3)
  avLogLoss <- -(realProbs[1]*log(shrinkProbs[,1]) +
                   realProbs[2]*log(shrinkProbs[,2]) + realProbs[3]*log(shrinkProbs[,3]))
  avLogLoss
}

# What is the average bet return for a c(p1,p2,1-p1-p2) game?
avBetReturn <- function(realProbs,ks){
  nbKs <- length(ks)
  shrinkProbs <- matrix((rep(realProbs,each=nbKs)+rep(ks,3))/(1+3*rep(ks,3)),
                        ncol=3)
  avBetReturn <- realProbs[1]/shrinkProbs[,1] +
    realProbs[2]/shrinkProbs[,2] +
    realProbs[3]/shrinkProbs[,3]
  avBetReturn
}

realProbs <- c(0.9998,0.0001,0.0001)
ks <- seq(0,0.8,0.001)
avLogLossVal <- avLogLoss(realProbs,ks)
avBetReturnVal <- avBetReturn(realProbs,ks)

bestKID <- order(avLogLossVal)[1]
plot(ks,avLogLossVal)
points(ks[bestKID],avLogLossVal[bestKID],col="green",pch=16)

bestKID <- order(avBetReturnVal)[1]
plot(ks,avBetReturnVal)
points(ks[bestKID],avBetReturnVal[bestKID],col="green",pch=16)
browser()

# Random generator in 3D mixture space
nbPoints <- 1e4
x1 <- runif(nbPoints)
x2 <- runif(nbPoints)

# Mirror points into the mixture space
mirrorId <- x2>1-2*abs(x1-.5)
mirrorX1 <- 0.25+0.5*(x1[mirrorId]>0.5)
x1[mirrorId] <- 2*mirrorX1-x1[mirrorId]
x2[mirrorId] <- 1-x2[mirrorId]

plot(x1,x2,xlim=c(0,1),ylim=c(0,1))

# Does the optimum of the average log loss correspond with the true
# probabilities?
p1 <- x2
p2 <- x1-x2/2

trueProbs <- c(0.5,0.3,0.2)
points(trueProbs[2]+trueProbs[1]/2,trueProbs[1],pch=16,col="green")

avLogLosses <- rep(NA,nbPoints)
for(i in 1:length(avLogLosses)){
  avLogLosses[i] <- -(trueProbs[1]*log(p1[i]) + trueProbs[2]*log(p2[i]) +
                        trueProbs[3]*log(1-p1[i]-p2[i]))
}

bestId <- which.min(avLogLosses)
ordered <- cbind(p1[order(avLogLosses)],p2[order(avLogLosses)])
bestProb <- c(p1[bestId],p2[bestId],1-p1[bestId]-p2[bestId])
cat(bestProb)
points(x1[bestId],x2[bestId],pch=16,col="blue")
