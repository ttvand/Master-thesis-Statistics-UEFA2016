# Graph settings
consideredSeasons <- 1950:2014
nbSeasons <- length(consideredSeasons)

# Load the required libraries
library(engsoccerdata)
library(ggplot2)
library(reshape2)

data <- engsoccerdata2[engsoccerdata2$Season %in% consideredSeasons &
                         engsoccerdata2$division == "1",]

# Calculate the mean home and away goals for all seasons
meanHomeGoals <- rep(NA,nbSeasons)
meanAwayGoals <- rep(NA,nbSeasons)

for(i in 1:nbSeasons){
  season <- consideredSeasons[i]
  meanHomeGoals[i] <- mean(data$hgoal[data$Season==season])
  meanAwayGoals[i] <- mean(data$vgoal[data$Season==season])
}

dat <- data.frame(consideredSeasons = consideredSeasons,
                  meanHomeGoals = meanHomeGoals,
                  meanAwayGoals = meanAwayGoals)

p <- ggplot(data=dat, aes(x=consideredSeasons)) +
  geom_line(aes(y = meanHomeGoals, colour = "blue"), size = 2) +
  geom_line(aes(y = meanAwayGoals, colour = "red"), size = 2) +
  ggtitle("Average goals English Premier League") +
  xlab("Season") +
  ylab("Average goals") +
  scale_colour_discrete(name="",
                        breaks=c("blue", "red"),
                        labels=c("Home", "Away")) +
  theme(axis.text=element_text(size=12),
        legend.text=element_text(size=11),
        axis.title=element_text(size=14), #,face="bold"
        title=element_text(size=14)) #,face="bold"

p

# Univariate analysis of the home goals scored
goalsUnivarSeasons <- 2000:2014
hgoals <- data$hgoal[data$Season %in% goalsUnivarSeasons]
nbMatches <- length(hgoals)
lambdaH <- mean(hgoals)
maxHGoals <- max(hgoals)
rbind(table(hgoals),dpois(0:maxHGoals,lambdaH)*nbMatches)
hGoalsTable <- table(hgoals)
homeDf <- data.frame(Goals = 0:maxHGoals,
                     Actual = c(hGoalsTable,rep(0,maxHGoals+1-
                                                  length(hGoalsTable))),
                     Poisson = dpois(0:maxHGoals,lambdaH)*nbMatches)

# Test Poisson GOF home goals
chisq.test(homeDf$Actual,homeDf$Poisson)

homeDf$Goals <- factor(homeDf$Goals)
homeDf <- melt(homeDf, id=c("Goals"))
rownames(homeDf) <- 1:nrow(homeDf)
names(homeDf) <- c("Goals","Type","Count")

# Generate the home goals comparison plot
hGoalsPlot <- ggplot(homeDf, aes(Goals, Count, fill=Type)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Average home goals distribution English Premier League") +
  theme(axis.text=element_text(size=12),
        legend.text=element_text(size=11),
        axis.title=element_text(size=14), #,face="bold"
        title=element_text(size=14)) #,face="bold"
hGoalsPlot

vgoals <- data$vgoal[data$Season %in% goalsUnivarSeasons]
lambdaV <- mean(vgoals)
rbind(table(vgoals),dpois(0:max(vgoals),lambdaV)*nbMatches)

# Test for independence
chisq.test(table(hgoals,vgoals),simulate.p.value=TRUE,B=1e5)

# True goal counts mixture of poissons
nbMatchesR <- 3e3
randomGoals <- rpois(nbMatchesR,c(1:3))
lambdaR <- mean(randomGoals)
rbind(table(randomGoals),dpois(0:max(randomGoals),lambdaR)*nbMatchesR)
