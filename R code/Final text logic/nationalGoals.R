# Graph settings
consideredYears <- 1980:2015
nbYears <- length(consideredYears)

# Load the required libraries
library(ggplot2)
library(reshape2)

# Load the data
load("internationalResultsEurope1980P.RData")
data <- allMatches
rm(allMatches)
data$Year <- as.numeric(format(data$Date,"%Y"))

# Exclude matches on neutral ground
data <- data[!data$Neutral,]

# Calculate the mean home and away goals for all seasons
meanHomeGoals <- rep(NA,nbYears)
meanAwayGoals <- rep(NA,nbYears)

for(i in 1:nbYears){
  year <- consideredYears[i]
  meanHomeGoals[i] <- mean(data$hgoal[data$Year==year])
  meanAwayGoals[i] <- mean(data$vgoal[data$Year==year])
}

dat <- data.frame(consideredYears = consideredYears,
                  meanHomeGoals = meanHomeGoals,
                  meanAwayGoals = meanAwayGoals)

p <- ggplot(data=dat, aes(x=consideredYears)) +
  geom_line(aes(y = meanHomeGoals, colour = "blue"), size = 2) +
  geom_line(aes(y = meanAwayGoals, colour = "red"), size = 2) +
  ggtitle("Average goals national team matches") +
  xlab("Year") +
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
goalsUnivarYears <- 1992:2015
hgoals <- data$hgoal[data$Year %in% goalsUnivarYears]
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
  ggtitle("Average home goals distribution national team matches") +
  theme(axis.text=element_text(size=12),
        legend.text=element_text(size=11),
        axis.title=element_text(size=14), #,face="bold"
        title=element_text(size=14)) #,face="bold"
hGoalsPlot

vgoals <- data$vgoal[data$Year %in% goalsUnivarYears]
vTable <- table(factor(vgoals, levels = min(vgoals):max(vgoals)))
lambdaV <- mean(vgoals)
rbind(vTable,dpois(0:max(vgoals),lambdaV)*nbMatches)

# Test for independence
chisq.test(table(hgoals,vgoals),simulate.p.value=TRUE,B=1e5)

# True goal counts mixture of poissons
nbMatchesR <- 3e3
randomGoals <- rpois(nbMatchesR,c(1:3))
lambdaR <- mean(randomGoals)
rbind(table(randomGoals),dpois(0:max(randomGoals),lambdaR)*nbMatchesR)
