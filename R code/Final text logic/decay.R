# Graph settings
consideredYears <- 5
halfPeriod <- 500

# Load the required libraries
library(ggplot2)

consideredYearsBack <- seq(0,consideredYears,0.001)
consideredDaysBack <- consideredYearsBack*365

FIFAWeights <- ifelse(consideredYearsBack<1,1,
                      ifelse(consideredYearsBack<2,0.5,
                             ifelse(consideredYearsBack<3,0.3,
                                    ifelse(consideredYearsBack<4,0.2,0))))
halfPeriodWeights <- exp(consideredDaysBack/halfPeriod*log(1/2))

dat <- data.frame(yearsBack = consideredYearsBack,
                  FIFAWeights = FIFAWeights,
                  halfPeriodWeights=halfPeriodWeights)

p <- ggplot(data=dat, aes(x=yearsBack)) +
  geom_line(aes(y = FIFAWeights, colour = "blue"), size = 2) +
  geom_line(aes(y = halfPeriodWeights, colour = "red"), size = 2) +
  ggtitle("Continuous versus FIFA decay function") +
  xlab("Years back") +
  ylab("Time weight") +
  scale_colour_discrete(name="Decay function",
                      breaks=c("blue", "red"),
                      labels=c("FIFA ranking", "Continuous depreciation")) +
  theme(axis.text=element_text(size=12),
        legend.text=element_text(size=11),
        axis.title=element_text(size=14), #,face="bold"
        title=element_text(size=14)) #,face="bold"

p
