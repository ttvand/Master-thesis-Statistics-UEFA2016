# Graph settings
xRange <- seq(-500,500,1)

# Load the required libraries
library(ggplot2)

Oexp <- 1/(1+10^(-xRange/400))

dat <- data.frame(ratingDiff = xRange,
                  Oexp = Oexp)

p <- ggplot(data=dat, aes(x=ratingDiff, y=Oexp)) +
  geom_line(color='steelblue', size = 2) +
  ggtitle("Expected outcome versus rating difference") +
  xlab("Rating difference") +
  labs(y=expression(O[exp])) +
  ylim(c(0,1)) +
  theme(axis.text=element_text(size=12),
        legend.position="none",
        axis.title=element_text(size=14), #,face="bold"
        title=element_text(size=14)) #,face="bold"
p
