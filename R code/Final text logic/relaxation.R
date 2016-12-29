# Graph settings
k <- 0.1

# Load the required libraries
library(ggplot2)

modelProb <- seq(0,1,0.001)
relaxedProb <- (modelProb+k)/(1+3*k)

dat <- data.frame(ModelProb = modelProb,
                  RelaxedProb = relaxedProb)

p <- ggplot(data=dat, aes(x=ModelProb)) +
  ggtitle("Relaxation effect for k=0.1") +
  geom_line(aes(y = RelaxedProb), size = 2, colour="steelblue") +
  xlab("Model probability") +
  ylab("Relaxed probability") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(axis.text=element_text(size=12),
        legend.position="none",
        axis.title=element_text(size=14),
        title=element_text(size=14))

p

# Second relaxation graph
ks <- seq(0,0.2,0.001)
relaxedProb <- (1+ks)/(1+3*ks)

dat <- data.frame(K = ks,
                  RelaxedProb = relaxedProb)

q <- ggplot(data=dat, aes(x=K)) +
  ggtitle("Relaxed probabilities for outcome probability of 1") +
  geom_line(aes(y = RelaxedProb), size = 2, colour="steelblue") +
  xlab("K") +
  ylab("Relaxed probability") +
  # xlim(c(0,1)) +
  # ylim(c(0,1)) +
  theme(axis.text=element_text(size=12),
        legend.position="none",
        axis.title=element_text(size=14),
        title=element_text(size=14))

q
