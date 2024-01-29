library(dslabs)
library(tidyverse)

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls_2012 <- data.frame(poll = 1:ncol(confidence_intervals),
                         t(confidence_intervals), sample_size = Ns)
names(polls_2012) <- c("poll", "estimate", "low", "high", "sample_size")


# calculates an estimate of the spread combining the results of all the polls
d_hat <- polls_2012 %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

# calculates an estimate of the proportion p and its margin of error
p_hat <- (1+d_hat)/2
moe <- 2*qnorm(0.975)*sqrt(p_hat*(1-p_hat)/sum(polls_2012$sample_size))

# creates a data frame avg combining the aggregated results of the spread, lower and upper
# confidence intervals and the aggregated sample size
avg <- data.frame(poll=factor("avg"),estimate=d_hat,low=d_hat-moe/2,high=d_hat+moe/2,
                  sample_size=sum(polls_2012$sample_size))

# combines both date frames into a single data frame with a new column type
polls_2012 <- rbind(polls_2012,avg)
polls_2012$poll <- factor(polls_2012$poll)
polls_2012 <- polls_2012 %>% mutate(type= ifelse(poll=="avg","aggregated","simple"))
polls_2012$type <- factor(polls_2012$type)

# crates a chart for viweing the confidence intervals of the simple polls vs the aggregated results

chart_polls_2012 <- polls_2012 %>% ggplot (aes(estimate,poll,color = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin=low,xmax=high)) + 
  geom_vline(xintercept = 0,linewidth=0.5)+
  geom_vline(xintercept = d,linetype="dotted",linewidth=0.5)+
  xlab("Spread Estimate") +
  ylab("Poll Number")+
  theme(panel.grid = element_blank())
chart_polls_2012
