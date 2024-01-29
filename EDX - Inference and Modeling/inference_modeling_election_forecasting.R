library(tidyverse)
library(dslabs)

polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - qnorm(0.975)*se, end = avg + qnorm(0.975)*se)

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)

#calculates values for posterior distribution
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

#calculates values for a credible interval for the posterior distribution
posterior_mean + c(-qnorm(0.975),qnorm(0.975))*posterior_se

#probability of d (true spread) being bigger than zero 
1 - pnorm(0,posterior_mean,posterior_se)