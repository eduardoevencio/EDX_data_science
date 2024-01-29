library(tidyverse)
library(dslabs)
data("polls_us_election_2016")

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

#Calculating the posterior mean and posterior standard error
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

#Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes no bias
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

#Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# histogram of outcomes with bias
data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# histogram comparing two outcomes
clinton_EV <- data.frame(clinton_EV) %>% mutate(bias = "no_bias") 
clinton_EV_2 <- data.frame(clinton_EV_2) %>% mutate(bias = "with_bias") %>% rename(clinton_EV = clinton_EV_2)
electoral_results_clinton <- rbind(clinton_EV,clinton_EV_2) %>% mutate(bias = as.factor(bias))

votes_to_win <- data.frame(x=269,type="votes_to_win")
electoral_results_clinton %>% ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1,color="black") +
  geom_vline(data=votes_to_win,aes(xintercept=x,color=type),linetype="dashed",linewidth=1.5)+ #the type column will represent how color behaves for scale color manual
  scale_color_manual(values = c("votes_to_win"="orange"))+ #Associates the color orange to votes_to win label 
  guides(color=guide_legend(title = NULL)) + #Excludes legend title
  facet_grid(bias~.,scales = "free_y") # Set scales to "free_y" for the y-axis


