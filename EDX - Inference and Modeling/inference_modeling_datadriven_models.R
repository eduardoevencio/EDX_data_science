# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# calculates standard deviation of spread
sd(one_poll_per_pollster$spread)

# construct 95% confidence interval for the spread accounting pollster to pollster variability
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - qnorm(0.975)*se, end = avg + qnorm(0.975)*se)
round(results*100, 1)