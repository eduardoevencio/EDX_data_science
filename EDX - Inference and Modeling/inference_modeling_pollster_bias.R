library(tidyverse)
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

# sees how many polls each pollster conducted 
polls %>% group_by(pollster) %>% summarize(n())

# creates a scaterplot for the spread with pollsters with more number of polls
polls %>% group_by(pollster) %>%
  filter(n()>=6) %>%
  ggplot(aes(pollster,spread))+
  geom_point() + 
  theme(axis.text.x = element_text(angle=90,hjust=1)) #rotates axis x text to a 90 degree angle for better view

# finds standard error for pollsters with more number of polls
polls %>% group_by(pollster) %>%
  filter(n()>=6) %>% 
  summarise(se=2*sqrt(p_hat*(1-p_hat)/median(samplesize)))
