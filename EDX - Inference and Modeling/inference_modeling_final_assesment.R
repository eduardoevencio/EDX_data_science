# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

#Question 1: Expected value and standard error of a poll
#Consider a poll with a sample of 1500 voters.

N <- 1500

#What is the expected total number of voters in the sample choosing "Remain"?
N*p

#What is the standard error of the total number of voters in the sample choosing "Remain"?
se_sum <- sqrt(N*p*(1-p))
se_sum

#What is the expected value of X_hat, the proportion of "Remain" voters?
X_hat <- p
X_hat

#What is the standard error of X_hat, the proportion of "Remain" voters?
se <- sqrt(p*(1-p)/N)
se 

#What is the expected value of d, the spread between the proportion of "Remain" voters and "Leave" voters?
d

#What is the standard error of , the spread between the proportion of "Remain" voters and "Leave" voters?
se_spread <- 2*se
se_spread

#Question 2: Actual Brexit poll estimates
#Calculate x_hat for each poll, the estimate of the proportion of voters choosing "Remain" on the 
#referendum day (p), given the observed spread and the relationship d = 2*p-1. 
#Use mutate() to add a variable x_hat to the brexit_polls object by filling in the skeleton code below

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

#What is the average of the observed spreads (spread)?
brexit_polls %>% summarise(avg = mean(spread))

#What is the standard deviation of the observed spreads?
brexit_polls %>% summarise(sd = sd(spread))  

#What is the average of x_hat, the estimates of the parameter p?  
brexit_polls %>% summarise(avg = mean(x_hat))

#What is the standard deviation of x_hat?  
brexit_polls %>% summarise(sd = sd(x_hat))

#Question 3: Confidence interval of a Brexit poll
#Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
brexit_polls <- brexit_polls %>%
  mutate(se = sqrt(x_hat*(1-x_hat)/samplesize))
brexit_polls[1,]

#What is the lower bound of the 95% confidence interval?
brexit_polls$x_hat[1]-qnorm(0.975)*brexit_polls$se[1]

#What is the upper bound of the 95% confidence interval?
brexit_polls$x_hat[1]+qnorm(0.975)*brexit_polls$se[1]

#Question 4: Confidence intervals for polls in June
june_polls <- brexit_polls %>%
  filter(enddate >="2016-06-01") %>%
  mutate(se_spread = 2*se, 
         lower = spread - qnorm(0.975)*se_spread,
         upper = spread + qnorm(0.975)*se_spread,
         hit = d >= lower & d <=upper)

#How many polls are in june_polls?
nrow(june_polls)

#What proportion of polls have a confidence interval that covers the value 0?  
june_polls %>%
  mutate(no_winner = 0 >= lower & 0 <=upper) %>%
  summarise(porportion_no_winner = mean(no_winner))

#What proportion of polls predict "Remain" (confidence interval entirely above 0)?
june_polls %>%
  mutate(predict_remain = lower>=0 & upper >=0) %>%
  summarise(porportion_remain = mean(predict_remain))

#What proportion of polls have a confidence interval covering the true value of d?  
june_polls %>%
  summarise(proportion_hit = mean(hit))
  
#Question 5: Hit rate by pollster
june_polls %>% group_by(pollster) %>%
  summarise(hit_rate = mean(hit), N_polls = n()) %>%
  arrange(desc(hit_rate))

#Question 6: Boxplot of Brexit polls by poll type 
june_polls %>%
  ggplot(aes(poll_type,spread))+
  geom_boxplot()

#Question 7: Combined spread across poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se = 2*sqrt(p_hat*(1-p_hat)/N),
            lower=spread - qnorm(0.975)*se,
            upper=spread + qnorm(0.975)*se,
            length = upper - lower)

#What is the lower bound of the 95% confidence interval for online voters?
combined_by_type[combined_by_type$poll_type=="Online",]$spread - qnorm(0.975)*combined_by_type[combined_by_type$poll_type=="Online",]$se

#What is the upper bound of the 95% confidence interval for online voters?
combined_by_type[combined_by_type$poll_type=="Online",]$spread + qnorm(0.975)*combined_by_type[combined_by_type$poll_type=="Online",]$se

#Question 9: Chi-squared p-value
data(brexit_polls)
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < d & spread_upper > d)

brexit_hit_poll <- brexit_hit %>%
  select(poll_type, hit)

#Use brexit_hit to make a two-by-two table of poll type and hit status. 
#Then use the chisq.test() function to perform a chi-squared test to determine whether the 
#difference in hit rate is significant.
two_by_two_brexit <- brexit_hit_poll %>%
  group_by(poll_type) %>%
  summarise(hit = sum(hit),
            no_hit = n()-hit,
            hit_rate = hit/(hit+no_hit))
two_by_two_brexit_2 <- tibble(hit_status = c("no_hit","hit"),
         Online = c(two_by_two_brexit[two_by_two_brexit$poll_type=="Online",]$no_hit,two_by_two_brexit[two_by_two_brexit$poll_type=="Online",]$hit),
      Telephone = c(two_by_two_brexit[two_by_two_brexit$poll_type=="Telephone",]$no_hit,two_by_two_brexit[two_by_two_brexit$poll_type=="Telephone",]$hit))

#What is the p-value of the chi-squared test comparing the hit rate of online and telephone polls?
two_by_two_brexit_2 %>%
  select(-hit_status) %>%
  chisq.test() %>% .$p.value

#Use the two-by-two table constructed in the previous exercise to calculate the odds ratio between 
#the hit rate of online and telephone polls to determine the magnitude of the difference in performance 
#between the poll types.
two_by_two_brexit <- two_by_two_brexit %>%
  mutate(odds_ratio = (hit/(hit+no_hit))/(no_hit/(hit+no_hit)))

#Calculate the odds that an online poll generates a confidence interval that covers the actual 
#value of the spread
two_by_two_brexit[two_by_two_brexit$poll_type=="Online",]$odds_ratio

#Calculate the odds that a telephone poll generates a confidence interval that covers 
#the actual value of the spread
two_by_two_brexit[two_by_two_brexit$poll_type=="Telephone",]$odds_ratio

#Calculate the odds ratio to determine how many times larger the odds are for online 
#polls to hit versus telephone polls

two_by_two_brexit[two_by_two_brexit$poll_type=="Online",]$odds_ratio/
  two_by_two_brexit[two_by_two_brexit$poll_type=="Telephone",]$odds_ratio

#Question 11: Plotting spread over time
#Use brexit_polls to make a plot of the spread (spread) over time (enddate) colored by poll type (poll_type). 
#Use geom_smooth() with method = "loess" to plot smooth curves with a span of 0.4. 
#Include the individual data points colored by poll type. Add a horizontal line indicating the final value of d

brexit_polls %>% 
  ggplot(aes(enddate,spread,col=poll_type))+
  geom_smooth(method = "loess",span=0.4)+
  geom_point()+
  geom_hline(yintercept = d)

#Question 12: Plotting raw percentages over time

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

#Make a graph of proportion over time colored by vote. Add a smooth trendline with geom_smooth() 
#and method = "loess" with a span of 0.3.

brexit_long %>%
  ggplot(aes(enddate,proportion,col=vote)) +
  geom_smooth(method = "loess",span=0.3)+
  scale_y_continuous(limits = c(0,0.6),breaks = seq(from = 0, to = 0.6, by = 0.1))
  
























