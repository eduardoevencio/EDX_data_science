prev <- 1/3900    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

# number of people with disease
N_D <- sum(outcome == "Disease")

# number of people without disease
N_H <- sum(outcome == "Healthy")

#for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table <- data.frame(table(outcome, test))

#finds the chance of someone having the disease given that the test was positive
chance <- table[table$outcome=="Disease" & table$test=="+",]$Freq/
  (table[table$outcome=="Disease" & table$test=="+",]$Freq +
     table[table$outcome=="Healthy" & table$test=="+",]$Freq)
chance