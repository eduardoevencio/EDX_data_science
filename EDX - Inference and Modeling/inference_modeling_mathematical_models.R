#generates results of spread for a single pollster with J polls
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

#generates results of spread for I pollster with J polls
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#generates results of spread for I pollster with J polls accounting for pollster to pollster variability
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#generates the probability of true spread d>0
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)#accounts for general bias in the standard error for the avarega of the spreads
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)