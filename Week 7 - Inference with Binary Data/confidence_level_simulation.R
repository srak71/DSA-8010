# simulation to demonstrate coverage probabilities
true.pi <- 0.36
alpha <- 0.07

n <- 125
x <- rbinom(n,size=1,prob=true.pi)
sum(x)
pi.hat <- sum(x)/n
pi.hat

se <- sqrt(pi.hat*(1-pi.hat)/n)
z.star <- qnorm(1-alpha/2,0,1)

interval <- c(pi.hat - z.star*se,
              pi.hat + z.star*se)
interval

# simulate repeated sampling
coverage <- rep(NA, n)
n.iterations <- 5e4

for( i in 1:n.iterations)
{
  # generate data
  x <- rbinom(n,size=1,prob=true.pi)
  
  # create CI
  pi.hat <- sum(x)/n
  se <- sqrt(pi.hat*(1-pi.hat)/n)
  z.star <- qnorm(1-alpha/2,0,1)
  interval <- c(pi.hat - z.star*se,
                pi.hat + z.star*se)
  
  coverage[i] <- (true.pi < interval[2]) & (true.pi > interval[1])
}

mean(coverage)
