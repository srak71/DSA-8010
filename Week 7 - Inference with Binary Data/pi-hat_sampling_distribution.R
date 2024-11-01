n <- 600
pi <- 0.997

x <- rbinom(n,size=1,prob=pi)
sum(x)/n



n.iterations <- 5e4
pi.hat.samples <- rep(NA,n.iterations)

for( i in 1:n.iterations)
{
  pi.hat.samples[i] <- sum(rbinom(n,1,prob=pi))/n
}

hist(pi.hat.samples)