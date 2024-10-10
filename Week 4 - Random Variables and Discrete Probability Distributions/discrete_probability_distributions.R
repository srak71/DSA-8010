# ==================================
# example 1: customer counts
# ==================================

# probability of 5 customers
# using R as a calculator
(3.75)^5*exp(-3.75)/factorial(5)

# using built in function
dpois(5, lambda=3.75)

# probability of >1 customers
ppois(1, lambda=3.75,lower.tail=FALSE)

# ==================================
# Example 2: mendel's peas
# ==================================
# Prob of at least 8 yellow

# using R as a calculator
choose(10,8)*(.75)^8*(1-.75)^2 + choose(10,9)*(.75)^9*(1-.75)^1 +
  choose(10,10)*(.75)^10*(1-.75)^0

# using dbinom
dbinom(8,size=10,prob=0.75) + dbinom(9,size=10,prob=0.75) + dbinom(10,size=10,prob=0.75)

# using pbinom
pbinom(7,size=10,prob=0.75, lower.tail=FALSE)

# =======================================
# Example 3: binomial
# =======================================

# P(17 < Y <= 20)
# using dbinom
dbinom(18,size=21,prob=0.65)+dbinom(19,size=21,prob=0.65) + +dbinom(20,size=21,prob=0.65)

# using pbinom
pbinom(20,size=21,prob=0.65)-pbinom(17,size=21,prob=.65)


# P(17 <= Y < 20)
# using dbinom
dbinom(17,size=21,prob=0.65)+dbinom(18,size=21,prob=0.65) + dbinom(19,size=21,prob=0.65)

# using pbinom
pbinom(19,size=21,prob=0.65, lower.tail=TRUE)-pbinom(16,size=21,prob=.65, lower.tail=TRUE)
