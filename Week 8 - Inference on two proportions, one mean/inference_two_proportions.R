# ===================================
# Example: texting and speeding
# ===================================

# large sample CI
pihat1 <- 25/305
pihat2 <- 45/450

# lower and upper endpoints
pihat1 - pihat2 + qnorm(.95)*sqrt(pihat1*(1-pihat1)/305 +
                                    pihat2*(1-pihat2)/450)

pihat1 - pihat2 - qnorm(.95)*sqrt(pihat1*(1-pihat1)/305 +
                                    pihat2*(1-pihat2)/450)

# Not covered in lecture: use prop.test
prop.test(x=c(25,45),n=c(305,450),conf.level=0.9,correct=FALSE)
prop.test(x=c(25,45),n=c(305,450),conf.level=0.9,correct=TRUE)

# ===================================
# Example: blood thinners
# ===================================

# Blood thinners using formula
# sample size in control and tmt groups
n1 <- 50; n2 <- 40

# number of successes in control and tmt groups
x1 <- 11; x2 <- 14

pihat1 <- x1/n1
pihat2 <- x2/n2
pihat.pool <- (x1+x2)/(n1+n2)

# test statistic
z0 <- (pihat1 - pihat2)/sqrt(pihat.pool*(1-pihat.pool)/n1 +
                               pihat.pool*(1-pihat.pool)/n2)

# p-value
2*pnorm(abs(z0),lower.tail=FALSE)

# Blood thinners using prop.test
# sample size in control and tmt groups
n1 <- 50; n2 <- 40
x1 <- 11; x2 <- 14
prop.test(x=c(x1,x2),n=c(n1,n2),alternative='two.sided',
          correct=TRUE)