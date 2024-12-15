# ================================
# crab data - descriptive analysis
# ================================

crabs <- read.csv("Data/all_crabs.csv",header=T)

head(crabs)
table(crabs$species)
class(crabs$species)

crabs$species <- as.factor(crabs$species)
head(crabs)


boxplot(crabs$rear_width ~ crabs$species)

summary(subset(crabs,species=='blue')$rear_width)
summary(subset(crabs,species=='orange')$rear_width)


# ==================================
# Airbnbs - CI for price difference
# ==================================
ybar1 <- 74.9
n1 <- 9
s1 <- 21.8

ybar2 <- 212.7
n2 <- 38
s2 <- 193


# find degrees of freedom
C <- (s1^2/n1)/(s1^2/n1 + s2^2/n2)
C  # C will always be between 0 and 1
degrees.of.freedom <- (n1-1)*(n2-1)/((1-C)^2*(n1-1)+C^2*(n2-1))

# use degrees of freedom to find t*
alpha <- 0.05
tstar <- qt(1-alpha/2,df=degrees.of.freedom)

# lower and upper endpoints
ybar1 - ybar2 - tstar*sqrt(s1^2/n1 + s2^2/n2)
ybar1 - ybar2 + tstar*sqrt(s1^2/n1 + s2^2/n2)


# ===============================================
# Airbnb: test H0: mu1 - mu2 = 0
# vs. HA: mu1 - mu2 < 0  (entire homes are more expensive)
# ===============================================

ybar1 <- 74.9
n1 <- 9
s1 <- 21.8

ybar2 <- 212.7
n2 <- 38
s2 <- 193


# find degrees of freedom
C <- (s1^2/n1)/(s1^2/n1 + s2^2/n2)
degrees.of.freedom <- (n1-1)*(n2-1)/((1-C)^2*(n1-1)+C^2*(n2-1))


# calculate the test statistic
t0 <- (ybar1 - ybar2)/sqrt(s1^2/n1 + s2^2/n2)
t0

# The alternative is one-sided, so pvalue is P(T<t0)
pt(t0, df=degrees.of.freedom)


# ========================================
# Crabs: test H0: mu1-mu2 = 0
# vs. HA: mu1 - mu2 =/= 0
# alpha = 0.05
# ========================================

t.test(crabs$rear_width~crabs$species, var.equal=TRUE)


# =========================================
# Crabs: what if the data came in
# two separate datasets?
# =========================================

blue_crabs <- read.csv("Data/blue_crabs.csv",header=T)
orange_crabs <- read.csv("Data/orange_crabs.csv",header=T)

head(blue_crabs)
head(orange_crabs)

t.test(blue_crabs$rear_width, orange_crabs$rear_width,
       var.equal=TRUE)


# ==============================================
# model checking - are measurements approximately 
# normal within each group?
# ==============================================

par(mfrow=c(1,2))
qqnorm(subset(crabs, species=='blue')$rear_width,
       main='blue crabs')

qqnorm(subset(crabs, species=='orange')$rear_width,
       main='orange crabs')
