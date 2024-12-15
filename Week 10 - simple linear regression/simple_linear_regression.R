# =====================================
# DSA 8010 - Simple linear regression
# Part 1
# =====================================

starbucks <- read.csv("Data/starbucks.csv")
head(starbucks)

# ================================
# descriptive analysis
# ================================

# create a scatterplot
plot(starbucks$calories, starbucks$carb, xlab='calories',ylab='carbs (grams)')

# calculate Pearson's correlation
cor(starbucks$calories, starbucks$carb)


# fit a linear regression model
lm_starbucks<- lm(carb~calories,data=starbucks)

# view coefficients, standard errors, p-values
summary(lm_starbucks)
summary(lm_starbucks)$coefficients
summary(lm_starbucks)$coefficients[2,2]

# you can also view just the coefficients
lm_starbucks$coefficients

# make a scatterplot with the regression line overlaid
plot(starbucks$calories, starbucks$carb, xlab='calories',ylab='carbs (grams)')
abline(a=lm_starbucks$coefficients[1], b= lm_starbucks$coefficients[2],col='darkgreen')

# predict Y at a new X value
newX <- 150  # number of calories

beta0_hat <- as.numeric(lm_starbucks$coefficients[1])
beta1_hat <- as.numeric(lm_starbucks$coefficients[2])
predictedY <-  beta0_hat + beta1_hat*newX
predictedY


# ======================================================
# Inference on regression parameters
# Make confidence intervals for the intercept and slope
# ======================================================

# this code retrieves the estimates AND standard errors
# lm_starbucks$coefficients just gives the estimates.
starbucks_coefficients <- summary(lm_starbucks)$coefficients
starbucks_coefficients

# 99% CI for the intercept
n <- nrow(starbucks)
tstar <- qt(.995,df=n-2)

beta0_hat <- starbucks_coefficients[1,1]
se_beta0_hat <- starbucks_coefficients[1,2]

lower <- beta0_hat - tstar*se_beta0_hat
upper <- beta0_hat + tstar*se_beta0_hat

# print the confidence interval
c(lower,upper)


# 95% CI for the slope
n <- nrow(starbucks)
tstar <- qt(.975,df=n-2)

# get estimate and standard error from row 2 of the coefficients matrix
beta1_hat <- starbucks_coefficients[2,1]
se_beta1_hat <- starbucks_coefficients[2,2]

lower_slope <- beta1_hat - tstar*se_beta1_hat
upper_slope <- beta1_hat + tstar*se_beta1_hat

# print the confidence interval
c(lower_slope,upper_slope)

# here is one more way to find the same thing!
# 99% confidence intervals for both coefficients
confint(lm_starbucks, level=.99)
confint(lm_starbucks,level=.95)

# ============================================
# Hypothesis tests for coefficients
# ============================================


# Q1- H0: beta1 = 0; HA: beta1 =/= 0
# using alpha = 0.05

# This is the p-value for the test
starbucks_coefficients[2,4]
starbucks_coefficients[2,3]

# Reject H0!


# Q2- H0: beta0 = 6; HA: beta0 =/= 6
# using alpha=0.05

t0 <- (beta0_hat - 6)/se_beta0_hat
t0

# find two-sided p-value from t0
2*pt(t0,df=n-2,lower.tail=FALSE)
