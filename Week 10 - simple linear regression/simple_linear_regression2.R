# =====================================
# DSA 8010 - Simple linear regression
# Part 2
# =====================================

starbucks <- read.csv("Data/starbucks.csv")
head(starbucks)

# ================================
# residual analysis
# ================================

# fit a linear regression model
lm_starbucks<- lm(carb~calories,data=starbucks)

# view coefficients, standard errors, p-values
summary(lm_starbucks)
class(lm_starbucks)

# default plots from R
plot(lm_starbucks)

# calculate residuals
starbucks_resid <- residuals(lm_starbucks)
starbucks_fitted <- fitted(lm_starbucks)

plot(starbucks$calories,starbucks_resid , main='x vs residuals',
     xlab='calories',ylab='residuals')
# add horizontal line at 0 
abline(h=0)

# Fitted vs residuals
# has the exact same pattern -- but this is not the case with multiple linear regression
plot(starbucks_fitted,starbucks_resid , main='fitted vs residuals',
     xlab='calories',ylab='residuals')
# add horizontal line at 0 
abline(h=0)

# normal quantile plot
qqnorm(starbucks_resid)


# ===============================================
# R squared
# ===============================================
summary(lm_starbucks)

# or to save the R^2 as an object
starbucks_rsq <- summary(lm_starbucks)$r.squared
starbucks_rsq


