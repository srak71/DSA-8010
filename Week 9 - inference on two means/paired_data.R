# set working directory and read in the data
setwd("C:/Users/2administrator/Dropbox/DSA 8010/Labs/Data")
gradrates <- read.csv("Data/gradrates.csv",header=T)
head(gradrates)

names(gradrates) <- c("school","gr1617","gr1718")
gradrates

# univariate summaries
summary(gradrates$gr1617)
summary(gradrates$gr1718)

boxplot(gradrates$gr1617, gradrates$gr1718)

# assess association
plot(gradrates$gr1617, gradrates$gr1718)
cor(gradrates$gr1617,gradrates$gr1718)


# summarize differences
gradrates$gr1617 -gradrates$gr1718
gradrates$differences <- gradrates$gr1617 -gradrates$gr1718

gradrates

# univariate summaries of differences
# this is a better descriptive approach
summary(gradrates$differences)
boxplot(gradrates$differences)


# ================================
# inference on mu_d
# ================================

# CI for mu_d: average diff. between 2016-2017 grad rates
# and 2017-2018 grad rates

t.test(gradrates$differences, conf.level=.90)

qqnorm(gradrates$differences)
qqline(gradrates$differences,col='red')

diff_no_outlier <- gradrates$differences[-3]
t.test(diff_no_outlier, conf.level=.9)
qqnorm(diff_no_outlier)
qqline(diff_no_outlier)


# hypothesis test:
# Is there evidence that grad rates increased?

# H_0: mu_d = 0
# H_A: mu_d < 0
# alpha =0.01

t.test(diff_no_outlier, alternative="less")
t.test(gradrates$differences, alternative="less")
