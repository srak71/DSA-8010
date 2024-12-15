# ======================================
# Chi-square test in R
# ======================================


# iPod example
# Input data as a table
ipod <- as.table(rbind(c(2,23,36), c(71,50,37)))
rownames(ipod) <- c("disclose","hide")
colnames(ipod) <- c("general","positive","negative")

# tables of row proportions and column proportions
prop.table(ipod,margin=1)
prop.table(ipod,margin=2)

# barplots: stacked and grouped
barplot(ipod)
barplot(ipod, beside=TRUE)

# chisquare test
chisq.test(ipod)

ipod_results <- chisq.test(ipod)
names(ipod_results)

# show expected counts
ipod_results$expected
ipod_results$observed

# print test statistic
ipod_results$statistic

# contributions to the test statistic
# both lines of code find the same thing
(ipod_results$observed-ipod_results$expected)^2/ipod_results$expected
(ipod_results$residuals)^2



# Data in spreadsheet format: make a table first.
# simulated example:

# simulate data similar to the ipod example
n <- 340
disclose_sim <- c("disclose","hide")[sample(1:2,n,replace=TRUE)]
assumptions_sim <- c("general","positive","negative")[sample(1:3,n,replace=TRUE)]

# use the table function and save it
table(disclose_sim, assumptions_sim)
ipod_sim_table <- table(disclose_sim, assumptions_sim)

# perform the test
chisq.test(ipod_sim_table)
