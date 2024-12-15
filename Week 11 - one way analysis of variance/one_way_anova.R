setwd("~/Dropbox/DSA 8010/Labs")



# ===========================================
# Typing data - ANOVA the ordinary way
# ===========================================

# read data in
typing <- read.csv('Data/typing.csv')
head(typing)

# find mean and standard deviation by group
# Approach 1: using subsets

mean(subset(typing, brand=="REGAL")$speed)
sd(subset(typing, brand=="REGAL")$speed)

mean(subset(typing, brand=="SPEEDYTYPE")$speed)
sd(subset(typing, brand=="SPEEDYTYPE")$speed)

mean(subset(typing, brand=="WORD-O-MATIC")$speed)
sd(subset(typing, brand=="WORD-O-MATIC")$speed)

# optional approach 2: the tapply function
group_means <- tapply(typing$speed, typing$brand, mean)
group_sds <- tapply(typing$speed, typing$brand, sd)



# ===========================================
# Fit the one-way anova model
# ===========================================

lm_typing <- lm(speed~brand,data=typing)
anova(lm_typing)

anova_typing <- anova(lm_typing)
names(anova_typing)

# use the $ to extract specific numbers
anova_typing$`Pr(>F)`
anova_typing$`Sum Sq`


# ==============================================
# Mosquito data - ANOVA the long way
# ==============================================
mosquito <- read.csv("Data/mosquito.csv")

boxplot(mosquito$Mosquito.bite.rate~ mosquito$Treatment.group)

t <- length(unique(mosquito$Treatment.group)) 
nT <- nrow(mosquito)

# calculate the means within each treatment
ybar_treatment <- tapply(mosquito$Mosquito.bite.rate, mosquito$Treatment.group,mean)
ybar_treatment

# calculate sample size within each group
n_treatment <- tapply(mosquito$Mosquito.bite.rate, mosquito$Treatment.group,length)
n_treatment

# ================================
# Finding SSW
# ================================

# first let's focus on group 1 to get some intuition.
response_group1 <- mosquito$Mosquito.bite.rate[mosquito$Treatment.group==1]
response_group1

# sum of squares within group 1
sum((response_group1-ybar_treatment[1])^2)

# the SSW is the within group sum of squares added up for all groups. 
# You can find this many ways.  Here is a way using a "for loop":

ssw <- 0
for( j in 1:t)
{
  response_groupj <- mosquito$Mosquito.bite.rate[mosquito$Treatment.group==j]
  
  # add the group j SS to the total SSW
  ssw <- ssw + sum((response_groupj-ybar_treatment[j])^2)
}

ssw

# ==================================
# SSB
# ==================================
overall_mean <- mean(mosquito$Mosquito.bite.rate)

(ybar_treatment-overall_mean)^2
ssb <- sum(n_treatment*((ybar_treatment-overall_mean)^2))
ssb

# ========================================
# calculate the missing ANOVA table values
# ========================================

# total sum of squares
ssb + ssw

# mean squares
s.squared.b <- ssb/(t-1)
s.squared.w <- ssw/(nT-t)

# f0 test statistic
f0 <- s.squared.b/s.squared.w
f0

# p-value
pf(f0,df1=t-1,df2=nT-t,lower.tail=FALSE)

# compare these to the automatic ANOVA

# cautionary tale -- something is wrong here
# notice df for treatment is 1, while it should be 4

lm_mosquito <- lm(Mosquito.bite.rate~Treatment.group, data=mosquito)
anova(lm_mosquito)

# convert the group indicator to be a factor!
mosquito$Treatment.group <- as.factor(mosquito$Treatment.group)

lm_mosquito2 <- lm(Mosquito.bite.rate~Treatment.group, data=mosquito)
anova(lm_mosquito2)



# =========================================
# Residual analysis
# =========================================

# Typing data (make sure to run the lm() first to create lm_typing)
typing_residuals <- residuals(lm_typing)

stripchart(typing_residuals~typing$brand, vertical=TRUE,
           main='residuals by group - typing data',
           ylab='residuals')
qqnorm(typing_residuals)
qqline(typing_residuals,col='darkgreen')

# FYI, you can use stripchart to visualize the data by group too
stripchart(typing$speed~typing$brand,vertical=TRUE)



# Mosquito data
mosquito_residuals <- residuals(lm_mosquito2)

stripchart(mosquito_residuals~mosquito$Treatment.group, vertical=TRUE,
           main='residuals by group -\n mosquito data',
           ylab='residuals')
qqnorm(mosquito_residuals)
qqline(mosquito_residuals,col='darkgreen')
