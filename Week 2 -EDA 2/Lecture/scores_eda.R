# ===========================================================
# Example 1: test scores
# Use this script to follow along with test scores lectures.
# Some lines of code may be slightly different than seen in
# the video.
# ===========================================================


#setwd("~/Dropbox/DSA 8010/Labs/Data")
setwd("C:/Users/2administrator/Dropbox/DSA 8010/Labs/Data")

scores <- read.csv("test_scores.csv",header=T)
head(scores)
names(scores)

scores$grade <- as.factor(scores$grade)

# look at univariate distributions
table(scores$grade)
hist(scores$math)
hist(scores$reading)

hist(scores$poverty)
hist(scores$minority)

mean(scores$math, na.rm=TRUE)
mean(scores$reading, na.rm=TRUE)

median(scores$math, na.rm=TRUE)
median(scores$reading, na.rm=TRUE)

table(scores$school)

# any missings?
sum(is.na(scores$math))
sum(is.na(scores$reading))
sum(is.na(scores$poverty))
sum(is.na(scores$minority))
subset(scores, is.na(scores$minority))
subset(scores,school==19)

# explore associations
plot(scores$school, scores$math)
plot(scores$reading, scores$math)
cor(scores$reading,scores$math,use="complete.obs")

plot(scores$poverty, scores$math)
plot(scores$poverty, scores$reading)
cor(scores$poverty, scores$math,use='complete.obs')
cor(scores$poverty, scores$reading,use='complete.obs')

plot(scores$poverty, scores$minority)
cor(scores$poverty,scores$minority,use='complete.obs')
cor(scores$poverty,scores$minority, method='spearman',
    use='complete.obs')

boxplot(scores$reading~scores$grade)
boxplot(scores$poverty~scores$grade)

# use tapply to find summary statistics by group
tapply(scores$reading,scores$grade,mean)
tapply(scores$reading,scores$grade,mean, na.rm=T)

tapply(scores$reading,scores$grade,sd, na.rm=T)
tapply(scores$reading,scores$grade,quantile, .40,na.rm=T)


# add a 3rd variable - use color a lazy way
plot(scores$poverty, scores$math,
     col=scores$grade)

# add a 3rd variable - choose colors
color.names <- c("red","darkgreen","navy")
plot(scores$poverty, scores$math,
     col=color.names[scores$grade])

# find sample correlation by grade
tapply(cbind(scores$poverty, scores$math),
       scores$grade, cor,use='complete.obs')

scores_grade3 <- subset(scores, grade==3)
dim(scores_grade3)
cor(scores_grade3$poverty,scores_grade3$math,use='complete.obs')

scores_grade4 <- subset(scores, grade==4)
dim(scores_grade4)
cor(scores_grade4$poverty,scores_grade4$math,use='complete.obs')


scores_grade5 <- subset(scores, grade==5)
dim(scores_grade5)
cor(scores_grade5$poverty,scores_grade5$math,use='complete.obs')


# clean up plot and save it
plot(scores$poverty, scores$math,
     col=color.names[scores$grade],
     ylab='math score',xlab='poverty (%)',
     main='Test scores and poverty rate')
legend('topright',legend=c("grade 3",
                           "grade 4",
                           "grade 5"),
       col=c("red","darkgreen","navy"),
       pch=1, cex=.6)

# optional: save plot using code
# uncomment the lines starting with "png" and "dev.off()"
#png(filename="scores_scatterplot.png",width=500,
#    height=400)
plot(scores$poverty, scores$math,
     col=color.names[scores$grade],
     ylab='math score',xlab='poverty (%)',
     main='Test scores and poverty rate')
legend('topright',legend=c("grade 3",
                           "grade 4",
                           "grade 5"),
       col=c("red","darkgreen","navy"),
       pch=1, cex=.6)

#dev.off()