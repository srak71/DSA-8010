# ===========================================================
# Example: precipitation
# Use this script to follow along with test scores lectures.
# Some lines of code may be slightly different than seen in
# the video.
# ===========================================================
precip <- read.csv('chicago_precip.csv')
names(precip)
head(precip)

# Find mean preciptation by year at Station 304
tapply(precip$station304,precip$year,mean)

# how many missing values are there?
precip$station304==-99.9
sum(precip$station304==-99.9)
sum(precip$station314==-99.9)

# Another approach: read in data again but use NA for missings
precip2 <- read.csv('chicago_precip.csv',na.strings = -99.9)
head(precip2)

# how many missing values in station 304 and 314
sum(is.na(precip2$station314))
sum(is.na(precip2$station304))

tapply(precip2$station314,precip2$year,mean,na.rm=T)

# look into where missing values occur
precip.t1 <- table(precip2$weekday,is.na(precip2$station304))
precip.t1

barplot(precip.t1[,2])

table(precip2$month,is.na(precip2$station304))

# two barplots of month by missings
barplot(table(precip2$month,is.na(precip2$station304))[,2])
barplot(table(is.na(precip2$station304),precip2$month))

# missings for stations 314
barplot(table(is.na(precip2$station314),precip2$month))
barplot(table(is.na(precip2$station314),precip2$weekday))

# Are some days rainier than others?
boxplot(precip2$station314~precip2$weekday)
hist(precip2$station314)
sum(precip2$station314 ==0, na.rm=T)
hist(log(precip2$station314+1))
boxplot(log(precip2$station314+1)~precip2$weekday)

boxplot(log(precip2$station314+1)~precip2$month)
zero.table <- table(precip2$station314==0,precip2$month)
prop.table(zero.table,2)

# Are some months rainier than others?
tapply(precip2$station314,precip2$month,mean,na.rm=T)
tapply(precip2$station304,precip2$month,mean,na.rm=T)

# What about annual averages?
tapply(precip2$station304,precip2$year,mean,na.rm=T)

table(precip2$month,is.na(precip2$station304))