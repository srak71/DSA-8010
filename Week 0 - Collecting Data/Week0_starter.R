getwd()
setwd("/users/dekunke/Dropbox/DSA8010/Labs/Data")
getwd()

highschools <- read.csv("Chicago_HS.csv",header=TRUE)

# view first 5 rows
head(highschools)

# investigate object classes
class(highschools)
class(highschools$Student_Count_Total)
class(highschools$Dress_Code)



# convert dress code into a factor
highschools$Dress_Code <- as.factor(highschools$Dress_Code)

class(highschools$Dress_Code)

# make some exploratory plots
hist(highschools$Student_Count_Total, main="histogram of student count",
     xlab='count',breaks=5)


# make a boxplot of "Student_Count_Low_Income"


# change title, axis labels, and orientation

# save boxplot as object


# scatter plot of "Average_ACT_School" vs "College_Enrollment_Rate_School"


# frequency table of "Dress_Code"


# summary statistics
mean(highschool$Student_Count_Total)
median(highschool$Student_Count_Total)
sd(highschool$Student_Count_Total)^2
var(highschool$Student_Count_Total)
summary(highschool$Student_Count_Total)
cor(highschool$Student_Count_Total, highschool$Graduation_Rate_Schopl)

# ====================================
# subsetting with R
# ====================================

# extract rows 1-19 of the data frame

# extract row 24 of the data frame

# extract rows for all schools with >1500 students

# extract just column 4

# extract columns 4,6, 9, and 10
