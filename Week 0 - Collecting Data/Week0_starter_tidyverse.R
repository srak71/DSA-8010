setwd("C:/Users/2administrator/Dropbox/DSA 8010/Labs/Data")
getwd()
#install.packages("tidyverse")
library(tidyverse)

highschools <- read.csv("Chicago_HS.csv",header=TRUE)

# view data
view(highschools)
glimpse(highschools)

# investigate object classes
class(highschools)
class(highschools$Student_Count_Total)
class(highschools$Dress_Code)

# convert dress code into a factor
highschools$Dress_Code <- as.factor(highschools$Dress_Code)
class(highschools$Dress_Code)

# make histogram of Student Count Total


# add labels

# make a boxplot of "Student_Count_Low_Income"

# save ggplot as object
g1 <- 
  
g1

# scatter plot of "Average_ACT_School" vs "College_Enrollment_Rate_School"
ggplot(data=highschools, mapping=aes(x=Average_ACT_School, y=College_Enrollment_Rate_School)) +
  geom_point()


# bar plot of "Dress_Code"
ggplot(highschools, aes(x = Dress_Code)) + 
  geom_bar() 

# summary statistics
# see "useful functions" https://dplyr.tidyverse.org/reference/summarise.html
highschools |> summarise(mean_student_count=mean(Student_Count_Total),
                         median_student_count=median(Student_Count_Total),
                         sd_student_count=sd(Student_Count_Total))

# ====================================
# subsetting with dplyr
# ====================================

# extract rows for all schools with >1500 students
highschools |> filter(Student_Count_Total > 1500)

# extract rows for all schools with no dress code
highschools |> filter(Dress_Code == 'N')

# plot histogram of Student Count Total for schools with no dress code
highschools |> filter(Dress_Code == 'N') |> ggplot(aes(x=Student_Count_Total)) + geom_histogram(bins=25)
