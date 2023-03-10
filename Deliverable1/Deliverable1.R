installed.packages("rio")
installed.packages("ggplot2")
installed.packages("DescTools")

install_formats()

rm(list = ls())

library(rio)
library(ggplot2)
library(DescTools)

link='https://github.com/superadlerrrk/543/blob/main/Deliverable1/Crime_Data.csv?raw=true'
crime = import(link)

names(crime)

head(crime$"Primary Offense Description",30) 

# check which columns contain null values
cols_with_nulls <- colSums(is.na(crime))

# print the column names with null values
names(crime)[cols_with_nulls > 0]

any(is.na(crime$"Reported Time"))
any(is.na(crime$"Occurred Time"))

sum(is.na(crime$"Reported Time"))
sum(is.na(crime$"Occurred Time"))

# create a logical vector indicating which rows have complete data in my_column
complete_rows <- complete.cases(crime$"Reported Time")
complete_rows <- complete.cases(crime$"Occurred Time")
# subset the data frame to remove rows with null values in my_column
crime <- crime[complete_rows, ]

sum(is.na(crime$"Reported Time"))
sum(is.na(crime$"Occurred Time"))

# absolute values
absoluteT=table(crime$Precinct)

absoluteT

# relative values
prop.table(absoluteT)

ptgT=prop.table(absoluteT)*100
ptgT

pie(absoluteT,cex=0.6)



