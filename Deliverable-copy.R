#install packages for analysis
installed.packages("rio")
installed.packages("ggplot2")
installed.packages("DescTools")
install.packages("dplyr")

install_formats()

rm(list = ls())

library(rio)
library(ggplot2)
library(DescTools)
library(dplyr)

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

crime <- crime %>%
  mutate(Precinct = if_else(Precinct == "", "UNKNOWN", Precinct))

crime_grouped <- crime %>%
  group_by(Precinct, Neighborhood) %>%
  summarise(n = n())

crime_totals <- crime_grouped %>%
  group_by(Precinct) %>%
  summarise(total_entries = sum(n))

crime_grouped <- crime_grouped %>%
  left_join(crime_totals, by = "Precinct") %>%
  mutate(ratio = n / total_entries)

precincts <- unique(crime_grouped$Precinct)

for (precinct in precincts) {
  data <- filter(crime_grouped, Precinct == precinct)
  plot_title <- paste0(precinct, " Crime Entries by Neighborhood")
  
  pie_chart <- ggplot(data, aes(x="", y=ratio, fill=Neighborhood)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    labs(title=plot_title) +
    theme_void()
  
  print(pie_chart)
}

crime$grouped_time <- cut(crime$"Occurred Time", 
                          breaks = c(0, 1, 559, 1159, 1759, 2359),
                          labels = c("Unknown", "Midnight", "Morning", "Afternoon", "Evening"),
                          include.lowest = TRUE)

table(crime$Precinct, crime$grouped_time)

# Get the counts of each group in grouped_time
time_counts <- table(crime$grouped_time)

# Plot a bar plot
barplot(time_counts, 
        main = "Occurrences by Time of Day", 
        xlab = "Time of Day", 
        ylab = "Count")

ggplot(crime, aes(x = grouped_time, fill = Precinct)) + 
  geom_bar() + 
  labs(x = "Grouped Occurred Time", y = "Count of Entries")

crime$offense_category <- gsub("-.*", "", crime$`Primary Offense Description`)

off_ctgr <- unique(crime$`offense_category`)

# Create a data frame with the count of entries by offense category
offense_counts <- data.frame(table(crime$offense_category))

# Sort the offense categories by count of entries in descending order
sorted_offenses <- offense_counts[order(offense_counts$Freq, decreasing = TRUE), ]

# Select the top 10 offense categories by count of entries
top_10_offenses <- head(sorted_offenses, 10)

# Create a bar plot of the top 10 offense categories
ggplot(top_10_offenses, aes(x = reorder(Var1, Freq), y = Freq, fill = Var1)) +
  geom_col() +
  xlab("Offense Category") +
  ylab("Count of Entries") +
  ggtitle("Top 10 Offense Categories by Count of Entries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
