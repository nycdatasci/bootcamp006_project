# David Richard Steinmetz
# Web scraping project
# NYC Data Science Academy
# Last modified: 2016-08-14




# Feature creation --------------------------------------------------------

# Create age buckets
library(Hmisc)
barplot(table(cut2(dt$age, g=7)))  # View
dt[, age.cat:= cut2(age, g=7)]  # Split age into 7 quantile blocks

# Create time buckets
barplot(table(cut2(dt$time, g=7)))  # View
dt[, time.cat:= cut2(time, g=7)]  # Split time into 7 quantile blocks

