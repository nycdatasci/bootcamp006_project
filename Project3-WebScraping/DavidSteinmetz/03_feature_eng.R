# David Richard Steinmetz
# Web scraping project
# NYC Data Science Academy
# Last modified: 2016-08-14




# Feature creation --------------------------------------------------------

# Create age buckets
library(Hmisc)
age.cat <- cut2(dt$age, g=7)  # Split age based on 7 quantile blocks
barplot(table(age.cat))


