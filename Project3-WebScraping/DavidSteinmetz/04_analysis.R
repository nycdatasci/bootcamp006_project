# David Richard Steinmetz
# Web scraping project
# NYC Data Science Academy
# Last modified: 2016-08-14



library(ggplot2)

# Gender Analysis ---------------------------------------------------------

# Boxplots
male_times <- dt[gender=='M',.(time)][['time']]
female_times <- dt[gender=='F',.(time)][['time']]
boxplot(dt[['time']] ~ dt[['gender']], 
        main = "Sample Distribution of Race Times",
        col = c("red", "blue"), names = c("Women", "Men"))

# Density plots
g <- ggplot(dt, aes(x = time))
g <- g + geom_density(aes(colour = gender))
g <- g + theme_minimal()
g <- g + ggtitle('Density Distribution of Race Times')
g <- g + xlab('Time (s)') + ylab('Density')
g <- g + scale_colour_discrete(name = "Gender")
g

# T-test to see if the sample means are significantly different
t.test(female_times, male_times, alternative = "two.sided")
# Welch Two Sample t-test
# 
# data:  female_times and male_times
# t = 23.613, df = 8779.4, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   892.3784 1053.9537
# sample estimates:
#   mean of x mean of y 
# 19955.15  18981.99 

# F-test to check the validity of the T-test
var.test(female_times, male_times, alternative = "two.sided") # T-test invalid!
# F test to compare two variances
# 
# data:  female_times and male_times
# F = 0.8733, num df = 4737, denom df = 13614, p-value = 2e-08
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.8336240 0.9153493
# sample estimates:
#   ratio of variances 
# 0.8732973 




# Year Analysis -----------------------------------------------------------

# Boxplots
# --Year
g <- ggplot(dt, aes(as.factor(year), time))
g <- g + geom_boxplot(aes(colour=as.factor(year)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Year') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Year")
g

# --Year reordered according to median time
g <- ggplot(dt, aes(reorder(as.factor(year), time, median), time))
g <- g + geom_boxplot(aes(colour=as.factor(year)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Year (ordered by median time)') + ylab('Time (s)')
g

# --Year + Gender
g <- ggplot(dt, aes(as.factor(year), time))
g <- g + geom_boxplot(aes(colour=gender))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Year') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Gender")
g

# --Year + Age Group
g <- ggplot(dt, aes(as.factor(year), time))
g <- g + geom_boxplot(aes(colour=age.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Year') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Age Group")
g

# --Year + Time Group
g <- ggplot(dt, aes(as.factor(year), time))
g <- g + geom_boxplot(aes(colour=time.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Year') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Time Group")
g

# Density plots
# --Year
g <- ggplot(dt, aes(time))
g <- g + geom_density(aes(colour = as.factor(year)))
g <- g + theme_minimal()
g <- g + ggtitle('Density Distribution of Race Times')
g <- g + xlab('Time (s)') + ylab('Density')
g <- g + scale_colour_discrete(name = "Year")
g







