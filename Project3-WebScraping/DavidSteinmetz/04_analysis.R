# David Richard Steinmetz
# Web scraping project
# NYC Data Science Academy
# Last modified: 2016-08-14



library(ggplot2)

# Gender Analysis ---------------------------------------------------------

dt[, .N, by = gender]  # Far fewer women
dt[, .N, by = .(gender, year)]

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
var.test(female_times, male_times, alternative = "two.sided") # T-TEST INVALID!
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

dt[, .N, by = year]  # Far fewer in 2012
mean(dt[, .N, by = year][['N']])
median(dt[, .N, by = year][['N']])

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
g <- g + scale_colour_discrete(name = "Year")
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

# --Year + Place Group
g <- ggplot(dt, aes(as.factor(year), time))
g <- g + geom_boxplot(aes(colour=place.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Year') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Place Group")
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

# One-way ANOVA between Years
summary(aov(time ~ year, data = dt))  # Sig. diff in mean times across years
# Check assumptions:
# 1. Population is normally distributed
# 2. SD of populations are equal
dt[,.(SD_time = sd(time)), by = year]
t.test(dt[year==2012][['time']], 
       dt[year==2015][['time']], 
       alternative = 'two.sided')  # P-value 2.2e-16; means are different
var.test(dt[year==2012][['time']],
         dt[year==2015][['time']],
         alternative = "two.sided") # P-value 0.4273; 
                                    # Null hypothesis is kept: variance between
                                    # 2012 and 2015 is the same
# 3. Obs. are randomly drawn and independent




# Age Analysis ------------------------------------------------------------

dt[, .N, by=age.cat]

# Boxplots
# --Age Group
g <- ggplot(dt[!is.na(age.cat)], aes(as.factor(age.cat), time))
g <- g + geom_boxplot(aes(colour=as.factor(age.cat)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Age Category")
g

# --Age Group (Place)
g <- ggplot(dt[!is.na(age.cat)], aes(as.factor(age.cat), place))
g <- g + geom_boxplot(aes(colour=as.factor(age.cat)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age Group') + ylab('Place')
g <- g + scale_colour_discrete(name = "Age Category")
g

# --Age Group reordered according to median time
g <- ggplot(dt[!is.na(age.cat)], aes(reorder(as.factor(age.cat), time, median), time))
g <- g + geom_boxplot(aes(colour=as.factor(age.cat)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age Group (ordered by median time)') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Year")
g

# --Age Group + Gender
g <- ggplot(dt[!is.na(age.cat)], aes(as.factor(age.cat), time))
g <- g + geom_boxplot(aes(colour=gender))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Gender")
g

# --Age Group + Year
g <- ggplot(dt[!is.na(age.cat)], aes(as.factor(age.cat), time))
g <- g + geom_boxplot(aes(colour=as.factor(year)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Age Group")
g

# --Age Group + Time Group
g <- ggplot(dt[!is.na(age.cat)], aes(as.factor(age.cat), time))
g <- g + geom_boxplot(aes(colour=time.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Time Group")
g

# --Age Group + Place Group
g <- ggplot(dt[!is.na(age.cat)], aes(as.factor(age.cat), time))
g <- g + geom_boxplot(aes(colour=place.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Place Group")
g


# Density plots
# --Age Group
g <- ggplot(dt[!is.na(age.cat)], aes(time))
g <- g + geom_density(aes(colour = as.factor(age.cat)))
g <- g + theme_minimal()
g <- g + ggtitle('Density Distribution of Race Times')
g <- g + xlab('Time (s)') + ylab('Density')
g <- g + scale_colour_discrete(name = "Age Group")
g

# One-way ANOVA between Age Groups
summary(aov(time ~ age.cat, data = dt))  # Sig. diff in mean time across ages
# Check assumptions:
# 1. Population is normally distributed
# 2. SD of populations are equal
dt[,.(SD_time = sd(time)), by = age.cat]
t.test(dt[age.cat=='[17,35)'][['time']], 
       dt[age.cat=='[57,77]'][['time']], 
       alternative = 'two.sided')  # P-value 2.2e-16; means are different
var.test(dt[age.cat=='[17,35)'][['time']],
         dt[age.cat=='[57,77]'][['time']],
         alternative = "two.sided") # P-value 2.2e-16; T-TEST INVALID
# Null hypothesis is rejected: variance between
# age groups is not the same and therefore the T-test is invalid.
# 3. Obs. are randomly drawn and independent


# Scatterplot: Age-Time
g <- ggplot(dt, aes(age, time))
g <- g + geom_point(aes(colour=as.factor(gender)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age (years)') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Age")
g

# Scatterplot + Jitter: Age-Time
g <- ggplot(dt, aes(age, time))
g <- g + geom_jitter(aes(colour=as.factor(gender)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age (years)') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Age")
g

# Scatterplot: Age-Place
g <- ggplot(dt, aes(age, place))
g <- g + geom_point(aes(colour=as.factor(gender)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Age (years)') + ylab('Place')
g <- g + scale_colour_discrete(name = "Age")
g


# Time Group Analysis -----------------------------------------------------

dt[, .N, by=time.cat]

# Boxplots
# --Time Group
g <- ggplot(dt, aes(as.factor(time.cat), time))
g <- g + geom_boxplot(aes(colour=as.factor(time.cat)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Time Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Time Category")
g

# --Time Group reordered according to median time
g <- ggplot(dt, aes(reorder(as.factor(time.cat), time, median), time))
g <- g + geom_boxplot(aes(colour=as.factor(time.cat)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Time Group (ordered by median time)') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Time Group")
g

# --Time Group + Gender
g <- ggplot(dt, aes(as.factor(time.cat), time))
g <- g + geom_boxplot(aes(colour=gender))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Time Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Gender")
g

# --Time Group + Year
g <- ggplot(dt[!is.na(time.cat)], aes(as.factor(time.cat), time))
g <- g + geom_boxplot(aes(colour=as.factor(year)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Time Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Year")
g

# --Time Group + Age Group
g <- ggplot(dt, aes(as.factor(time.cat), time))
g <- g + geom_boxplot(aes(colour=age.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Time Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Age Group")
g

# --Time Group + Place Group
g <- ggplot(dt, aes(as.factor(time.cat), time))
g <- g + geom_boxplot(aes(colour=place.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Time Group') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Place Group")
g

# Density plots
# --Time Group
g <- ggplot(dt, aes(time))
g <- g + geom_density(aes(colour = as.factor(time.cat)))
g <- g + theme_minimal()
g <- g + ggtitle('Density Distribution of Race Times')
g <- g + xlab('Time (s)') + ylab('Density')
g <- g + scale_colour_discrete(name = "Time Group")
g



# Nationality Analysis ----------------------------------------------------

nat.N <- head(dt[, .N, by = nat][order(-N)],10)  # Countries with the most participants
nat <- subset(dt, nat %in% c('SUI', 'GER', 'NED', 'GBR', 'ITA'))

# Barchart by nationality
g <- ggplot(nat.N, aes(nat, N))
g <- g + geom_bar(stat='identity')
g <- g + theme_minimal()
g <- g + ggtitle('Number of Participants by Nationality')
g <- g + xlab('Country') + ylab('Number')
g <- g + coord_flip()
g


# Boxplots
# --Nationality
g <- ggplot(nat, aes(as.factor(nat), time))
g <- g + geom_boxplot(aes(colour=as.factor(nat)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Year') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Year")
g

# --Nationality reordered according to median time
g <- ggplot(nat, aes(reorder(as.factor(nat), time, median), time))
g <- g + geom_boxplot(aes(colour=as.factor(nat)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Nationality (ordered by median time)') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Nationality")
g

# --Nationality + Gender
g <- ggplot(nat, aes(as.factor(nat), time))
g <- g + geom_boxplot(aes(colour=gender))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Nationality') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Gender")
g

# --Gender + Nationality
g <- ggplot(nat, aes(as.factor(gender), time))
g <- g + geom_boxplot(aes(colour=reorder(nat, time)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Gender') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Nationality")
g

# --Nationality + Year
g <- ggplot(nat, aes(as.factor(nat), time))
g <- g + geom_boxplot(aes(colour=as.factor(year)))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Nationality') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Year")
g

# --Nationality + Time Group
g <- ggplot(nat, aes(as.factor(nat), time))
g <- g + geom_boxplot(aes(colour=time.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Nationality') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Time Group")
g

# --Nationality + Place Group
g <- ggplot(nat, aes(as.factor(nat), time))
g <- g + geom_boxplot(aes(colour=place.cat))
g <- g + theme_minimal()
g <- g + ggtitle('Distribution of Race Times')
g <- g + xlab('Nationality') + ylab('Time (s)')
g <- g + scale_colour_discrete(name = "Place Group")
g

# Density plots
# --Nationality
g <- ggplot(nat, aes(time))
g <- g + geom_density(aes(colour = as.factor(nat)))
g <- g + theme_minimal()
g <- g + ggtitle('Density Distribution of Race Times')
g <- g + xlab('Time (s)') + ylab('Density')
g <- g + scale_colour_discrete(name = "Nationality")
g

# One-way ANOVA between Nationalities
summary(aov(time ~ nat, data = dt))  # Sig. diff in mean time across ages
# Check assumptions:
# 1. Population is normally distributed
# 2. SD of populations are equal
nat[,.(SD_time = sd(time)), by = nat]
t.test(dt[nat=='NED'][['time']], 
       dt[nat=='GER'][['time']], 
       alternative = 'two.sided')  # P-value 0.0007; means are different
var.test(dt[nat=='NED'][['time']], 
         dt[nat=='GER'][['time']], 
         alternative = "two.sided") # P-value 0.092; T-test is VALID
# Null hypothesis is kept: variance between
# nationalities is the same and therefore the T-test is valid.
# 3. Obs. are randomly drawn and independent



