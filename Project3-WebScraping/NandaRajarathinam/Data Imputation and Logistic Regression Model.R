#Code to build a Logistic Regression model to predict if a Business school is a Top tier school
library(googleVis)
library(ggplot2)
library(Hmisc)
library(car)
library (lubridate) 
library (dplyr) 
library(stringr)
library(reshape2)
library(mice)
library(VIM)
library(corrplot)
library(MASS)
library(ggthemes)
library(tm); 
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

bus_school_full_data = read.csv("C:\\Users\\trichna\\Documents\\NYCDSA\\WebScraping\\full_school_data.csv",fileEncoding='UTF-8',stringsAsFactors = FALSE)

 
 attach(bus_school_full_data)
 bus_school_full_data$Post.MBA.salary = as.numeric(substr( bus_school_full_data$Post.MBA.salary,1,6) )
 #clean up the salary for Carnegie Mellon. Its a typo, the average annual salary should have been 106981
 bus_school_full_data[bus_school_full_data$Post.MBA.salary < 1000, ]$Post.MBA.salary =   bus_school_full_data[bus_school_full_data$Post.MBA.salary < 1000,'Post.MBA.salary' ] * 1000
 
 bus_school_full_data$Geographical.diversity.score = as.numeric(substr(bus_school_full_data$Geographical.diversity.score.out.of.100,1,4) ) 
 bus_school_full_data$Student.rating.of.faculty = as.numeric(substr(bus_school_full_data$Student.rating.of.faculty.out.of.5,1,4) ) 
 bus_school_full_data$Student.rating.of.programme = as.numeric(substr(bus_school_full_data$Student.rating.of.programme.out.of.5,1,4) ) 
 
 
 #Renaming variable names
 names(bus_school_full_data)[names(bus_school_full_data) == 'Student.rating.of.careers.service.out.of.5'] = 'Student.rating.of.careers.service'
 names(bus_school_full_data)[names(bus_school_full_data) == 'Increase.on.pre.MBA.salary..'] = 'Percent.Increase.on.pre.MBA.salary'
 names(bus_school_full_data)[names(bus_school_full_data) == 'Average.number.of.years.work.experience'] = 'Average.work.experience'
 names(bus_school_full_data)[names(bus_school_full_data) == 'Percentage.who.received.a.job.offer.within.three.months.of.graduation'] = 'Percentage.who.received.a.job.offer'
 names(bus_school_full_data)[names(bus_school_full_data) == 'Student.rating.of.culture.and.classmates.out.of.5'] = 'Student.rating.of.cohort' 
 names(bus_school_full_data)[names(bus_school_full_data) == 'Principal.recruiters.of.graduates'] = 'Principal.recruiters'
 names(bus_school_full_data)[names(bus_school_full_data) == 'Percentage.of.graduates.finding.jobs.through.careers.services'] = 'Percent.graduates.finding.jobs.through.careers.services'
 
 #International schools did not have GMAT score, so the scores were imputed
 bus_school_full_data$Average.GMAT =  round(impute(as.numeric(bus_school_full_data$Average.GMAT.score),mean))
 bus_school_full_data$Average.age =  round(impute(as.numeric(bus_school_full_data$Average.age),mean))
 bus_school_full_data$Percent.graduates.finding.jobs.through.careers.services =  round(impute(as.numeric(bus_school_full_data$Percent.graduates.finding.jobs.through.careers.services),mean))
  
 
 
 #Creating new Region feature
 bus_school_full_data$Region = ''
bus_school_full_data[bus_school_full_data$Location %in% c("United States","Canada"),  ]$Region = "North America"  
bus_school_full_data[bus_school_full_data$Location %in% c("France","Spain","United Kingdom","Switzerland","Germany","Italy","Ireland","Netherlands","Monaco"),  ]$Region = "Europe" 
bus_school_full_data[bus_school_full_data$Location %in% c("Japan","Singapore", "Hong Kong", "India") , ]$Region = "Asia Pacific" 
bus_school_full_data[bus_school_full_data$Location == "Australia",  ]$Region = "Australia"   


bus_school_data = bus_school_full_data[ ,c('School.Name','Rank', 'Location', 'Programme.fees', 'Average.GMAT','Average.work.experience', 'Average.age', 'Post.MBA.salary',  'Student.rating.of.faculty', 'Ratio.of.faculty.to.students', 'Student.rating.of.programme', 'Student.rating.of.careers.service', 'Percent.Increase.on.pre.MBA.salary', 'Percentage.who.received.a.job.offer', 'Student.rating.of.cohort', 'Percent.graduates.finding.jobs.through.careers.services','Principal.recruiters', 'Region', 'Geographical.diversity.score')]

## -- not reqd names(bus_school_data)[names(bus_school_data) == 'AAPercent.graduates.finding.jobs.through.careers.services'] = 'Percent.graduates.finding.jobs.through.careers.services' 

bus_school_data$Tier = ''
bus_school_data[bus_school_full_data$Rank >= 1 & bus_school_full_data$Rank <= 25,  ][ ,'Tier'] = "Tier 1"   
bus_school_data[bus_school_full_data$Rank >= 26 & bus_school_full_data$Rank <= 50,  ][ ,'Tier'] = "Tier 2"  
bus_school_data[bus_school_full_data$Rank >= 51 & bus_school_full_data$Rank <= 75,  ][ ,'Tier'] = "Tier 3"  
bus_school_data[bus_school_full_data$Rank >= 76 & bus_school_full_data$Rank <= 100,  ][ ,'Tier'] = "Tier 4"  

#Perform ANOVA test to see if there is a difference in Average GMAT score & Average Post MBA salary
bus_school_data$Average.GMAT = as.integer(bus_school_data$Average.GMAT)
bus_school_data$Average.age = as.integer(bus_school_data$Average.age)
bus_school_data$Percent.graduates.finding.jobs.through.careers.services = as.integer(bus_school_data$Percent.graduates.finding.jobs.through.careers.services)



gmat.bp = ggplot(data = bus_school_data, aes(x = Tier, y = Average.GMAT)) 
gmat.bp + geom_boxplot(aes(fill = Tier)) + theme_economist() + theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Distribution of Average GMAT score\n for various Tiers\n') + ylab("Average GMAT Score\n") + xlab("\nBusiness School Tier")  + theme(axis.text.x = element_text(vjust = 0, angle = 0, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5))
# Tier 1 and Tier 2 schools have a marked difference in the median GMAT scores and a similar behavior is noted 
# in Tier 3 and 4. The difference between Tier 2 and 3 are minimal. Tiers 2 and 4 have outliers


mba.sal.bp = ggplot(data = bus_school_data, aes(x = Tier, y = Post.MBA.salary)) 
mba.sal.bp + geom_boxplot(aes(fill = Tier)) + theme_economist() + theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Distribution of Post MBA Compensation\n for various Tiers\n') + ylab("Annual Compensation\n") + xlab("\nBusiness School Tier")  + theme(axis.text.x = element_text(vjust = 0, angle = 0, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5))
#Outliers in Tier 3 and Tier 4 (lower end) are due to Asian schools (India & Singapore) . Rhough the salary is in USD, it does
#not consider inflation

plot(density(bus_school_data$Post.MBA.salary), main = "Overall Distribution of Post MBA Salary")
# almost appers to be normally distributed
plot(density(bus_school_data$Average.work.experience), main = "Overall Distribution of Average work experience prior to MBA")
abline(v = mean(bus_school_data$Average.work.experience), lwd = 2, lty = 2)
# Right skewed distribution (positively skewed), mean work experience is 5.5 years but we have institutions where the mean 
# work experience is much higher than 6 years

plot(density(bus_school_data$Post.MBA.salaary), main = "Overall Distribution of Average work experience prior to MBA")
avg.work.exp.dp = ggplot(data = bus_school_data, aes(x = Average.work.experience)) 
avg.work.exp.dp + geom_density_2d(stat = 'density') + theme_economist() + theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Overall Distribution of Average work experience prior to MBA') + ylab("Density\n") + xlab("\n Average Work Experience")  + theme(axis.text.x = element_text(vjust = 0, angle = 0, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5))
#

mba.sal.dp = ggplot(data = bus_school_data, aes(x = Post.MBA.salary)) 
mba.sal.dp + geom_density_2d(stat = 'density') + theme_economist() + theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Overall Distribution of Average MBA Compensation\n') + ylab("Density\n") + xlab("\nAverage Annual Compensation")  + theme(axis.text.x = element_text(vjust = 0, angle = 0, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5))
#

summary(aov(bus_school_data$Average.GMAT ~ bus_school_data$Tier))
#                      Df Sum Sq Mean Sq F value   Pr(>F)    
#bus_school_data$Tier  3  57708   19236   31.12 3.87e-14 ***
#  Residuals           96  59346     618             
#The p-value for this test is extremely small (<.0005). Thus, the average GMAT score
# for at least one of the tiers differs from the average GMAT score of
#the others. We reject the null hypothesis that the average GMAT score is the
#same for each of the tiers.   

summary(aov(bus_school_data$Post.MBA.salary ~ bus_school_data$Tier))
#                     Df    Sum Sq   Mean Sq F value   Pr(>F)    
#bus_school_data$Tier  3 2.077e+10 6.924e+09   27.88 4.71e-13 ***
#  Residuals          96 2.384e+10 2.483e+08         

summary(aov(bus_school_data$Student.rating.of.programme ~ bus_school_data$Tier))
#                      Df Sum Sq Mean Sq F value   Pr(>F)    
#bus_school_data$Tier  3  2.272  0.7573   19.41 6.42e-10 ***
#  Residuals            96  3.745  0.0390 

summary(aov(bus_school_data$Student.rating.of.programme ~ bus_school_data$Region))
#                        Df Sum Sq Mean Sq F value Pr(>F)
#bus_school_data$Region  3  0.348 0.11609   1.966  0.124
#Residuals              96  5.668 0.05905  

summary(aov(bus_school_data$Student.rating.of.faculty ~ bus_school_data$Tier))
#4.55e-08 ***

summary(aov(bus_school_data$Student.rating.of.faculty ~ bus_school_data$Region))
#0.00119 **

summary(aov(bus_school_data$Geographical.diversity.score ~ bus_school_data$Region))

summary(aov(bus_school_data$Student.rating.of.cohort ~ bus_school_data$Tier))
#4e-13 ***



#---------------------------------------------------------
bus_school_cor = bus_school_data[ , c(4:16, 19)]
t = cor(bus_school_cor)
df = as.matrix(bus_school_cor)
b = corrplot(t)
corrplot(data.matrix(t), type="upper", order="hclust", tl.col="black", tl.srt=45)
t

#---------------------------------------------------
#F test for Tiers 2 and 3

var.test(bus_school_data$Post.MBA.salary[bus_school_data$Tier == "Tier 2"],
         bus_school_data$Post.MBA.salary[bus_school_data$Tier == "Tier 3"],
         alternative = "two.sided") 
#F test to compare two variances

#data:  bus_school_data$Post.MBA.salary[bus_school_data$Tier == "Tier 2"] and bus_school_data$Post.MBA.salary[bus_school_data$Tier == "Tier 3"]
#F = 0.43291, num df = 24, denom df = 24, p-value = 0.04536
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.1907716 0.9824012
#sample estimates:
#  ratio of variances 
#0.4329137 

#The p-value for this test is 0.046 which slightly < 0.05 
#Thus, our data supports the alternative hypothesis that the variance in the average post MBA salary is different for the Tiers 2 and 3

#------
#US Vs Rest of the World 
US.programme.rating = bus_school_data[bus_school_data$Location == "United States"  ,'Student.rating.of.programme']
Interanational.programme.rating  = bus_school_data[!bus_school_data$Location ==  "United States"  ,'Student.rating.of.programme']

t.test(US.programme.rating, Interanational.programme.rating, alternative = "two.sided")

# p value 0.005 is < 0.05 indicating that null hypothesis can be rejected in favor of alternte gypothesis
# that the true difference in means between US programmes and international business school programmes is not 0


US.diversity.score = bus_school_data[bus_school_data$Location == "United States"  ,'Geographical.diversity.score']
Interanational.diversity.score  = bus_school_data[!bus_school_data$Location ==  "United States"  ,'Geographical.diversity.score']
t.test(US.diversity.score, Interanational.diversity.score, alternative = "two.sided")
# p value 0.005 is < 0.05 indicating that null hypothesis can be rejected in favor of alternte gypothesis
# that the true difference in means between US diversity score and international business schools' diversity score is not 0

#-----------------------------------------------



bus_school_data$Top.Tier = ''
bus_school_data[bus_school_full_data$Rank >= 1 & bus_school_full_data$Rank <= 25,  ][ ,'Top.Tier'] = 1  
bus_school_data[!(bus_school_full_data$Rank >= 1 & bus_school_full_data$Rank <= 25),  ][ ,'Top.Tier'] = 0
bus_school_data$Top.Tier = as.integer(bus_school_data$Top.Tier)

#Create Train and Test datasets
set.seed(0)
n = sample(1:nrow(bus_school_data), 6.8*nrow(bus_school_data)/10)
bus_train_ds = bus_school_data[n, ]
bus_test_ds = bus_school_data[-n, ]

logit.bschool5 = glm(Top.Tier ~    Average.GMAT +  Post.MBA.salary+   Percentage.who.received.a.job.offer + Percent.graduates.finding.jobs.through.careers.services + Student.rating.of.programme+ Student.rating.of.careers.service+ Ratio.of.faculty.to.students ,
                     family = "binomial", data = bus_train_ds, control = list(maxit = 50))

summary(logit.bschool5)

#Visualization of Residuals
scatter.smooth(logit.bschool5$fit,
               residuals(logit.bschool5, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Business School Data")
abline(h = 0, lty = 2)

exp(logit.bschool5$coefficients)

#Coefficient interpretations on the odds scale:
#-Intercept: The odds of a Business school being a Top Tier school
#             when the average GMAT score of the students joining the program is zero
#            when there is no geographical diversity of the students and post MBA salary is zero is approximately 0
#-Average GMAT: For every additional one point increase on the average GMAT score, the odds
#      of that school being a top tier Business school is multiplied by approximately 1.113,
#      holding all other variables constant.
#-Percentage.who.received.a.job.offer: For every additional percentage point increase in the students who got a job offer, the odds of
#      that school being a top tier Business school is multiplied by approximately 0.731, holding
#      all other variables constant.
#-Post MBA Salary: For every additional $1 increase in the average salary of the students, the odds of
#      that school being a top tier Business school is multiplied by approximately 1, holding
#      all other variables constant.
#-Percent of graduates finding jobs through School's career services: For every additional percentage point increase in the students who get a job offer through the School's career services team, the odds of
#      that school being a top tier Business school is multiplied by approximately 1.134, holding
#      all other variables constant.


#Generating confidence intervals for the coefficients on the odds scale.

exp(confint.default(logit.bschool5))
 
#Predict the probability of a business school being a top tier school 
dd = exp(predict(logit.bschool5, bus_test_ds))/(1 + exp(predict(logit.bschool5, bus_test_ds)))

Top.Tier.predicted = predict(logit.bschool5, bus_test_ds, type = "response")

bus_test_ds = cbind(bus_test_ds, "Prob.Top.Tier" = Top.Tier.predicted, "Predict.Top.Tier" = ifelse(Top.Tier.predicted > 0.8, 1, 0))


#Comparing the true values to the predicted values:
Top.Tier.School.predicted = ifelse(logit.bschool5$fitted.values> 0.8, 1, 0)
table(truth = bus_school_data$Top.Tier, prediction = Top.Tier.School.predicted)
table(bus_test_ds$Top.Tier, bus_test_ds$Predict.Top.Tier)

 
#    FALSE TRUE
#0    22    2
#1    1     7

 table(bus_train_ds$Top.Tier, bus_train_ds$Predict.Top.Tier)
 #    FALSE TRUE
 #0   51     0
 #1   7     10
 
#McFadden's R squared
1 - logit.bschool5$deviance/logit.bschool5$null.deviance
#This model explains about 65.52% of the variability in a Business school being classified as top tier 
#based on the predictors in our model

bus_test_ds1 = bus_test_ds[,-Prob.Top.Tier]


bus_train_ds = cbind(bus_train_ds,  "Predict.Top.Tier" = ifelse(logit.bschool5$fitted.values > 0.8, 1, 0))


de = c(Top.Tier.School.predicted, bus_test_ds$Predict.Top.Tier)
