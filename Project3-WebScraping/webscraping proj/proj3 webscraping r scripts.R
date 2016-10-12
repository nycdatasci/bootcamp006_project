indeed1=left_join(indeed_salaries, indeed_review, by = 'comp_name')

getwd()
setwd("C:/Users/Le Wei/Desktop/")
write.csv(indeed1,file="indeed1.csv",row.names = F)
indeed2=read.csv('indeed1.csv')
#res = ifelse(indeed2$comp_rating_overall > 4.0 & < 5.0, '4.0~5.0', ifelse(indeed2
#> 3.0 & < 4.0, '3.0~4.0', '2.0~3.0'))
indeed3=read.csv('indeed3.csv')

library(dplyr)




g <- ggplot(data = indeed3, aes(x = sal_yr))
g + geom_histogram()
g + geom_histogram() + facet_wrap( ~ rating_class)
a=ggplot(data = indeed3, aes(x = comp_rating_overall, y = sal_yr)) + geom_point()
modelindeed3 = lm(sal_yr ~ comp_rating_overall, data = indeed3)
summary(modelindeed3)
plot(modelindeed3)


View(indeed)
plot(indeed)
library(corrplot)
M <- cor(indeed)
corrplot(M, method="circle")
model.saturate1 = lm(overall_rating ~ ., data = indeed)
plot(model.saturate1)
#we found observation 190 is leverage point, so we look back at our data 
#and find out what's going on.
indeed1=indeed[-190, ]
model.saturate2 = lm(overall_rating ~ ., data = indeed1)#a new model without observation190
plot(model.saturate2)
summary(model.saturate2)
#the benefit_rating, culture_rating, jsecuretyrating and wl_bal_rating is significant
#the mgmt_rating is not.
library(car) 
influencePlot(model.saturate2)
vif(model.saturate2)
avPlots(model.saturate2)
#after we run avplot, we find the mgmt seems not contribute to y, so we run
#another two test, 1 test is set y = overall ratings, x =mgmt_ratings, to see 
#r^2, and test 2 is set y=overallratings, x= benefit ratings, culture_rating
#and wl_bal_rating to see the R^2, if r^2 of mgmt_rating > r^2 of another test with
#three xs, we choose to keep mgmt_ratings.(remember we need to check normality for each of the test) so sometimes even though the mgmt_rating
#is not significant, which means that mgmt_ratings's effect is not xianzhu, we may wanna 
#check this variable, if no problem or like above happen, we can keep this 
#even though it shows insignificant.


model3=lm(overall_rating~. -mgmt_rating, data=indeed1)
plot(model3)
summary(model3)
avPlots(model3)

model1=lm(overall_rating~. -benefit_rating -culture_rating -wl_bal_rating, data=indeed1)
plot(model1)
summary(model1)


 model.empty1 = lm(overall_rating ~ 1, data = indeed) #The model with an intercept ONLY.
model.full1 = lm(overall_rating ~ ., data = indeed) #The model with ALL variables.
model3=lm(overall_rating~. -mgmt_rating, data=indeed1)

AIC(model.saturate2,    #Model with all variables.
    model3,        #Model with all variables EXCEPT Illiteracy.
    model1) #

