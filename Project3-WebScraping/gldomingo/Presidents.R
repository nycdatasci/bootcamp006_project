presidents = read.csv('Reducedcsv.csv')
pres = presidents

library(car)
library(xtable)

summary(pres)
str(pres)

### party (0 = Democrat, 1 = Republican)
### partybefore (0 = Dem, 1= Rep) This is party of prior administration
### noofrepin4 is number of republican administrations in the
###     preceding 4 administrations
### reelection (0 = new, 1 = reelected)

pres$party = as.factor(pres$party)
pres$partybefore = as.factor(pres$partybefore)
pres$reelection = as.factor(pres$reelection)

sapply(pres,class)

addmargins(xtabs(~party,data=pres))
# Both parties basically even
# party
# 0   1 Sum 
# 16  17  33 

addmargins(xtabs(~party+partybefore,data=pres))
addmargins(prop.table(xtabs(~party+partybefore,data=pres)))
# Republicans have a difficult time at winning when a democrat
#     is incumbent but have better than average chance if 
#     Republican incumbent. Democrats its same regardless of
#     incumbent.

#        partybefore
# party    0         1       Sum
# 0     0.2424242 0.2424242 0.4848485
# 1     0.2121212 0.3030303 0.5151515
# Sum   0.4545455 0.5454545 1.0000000
addmargins(xtabs(~party+noofrepin4,data=pres))
addmargins(prop.table(xtabs(~party+noofrepin4,data=pres)))
addmargins(xtabs(~party+reelection,data=pres))
addmargins(prop.table(xtabs(~party+reelection,data=pres)))

plot(pres$party,pres$partybefore,col='blue')
plot(pres$party,pres$noofrepin4,col = pres$noofrepin4+2)
plot(pres$party,pres$reelection,col='green')

hist(pres$noofrepin4,prob=T)
lines(density(EDA$lcavol),col="Red")

### RUN LOG REGRESSION
logit.presall = glm(party ~ ., family = "binomial",
                         data = pres)

logit.preslessreel = glm(party ~ .-reelection, family = "binomial",
                    data = pres)

# Mc Faddens Pseudo R2 For Saturated Model
1 - logit.presall$deviance/logit.presall$null.deviance
# [1] 0.04845773

# Mc Faddens Pseudo R2 For Saturated Model less Reelection
1 - logit.preslessreel$deviance/logit.preslessreel$null.deviance
# [1] 0.04235546

# Check for outliers. Possible #17 but upon checking ok.
influencePlot(logit.presall)

summary(logit.presall)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5126  -1.0183   0.7401   1.1320   1.4250  

# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept)    1.1549     1.1024   1.048    0.295
# partybefore1   0.6732     0.7804   0.863    0.388
# noofrepin4    -0.5734     0.4470  -1.283    0.200
# reelection1   -0.3944     0.7486  -0.527    0.598
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 45.717  on 32  degrees of freedom
# Residual deviance: 43.502  on 29  degrees of freedom
# AIC: 51.502

summary(logit.preslessreel)
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.5942  -1.1062   0.7594   1.1839   1.4968  
# 
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)
# (Intercept)    0.9414     1.0187   0.924    0.355
# partybefore1   0.7102     0.7736   0.918    0.359
# noofrepin4    -0.5556     0.4445  -1.250    0.211
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 45.717  on 32  degrees of freedom
# Residual deviance: 43.781  on 30  degrees of freedom
# AIC: 49.781



cbind('Log Odds' = logit.presall$coefficients,
      'Odds' = exp(logit.presall$coefficients))

#               Log Odds      Odds
# (Intercept)   1.1548784 3.1736375
# partybefore1  0.6732431 1.9605854
# noofrepin4   -0.5734475 0.5635791
# reelection1  -0.3943839 0.6740952

# log odds confidence intervals 
confint(logit.presall)

# odds confidence intervals 
exp(confint(logit.presall))

### Compare Full versus Reduced Model
reduced.deviance = logit.preslessreel$deviance
reduced.df = logit.preslessreel$df.residual
full.deviance = logit.presall$deviance
full.df = logit.presall$df.residual

anova(logit.preslessreel, logit.presall, test = "Chisq")
# Conclude that reduced model is OK based on Chi-Squared test

# Model 1: party ~ (partybefore + noofrepin4 + reelection) - reelection
# Model 2: party ~ partybefore + noofrepin4 + reelection
# Resid. Df Resid. Dev Df   Deviance   Pr(>Chi)
# 1        30      43.781                     
# 2        29      43.502  1  0.27898   0.5974

logit.presless2a = glm(party ~ partybefore, family = "binomial",
                         data = pres)

logit.presless2b = glm(party ~noofrepin4 , family = "binomial",
                       data = pres)

reduced2a.deviance = logit.presless2a$deviance
reduced2a.df = logit.presless2a$df.residual

reduced2b.deviance = logit.presless2b$deviance
reduced2b.df = logit.presless2b$df.residual

### Compare model with just partybefore to reduced 2 variable model
anova(logit.presless2a, logit.preslessreel, test = "Chisq")

# Model 1: party ~ partybefore
# Model 2: party ~ (partybefore + noofrepin4 + reelection) - reelection
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1        31     45.458                     
# 2        30     43.781  1   1.6773   0.1953

### Compare model with just noofrepin4 to reduced 2 variable model
anova(logit.presless2b, logit.preslessreel, test = "Chisq")

# Model 1: party ~ noofrepin4
# Model 2: party ~ (partybefore + noofrepin4 + reelection) - reelection
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1        31     44.645                     
# 2        30     43.781  1  0.86444   0.3525

# PREDICT USING CURRENT CONDITIONS
newdata = with(pres, data.frame(partybefore = '0',
          noofrepin4 = 2,reelection='0'))
predict(logit.preslessreel, newdata, type = "response")

# CHANCES OF REPUBLICAN PARTY WINNING IS 46%
# 1 
# 0.4576469 
