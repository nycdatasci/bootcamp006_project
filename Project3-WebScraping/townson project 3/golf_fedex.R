library(stats)
library(VIM)
library(mice)
library(car)
library(Hmisc)
setwd('~/R')

#import and prepare the master data.
full_golf=read.csv('full_golf.csv',strip.white=T,stringsAsFactors=FALSE)
row.names(full_golf)=full_golf$name
full_golf=full_golf[,-which(names(full_golf) %in% c('X','name'))]
golf_fedex = full_golf[is.na(full_golf$rk_fedex)==F,]
summary(golf_fedex)
aggr(golf_fedex) #Lots of missing data, but is it relevant data?  Get a good subset of data first



########################################################################################################
################################Shots Gained v fedex Earned#############################################
########################################################################################################

#Prepare the dataset
sg_data=golf_fedex[,c('rk_fedex','sg_ott','sg_aptg','sg_artg','sg_putt')]
aggr(sg_data)
md.pattern(sg_data) #Missing Predictor Variables... not really imputable, so we won't test these, fair to drop
                    #because this data only recorded for top 200 players, the others will be OUTSIDE of the other data
sg_data_complete=sg_data[which(complete.cases(sg_data)),]
aggr(sg_data_complete)
summary(sg_data_complete) #Data is prepped for testing

#Fit a multiple linear regression
sg.saturated=lm(rk_fedex~.,sg_data_complete)
summary(sg.saturated)
vif(sg.saturated)
avPlots(sg.saturated)
plot(sg.saturated)

#Better Box-Cox transform this
sg_sat.bc=boxCox(sg.saturated)
lambda = sg_sat.bc$x[which(sg_sat.bc$y == max(sg_sat.bc$y))]
sg_data_complete$rk_fedex.bc = (sg_data_complete$rk_fedex^lambda - 1)/lambda
sg_sat_mod.bc=lm(rk_fedex.bc~sg_ott +sg_aptg +sg_artg +sg_putt,sg_data_complete)
summary(sg_sat_mod.bc)
plot(sg_sat_mod.bc)
vif(sg_sat_mod.bc)
avPlots(sg_sat_mod.bc)
#########Seems like a strong relationship!!  Inspect further.

#Check for normality of subsets of variables.  Strokes Gained Off The Tee to start
sg_ott_mod.bc = lm(rk_fedex.bc~sg_ott,sg_data_complete)
summary(sg_ott_mod.bc)
plot(sg_ott_mod.bc) #Actually looks pretty strong on the OTT variable

#Putting?
sg_putt_mod.bc = lm(rk_fedex.bc~sg_putt,sg_data_complete)
summary(sg_putt_mod.bc)
plot(sg_putt_mod.bc)

#Approaching The Green
sg_aptg_mod.bc = lm(rk_fedex.bc~sg_aptg,sg_data_complete)
summary(sg_aptg_mod.bc)
plot(sg_aptg_mod.bc)

#Around The Green
sg_artg_mod.bc = lm(rk_fedex.bc~sg_artg,sg_data_complete)
summary(sg_artg_mod.bc)
plot(sg_artg_mod.bc)

#Check Collinearity
cor(sg_data_complete[,c('sg_artg','sg_putt','sg_ott','sg_aptg')])
#Low corellations amongst variables, supported already by the VIF
#Looks like multiple linear regression model may hold.
#But, let's see if we need all of these variables...

#prepare less fit models
model.empty=lm(rk_fedex.bc~1,sg_data_complete)
model.full=lm(rk_fedex.bc~.-rk_fedex,sg_data_complete)
scope=list(lower = formula(model.empty), upper = formula(model.full))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(196))
backwardBIC = step(model.full, scope, direction = "backward", k = log(196))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(196))
bothBIC.full = step(model.full, scope, direction = "both", k = log(196))

#Looks like combination of the four stats will work fine, but for this Steven Bowditch

#LET'S MAKE OUR OWN MODEL

########################################################################################################
##############################Other Variables v fedex Earned############################################
########################################################################################################

#prepare a data set
raw_data=golf_fedex[,c('rk_fedex','drd','dra','gir','ssv','scr','pth','pthatg','pmd','ppr')]
aggr(raw_data) #Again, the lower-ranked golfers, so hard to impute, since they all fall outside of the data ranges.
raw_data=raw_data[which(complete.cases(raw_data)),]
aggr(raw_data)

#fit multiple linear model on all data
td_mod = lm(rk_fedex~.,raw_data)
summary(td_mod)
td_mod_summary = summary(td_mod)
avPlots(td_mod)
plot(td_mod)  #Definitely some relationship, but it's not really "linear", maybe we can make it so with box-cox transform

##########################MAKE MY OWN REDUCED MODEL################################
td_mod_red = lm(rk_fedex~drd+gir+pthatg+ppr+pmd,raw_data)
summary(td_mod_red)
plot(td_mod_red)
vif(td_mod_red)

#Box Cox transform
td_mod.bc=boxCox(td_mod)
lambda = td_mod.bc$x[which(td_mod.bc$y == max(td_mod.bc$y))]
raw_data$rk_fedex.bc = (raw_data$rk_fedex^lambda - 1)/lambda
td_mod.bc=lm(rk_fedex.bc~.-rk_fedex,raw_data)
summary(td_mod.bc)
plot(td_mod.bc) #Looks like a reasonable model, let's try a reduced model based on significant variables

#Test Box Cox transformed dependent variable on the reduced variable set
td_mod_red.bc=lm(rk_fedex.bc~drd+pmd+gir+pthatg+ppr,raw_data)
summary(td_mod_red.bc)
plot(td_mod_red.bc)
vif(td_mod_red.bc)
avPlots(td_mod_red.bc)
#Looks like a decent model, but let's see if there's a "best model"

#Check full vs reduced
AIC(td_mod_red.bc,td_mod.bc)
BIC(td_mod_red.bc,td_mod.bc)
#AIC and BIC don't show a clear advantage

#Alternatively, let's do a stepwise regression and see what set of variables is identified
model.empty=lm(rk_fedex.bc~1,raw_data)
model.full=lm(rk_fedex.bc~.-rk_fedex,raw_data)
scope=list(lower = formula(model.empty), upper = formula(model.full))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(196))
backwardBIC = step(model.full, scope, direction = "backward", k = log(196))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(196))
bothBIC.full = step(model.full, scope, direction = "both", k = log(196))

#BIC identifies this subset, similar to the SG model statistics
ad_mod_red2.bc=lm(rk_fedex.bc~drd+gir+pthatg+ppr,raw_data)
summary(ad_mod_red2.bc)
#Looks like resonably good model, but check assumptions
plot(ad_mod_red2.bc)
vif(ad_mod_red2.bc)
avPlots(ad_mod_red2.bc)

#Looking Good!  Check Individual variable assumptions
plot(lm(rk_fedex.bc~drd,raw_data))
plot(lm(rk_fedex.bc~gir,raw_data))
plot(lm(rk_fedex.bc~pthatg,raw_data))
plot(lm(rk_fedex.bc~ppr,raw_data))

###Which Model is best?
AIC(ad_mod_red2.bc,sg_sat_mod.bc)
BIC(ad_mod_red2.bc,sg_sat_mod.bc)


#Let's Test some made-up Data!!
Tiger_Trad02=data.frame(drd=293.3,gir=73.96,pthatg=95,ppr=29.41)
Tiger_Success_Trad02 = predict(ad_mod_red2.bc,Tiger_Trad02,interval='confidence')
raw_data$fitted.values = ad_mod_red2.bc$fitted.values

Tiger_Trad04=data.frame(drd=301.9,gir=66.9,pthatg=80,ppr=28.44)
Tiger_Success_Trad04 = predict(ad_mod_red2.bc,Tiger_Trad04,interval='confidence')

Tiger_SG04=data.frame(sg_ott=0.321,sg_aptg=0.873,sg_artg=0.335,sg_putt=0.853)
Tiger_Success_SG04 = predict(sg_sat_mod.bc,Tiger_SG04,interval='confidence')
sg_data_complete$fitted.values = sg_sat_mod.bc$fitted.values

Ben_Trad=data.frame(drd=280,gir=45,pthatg=120,ppr=36)
Ben_Success=predict(ad_mod_red2.bc,Ben_Trad,interval='confidence')