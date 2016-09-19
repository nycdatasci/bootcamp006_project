#Linlin Cheng

library(dplyr)
library(VIM)
library(ggplot2)
library(ggmap)
library(glmnet)
library(randomForest)
library(xgboost)
library(glmnet)
library(caret)
library(e1071)

set.seed(1234)
###########################################################
###########################################################
#dataset link:
#https://www.drivendata.org/competitions/7/page/25/#features_list

#import training dataset:
water_train <- read.csv("~/Desktop/water_well.csv", stringsAsFactors=FALSE)
label <- read.csv("~/Desktop/label.csv", stringsAsFactors=FALSE)
#import test dataset:
water_test <- read.csv("~/Desktop/water_test.csv", stringsAsFactors = FALSE)

#Preprocessing on the traing dataset:
dim(water_train) #[1] 59400    40
dim(label)       #[1] 59400     2
dim(water_test)  #[1] 14850    40

sum(water_train$id!=label$id) #check for direct column bind conditions 
#[1] 0, suggest that the id columns in water_train and label are identical
#thus suitable for concatenation 
water_train$label = as.factor(label$status_group)
dim(water_train) #[1] 59400    41
View(water_train)

sapply(water_train, class)
# id                    amount_tsh 
# "integer"              "numeric" 
# date_recorded          funder 
# "factor"               "factor" 
# gps_height             installer 
# "integer"              "factor" 
# longitude              latitude 
# "numeric"              "numeric" 
# wpt_name               num_private 
# "factor"               "integer" 
# basin                   subvillage 
# "factor"               "factor" 
# region                  region_code 
# "factor"               "integer" 
# district_code          lga 
# "integer"              "factor" 
# ward                    population 
# "factor"               "integer" 
# public_meeting          recorded_by 
# "logical"              "factor" 
# scheme_management       scheme_name 
# "factor"               "factor" 
# permit                 construction_year 
# "logical"              "integer" 
# extraction_type       extraction_type_group 
# "factor"               "factor" 
# extraction_type_class management 
# "factor"              "factor" 
# management_group      payment 
# "factor"              "factor" 
# payment_type          water_quality 
# "factor"               "factor" 
# quality_group         quantity 
# "factor"               "factor" 
# quantity_group        source 
# "factor"              "factor" 
# source_type            source_class 
# "factor"              "factor" 
# waterpoint_type       waterpoint_type_group 
# "factor"              "factor" 
# label 
# "factor" 

#missingness:

aggr(water_train)  #need to be applied before the upcoming few lines
prop_miss(water_train)

water_train[water_train==""]<-0
water_train[is.na(water_train)]<-0


####################
#num_private all NAs
#recorded_by, same entry 
#extraction,identical with extraction_type, extraction_type_group
#quantity_group, identical with quantity
#water_quality, same as quality group
#source_type, identical with source
#waterpoint_type_group, indentical with waterpoint_type
#payment_type, identical with payment
#lga, ward, subvillage, region(may keep), basin (keep) are both indicators for geographical locations, keep basin
#wpt_name, 
#scheme_name, empty strings removed for calculations 
####################


#funder
##date
#date_recorded,  assume not very relevant, focus on construction_year instead
range(water_train$construction_year[water_train$construction_year!=0],
      na.rm= T)
#[1] 1960 2013

##############################
##check for irregular patterns:
#amount_tsh:
sum(water_train$amount_tsh==0) 
#[1] 41639 
sum(water_train$amount_tsh==0 & water_train$label == 'non functional')
#[1] 18885

#change strings back to factors:
water_train[,c(4, 6, 9:17, 19:41)]<-lapply(water_train[,c(4, 6, 9:17, 19:41)], as.factor)
sapply(water_train, levels)

#####################################################
#####################################################
fun<-as.character(water_train$funder)
#1. generate a word cloud based on funders, need to replace UN sub-agencies with UN

#find association with the world bank, UN agencies, government of tanzania, 
#
#
f_gov<-c('danida', 'A/co germany', 'belgian', 'british', 'england', 'german', 'germany',
         'china', 'egypt', 'European Union', 'finland', 'japan', 'france', 'greec',
         'netherlands', 'holland', 'holand', 'nethe', 'nethalan', 'netherla', 'netherlands',
         'iran', 'irish', 'islam','italy', 'U.S.A', 'usa', 'usaid', 'swiss', 'swedish','korea', 'niger'
) #'Jica',
NGO<-c('World Bank', 'Ngo', "Ngos", "Un","Un Habitat", "Un/wfp", "Undp", "Undp/aict", "Undp/ilo", "Unesco",                        
       "Unhcr", "Unhcr/government", "Unice", "Unice/ Cspd", "Unicef", "Unicef/ Csp", "Unicef/african Muslim Agency", 
       "Unicef/central", "Unicef/cspd", "Uniceg", "Unicet", "Unicrf", "Uniseg", "Unp/aict", "wwf", "wfp")
local_commu <- unique(c(agrep('commu', water_train$funder, value=TRUE), #includes commu for community, vill for village
                        agrep('vill', water_train$funder, value=TRUE)))
tanz_gov<- unique(c(agrep('Government of Tanzania', water_train$funder, value=TRUE), #includes commu for community, vill for village
                    agrep('wsdp', water_train$funder, value=TRUE)))               

unique(fun[agrep('wsdp', fun)])

water_train$funder = as.character(water_train$funder)

temp = water_train$funder

for (i in 1:length(NGO)){
  temp = replace(temp, 
                 agrep(NGO[i], temp),
                 'UN_agencies')
}

for (i in 1:length(f_gov)){
  temp = replace(temp, 
                 agrep(f_gov[i], temp),
                 'foreign_gov')
}

for (i in 1:length(local_commu)){
  temp = replace(temp, 
                 agrep(local_commu[i], temp), 
                 "local_community")
}

# for (i in 1:length(church)){
#   temp = replace(temp, 
#                  agrep(church[i], temp), 
#                  "church")
# }

for (i in 1:length(tanz_gov)){
  temp = replace(temp, 
                 agrep(tanz_gov[i], temp), 
                 "Tanzania_Gov")
}


temp = replace(temp, 
               temp != "UN_agencies" & temp != 'foreign_gov' & temp != 'local_community' & temp != 'Tanzania_Gov',
               'other')
# & temp != 'church'


water_train$funder_cat<-temp
table(water_train$label, water_train$funder_cat)
#before word cloud, plot by funder category: using ggplot

#<Graph>
position <- c("local_community", "Tanzania_Gov", "other", "foreign_gov", "UN_agencies")
ggplot(data = water_train, aes(x=funder_cat)) + geom_bar(aes
                                                         (fill = label), position =
                                                           "fill") + scale_x_discrete(limits = position)+
  xlab('')+ylab('')+ggtitle('Well Status by Funder Category')

ggsave(file = "~/Desktop/tanzania/funder_category_plot.png",width = 20, height = 20, units = "cm")

#<graph: extraction class type>
position = c("gravity", "handpump", "other", "submersible",  "motorpump", "rope pump", "wind-powered")
ggplot(data = water_train, aes(x = extraction_type_class)) + geom_bar(aes
                                                                      (fill = label)) + scale_x_discrete(limits = position)+
  xlab('')+ylab('')+ggtitle('Well Status by Extraction Class Type')

#<graph. status by payment type>
position = c("annually", "per bucket", "monthly", "on failure", "other",  "never pay", "unknown")
ggplot(data = water_train, aes(x = payment_type)) + geom_bar(aes
                                                             (fill = label), position = "fill") + scale_x_discrete(limits = position)+
  xlab('')+ylab('')+ggtitle('Well Status by Payment Type')


#<graph>
position = c("annually", "per bucket", "monthly", "on failure", "other",  "never pay", "unknown")
ggplot(data = water_train, aes(x = source_class)) + geom_bar(aes
                                                             (fill = label), position = "fill") + #scale_x_discrete(limits = position)+
  xlab('')+ylab('')+ggtitle('Well Status by Source Class')

####
#<Graph>
df_extract_la <- as.data.frame(table(water_train$extraction_type, water_train$label))
df_extract_cat<-data.frame(extract_type = df_extract_la$Var1[1:18], functional = df_extract_la$Freq[1:18], `functional needs repair` = df_extract_la$Freq[19:36],
                           `non functional` = df_extract_la$Freq[37:54])

df_extract_cat %>% mutate(functional_rate = functional /(functional+functional.needs.repair+non.functional))

#restore to original dataset for modeling:
water_train<-water_train %>% select(-c(funder_cat))

#####################################################
#####################################################
#design matrix manipulation:
x_design = select(as.data.frame(water_train), -c(num_private, recorded_by, id, date_recorded, scheme_name,
                                                 extraction_type_group, extraction_type,  quantity_group, waterpoint_type_group,
                                                 payment_type, source_type, lga, ward, subvillage))
x_design$public_meeting <- as.factor(x_design$public_meeting)

###
##want: edit Unrecorded, impute in numeric if makes senses

x_design[is.na(x_design)] <- 0 #impute all 0s for XGboost run
#impute 0s for factor columns an revert them back to factors:
#######
View(x_design)
train = sample(1:nrow(x_design), 8*nrow(x_design)/10)
x_train = x_design[train, ]
x_test = x_design[-train, ]
tlabel <- as.numeric(as.factor(x_train$label))-1


#####################################################
####################################################
#XGboost
tlabel <- as.numeric(as.factor(x_train$label))-1
x_train$label <-tlabel
xgbtrain <- xgb.DMatrix(data.matrix(select(x_train, -label)), label=tlabel, missing=NA)
xgbtest<- xgb.DMatrix(data.matrix(select(x_test, -label)))


m = xgb.train(params = list(silent = 1), data = xgbtrain, nrounds = 380,
              eta = 0.07, 
              num_class = 3,                                                                # learning rate
              max.depth = 11, 
              gamma = 1, 
              colsample_bytree = 0.6, 
              min_child_weight = 1,
              eval_metric = "merror",
              objective = "multi:softmax")

p_train = predict(m, newdata = xgbtrain) #prediction
p_test = predict(m, newdata = xgbtest)
#tabulate for accuracy:
##confusion matrix for the training dataset:
table(x_train$label, p_train)
##confusion matrix for the test dataset:  
table(x_test$label, p_test)

#accuracy:
train_error <- sum((as.numeric(x_train$label)-1)==p) / sum(table(x_train$label, p))
test_error <- sum((as.numeric(x_test$label)-1)==p_test) / sum(table(x_test$label, p_test))
#set.see(1234)
#train: [1] 0.9149832
#test: [1] 0.8111111


