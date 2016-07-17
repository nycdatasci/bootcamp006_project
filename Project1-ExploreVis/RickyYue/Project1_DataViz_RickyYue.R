library(dplyr)   ## load dplyr package for data preprocessing
## read the biomarker data set
df1 <- read.csv("C:/Users/ricky/Dropbox/interviews/gilead/Biomarker Data.csv")
## read the averse effect time data set
df2 <- read.csv("C:/Users/ricky/Dropbox/interviews/gilead/AE times.csv")
## join the two data sets
df3 <- left_join(df1,df2,by=c("SUBJECT"="ID"))
## filter those records to be deleted based on adverse effect data set
df4 <- filter(df3,WEEK>EVENT.TIME)
## generate the final data frame to be used to identify useful markers
df5 <- anti_join(df3,df4,by=names(df3))

## add a new column with base marker.value(marker.value at week 0) to the data frame
library(data.table)                                        ## lode data.table package 
key <- paste(df5$SUBJECT,df5$MARKER.NAME,df5$WEEK,sep="_") ## build a unique key for each record with SUBJECT,MARKER.NAME and WEEK info. 
dt  <- data.table(cbind(df5,key))                          ## combine the key to the data frame
setkey(dt,key)                                             ## set the key as index to each record
new_key <- paste(df5$SUBJECT,df5$MARKER.NAME,0,sep="_")    ## add a new key with SUBJECT and MARKER.NAME info, with WEEK=0.

base <- c()                                ## initiate a new column which has MARKER.VALUE when WEEK=0

##look up the table and get MARKER.VALUE indexed with the new key and return base MARKER.VALUE(WEEK=0) for all records 
for (i in 1:nrow(dt)){
  base[i] <- dt[new_key[i]]$MARKER.VALUE  
}

## check the distribution of base value
summary(base)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00   10.00   22.00   26.03   38.00  103.00

## build the metric FOLD
## since base has zero values, 1 is added to both the numerator and denominator of FOLD=(MARKER.VALUE/BASE), 
## the FOLD is calculated as below:
FOLD <- (df5$MARKER.VALUE+1)/(base+1)

df <- mutate(df5,FOLD)  ## add FOLD to the data frame

## group the data by MARKER.NAME and WEEK, cacluate the mean and sd for MARKER.VALUE, and attach those two values to table 
df_wide<- summarise(group_by(df,MARKER.NAME,WEEK),AVG.MARKER.VALUE=mean(MARKER.VALUE),MARKER.VALUE.SD=sd(MARKER.VALUE),AVG.FOLD=mean(FOLD),FOLD.SD=sd(FOLD))


library("ggplot2") ## load ggplot for data visualization

## draw the "marker.value vs week" line chart
value.chart <- ggplot(data=df_wide, aes(x=WEEK, y=AVG.MARKER.VALUE, group = MARKER.NAME, colour = MARKER.NAME)) +
  geom_line() + 
  geom_point(size=6, shape=21, fill="white") + 
  geom_text(aes(label=ifelse(df_wide$WEEK==7,as.character(df_wide$MARKER.NAME),"")),hjust=2, vjust=0) +
  ggtitle("[MARKER.VALUE vs WEEK]")

value.chart  ## show the chart

## draw the "fold vs week" line chart
fold.chart <- ggplot(data=df_wide, aes(x=WEEK, y=AVG.FOLD, group = MARKER.NAME, colour = MARKER.NAME)) +
  geom_line() + 
  geom_point(size=6, shape=21, fill="white") + 
  geom_text(aes(label=ifelse(df_wide$WEEK==7,as.character(df_wide$MARKER.NAME),"")),hjust=2, vjust=0) +
  ggtitle("[FOLD vs WEEK]")

fold.chart  ## show the chart

## make a short list of markers, where M1,M2,M3,M4 are selected 
df_wide_short <- filter(df_wide,MARKER.NAME %in% c("M1","M2","M3","M4"))

## prepare graphs of "MARKER.VALUE vs WEEK" with error bars
## calculate error bars of MARKER.VALUE
limits.value <- aes(ymax = df_wide_short$AVG.MARKER.VALUE + df_wide_short$MARKER.VALUE.SD, ymin = df_wide_short$AVG.MARKER.VALUE - df_wide_short$MARKER.VALUE.SD,
                   colour = df_wide_short$MARKER.NAME)
## draw the "MARKER.VALUE vs WEEK" graph with error bars
short.list.value <- ggplot(data=df_wide_short, aes(x=WEEK, y=AVG.MARKER.VALUE, group = MARKER.NAME, colour = MARKER.NAME)) +
  geom_line() + 
  geom_point(size=6, shape=21, fill="white") + 
  facet_wrap(~ MARKER.NAME) +
  geom_text(aes(label=ifelse(df_wide_short$WEEK==7,as.character(df_wide_short$MARKER.NAME),"")),hjust=2, vjust=0) +
  geom_errorbar(limits.value, width=0.2)+ 
  ggtitle("[MARKER.VALUE vs WEEK]")

short.list.value

## prepare graphs of "FOLD vs WEEK" with error bars
## calculate error bars of FOLD
limits.fold <- aes(ymax = df_wide_short$AVG.FOLD + df_wide_short$FOLD.SD, ymin = df_wide_short$AVG.FOLD - df_wide_short$FOLD.SD,
                   colour = df_wide_short$MARKER.NAME)

## draw the "FOLD vs WEEK" graph
short.list.fold <- ggplot(data=df_wide_short, aes(x=WEEK, y=AVG.FOLD, group = MARKER.NAME, colour = MARKER.NAME)) +
  geom_line() + 
  geom_point(size=6, shape=21, fill="white") + 
  facet_wrap(~ MARKER.NAME) +
  geom_text(aes(label=ifelse(df_wide_short$WEEK==7,as.character(df_wide_short$MARKER.NAME),"")),hjust=2, vjust=0) +
  geom_errorbar(limits.fold, width=0.2)+ 
  ggtitle("[FOLD vs WEEK]")

short.list.fold

## compare the fold change of the short listed markers between subjects having adverse effect 
## and not having adverse effect
df_ae <- filter(df,SUBJECT%in%c(1:12)) ## get SUBJECT(ID=1:12) which has AE 
df_ne <- filter(df,SUBJECT%in%c(13:39)) ## get SUBHECT(ID=13:39) which has no AE

EFFECT <- c(rep(1,160),rep(0,180))  ## label SUBJECT with regard to AE(EFFECT=1 if AE, EFFEC=0 if no AE)
## get records with AE and compute the mean and std.error of fold
ae <- summarize(group_by(df_ae,WEEK,MARKER.NAME),avg.fd=mean(FOLD),se=sd(FOLD)/sqrt(length(FOLD)))
## get records without AE and compute the mean and std.error of fold
ne <- summarize(group_by(df_ne,WEEK,MARKER.NAME),avg.fd=mean(FOLD),se=sd(FOLD)/sqrt(length(FOLD)))

dff <- rbind(ae,ne) ## bind two tables above
dfff <- cbind(dff,EFFECT)  ## bind the EFFECT label to table
dffff <- filter(dfff,MARKER.NAME%in%c("M1","M2","M3","M4")) ## only choose MARKER.NAME= M1,M2,M3,M4.
## draw the "fold vs week" line chart between the two group (Subject with AE vs Subject without AE) with std. error
short.list <- ggplot(data=dffff, aes(x=WEEK, y=avg.fd, group = EFFECT,color=as.factor(EFFECT))) +
  geom_errorbar(aes(ymin=avg.fd-se, ymax=avg.fd+se), width=.1) +
  geom_line() + 
  geom_point(size=6, shape=21, fill="white") + 
  facet_wrap(~ MARKER.NAME) + 
  xlab("Week") +
  ylab("Fold") +
  ggtitle("Comparing fold change between Subject having AE(1) and not having AE(0)]")

short.list ## show the graph

########################################################################################
###################### predict the event time ##########################################
########################################################################################
col1 <- c(df2$ID,c(13:39))   ## get all ID(SUBJECT) for prediction
col2 <- c(rep(1,12),rep(0,27)) ## label those ID as having adverse effect (label=1) or not (label=0)

## sample WEEK ID for those ID with label=0 based on the distribution of WEEK number in the AE data set(df2)
col3_temp <- c()            
for(i in 1:27){
  col3_temp[i] <- sample(c(4,6,7),1,prob=c(4/12,1/12,7/12))
}
col3 <- c(df2$EVENT.TIME,col3_temp)

df_week <- data.frame(ID=col1,LABEL=col2,L1WEEK=col3-1) ## An intermediate WEEK index table used to find the FOLD value of last week 

## get the FOLD value of M3 from last week
M3.LW<- filter(inner_join(df[,-5],df_week,by=c("SUBJECT"="ID","WEEK"="L1WEEK")),MARKER.NAME=="M3")
M3.LW.FOLD <- M3.LW[order(M3.LW$SUBJECT),]$FOLD
## check distribution of FOLD for M3 in the full data set
hist(df[which(df$MARKER.NAME=="M3"),]$FOLD)
## do the log transformation and check the distribution again
M3.LW.FOLD.LOG <- log(M3.LW[order(M3.LW$SUBJECT),]$FOLD)
hist(log(df[which(df$MARKER.NAME=="M3"),]$FOLD))

## get the FOLD value of M4 from last week
M4.LW<- filter(inner_join(df[,-5],df_week,by=c("SUBJECT"="ID","WEEK"="L1WEEK")),MARKER.NAME=="M4")
M4.LW.FOLD <- M4.LW[order(M3.LW$SUBJECT),]$FOLD
## check distribution of FOLD for M4 in the full data set
hist(df[which(df$MARKER.NAME=="M4"),]$FOLD)
## do the log transformation and check the distribution again
M4.LW.FOLD.LOG <- log(M4.LW[order(M4.LW$SUBJECT),]$FOLD)
hist(log(df[which(df$MARKER.NAME=="M4"),]$FOLD))

## build the table for prediction with LABEL(AE: 1 or 0), 
## M3.FOLD,M4.OLD, LOG(M3.FOLD) and LOG(M4.FOLD) for one week before AE occurs,respectively
df_model <- data.frame(LABEL=as.factor(df_week[,2]),M3.LW.FOLD,M4.LW.FOLD,M3.LW.FOLD.LOG,M4.LW.FOLD.LOG)

## logistic regression model with sqrt transformed fold
glm_model <- glm(df_model[1:24,1]~sqrt(df_model[1:24,2])+sqrt(df_model[1:24,3]),family=binomial(link='logit'))
fitted_result <- predict.glm(glm_model,data.frame(sqrt(df_model[25:39,2]),sqrt(df_model[25:39,3])),type="response")
glm_label <-ifelse(fitted_result>0.5,1,0)
summary(glm_model)

## try more models 
library(e1071)   
## supporting vector machine 
## svm model with fold
svm1_model <- svm(df_model[1:24,2:3], df_model[1:24,1])
svm1_label <- predict(svm1_model, df_model[1:24,2:3])
svm1_test_label <- predict(svm1_model, df_model[25:39,2:3])

## svm model with log transfomred fold 
svm2_model <- svm(df_model[1:24,4:5], df_model[1:24,1])
svm2_label <- predict(svm2_model, df_model[1:24,4:5])
svm2_test_label <- predict(svm2_model, df_model[25:39,2:3])

## naive bayes with log transfomred fold
nb_model <- naiveBayes(df_model[1:24,4:5], df_model[1:24,1])
nb_label <- predict(nb_model,df_model[1:24,4:5])
nb_test_label <- predict(nb_model, df_model[25:39,2:3])


## check area under the ROC (AUC)
library(AUC)
glm_auc  <- auc(roc(glm_label,df_model[1:24,]$LABEL))  
glm_auc
svm1_auc <- auc(roc(svm1_label,df_model[1:24,]$LABEL)) 
svm1_auc
svm2_auc <- auc(roc(svm2_label,df_model[1:24,]$LABEL)) 
svm2_auc
nb_auc   <- auc(roc(nb_label,df_model[1:24,]$LABEL))
nb_auc

## plot the ROC curve
plot(roc(svm1_label,df_model[1:24,]$LABEL),main="SVM model")
plot(roc(svm2_label,df_model[1:24,]$LABEL),main="SVM model with log transformation")
plot(roc(nb_label,df_model[1:24,]$LABEL),main="Naive Nayes model with log transformation")
plot(roc(glm_label,df_model[1:24,]$LABEL),main="Logistic regression model with sqrt transformation")






