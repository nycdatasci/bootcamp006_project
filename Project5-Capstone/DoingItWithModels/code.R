setwd("C:/Users/ricky/dropbox/bootcamp/capstone/")
train = read.csv("train.csv")
test  = read.csv("test.csv")

## Check the correlation between SalePrice and GrLivArea
model = lm(train$SalePrice ~ train$GrLivArea)
eq = paste0("SalePrice = ", round(model$coefficients[[2]]), "*PriceSqFt+", round(model$coefficients[[1]]))
plot(train$GrLivArea,train$SalePrice,xlab="GrLivArea",ylab="SalePrice",cex.lab=1.5,cex.main=2,main=eq)
abline(model,col = "red")

## Calculate the sale price per square feet
train$PriceSqFt = train$SalePrice / train$GrLivArea

## Check the ditribution of SalePrice vs PriceSqFt
plot(density(train$SalePrice),xlab = "SalePrice",main = "Distribution of SalePrice",cex.lab=1.5,cex.main=2)
plot(density(train$PriceSqFt), xlab = "PriceSqFt",main = "Distribution of PriceSqFt",cex.lab=1.5,cex.main=2)

## Check the correlation between PriceSqFt, OverallQual and Neighborhood
boxplot(PriceSqFt~OverallQual,data=train, xlab="OverallQual",ylab="PriceSqFt",cex.lab=1.5)
boxplot(PriceSqFt~Neighborhood,data=train,xlab="Neighborhood",ylab="PriceSqFt",cex.lab=1.5)

## Outlier
## Regress the most two important features on PriceSqFt to get the outliers
model = lm(PriceSqFt ~ OverallQual + Neighborhood, data = train)
summary(model)
library(car)
influencePlot(model,col = c("red","black")) # 
outlierTest(model)   # Bonferonni p-value for most extreme obs

## Remove 600,689 and 1182 from training
train1 = train[-c(600,689,1182),]

## Time Series Plot of Median PriceSale by month
library(dplyr)
#df.price = train1 %>% group_by(YrSold,MoSold) %>% summarize(PriceMed = median(PriceSqFt))
df.price = train1 %>% group_by(YrSold,MoSold) %>% summarize(PriceMed = median(SalePrice))
myts <- ts(df.price$PriceMed, start=c(2006, 1), end=c(2010, 7), frequency=12)
## Seasonal Decomposition of Time Series by Loess, Additive, T+S+C+I
#fit.add <- stl(myts, s.window="period")
#plot(fit.add)

## Seasonal Decomposition of Time Series by Loess, Multiplicative, T*S*C*I
myts.log = log(myts)
fit.mult <- stl(myts.log, s.window="period")
plot(fit.mult)

## Time Series of Sale Volumn
count_df = train %>% group_by(YrSold,MoSold) %>% summarize(volume = n())
myts.cnt <- ts(count_df$volume, start=c(2006, 1), end=c(2010, 7), frequency=12)
fit1 <- stl(myts.cnt, s.window="period")
plot(fit1)

## Save Trend and Season index in dataframe
SeasonIdx = exp(fit.mult$time.series[,1])
TrendIdx = exp(fit.mult$time.series[,2])

## Create Time Series Index with interaction of trend and seasonality 
TsIdx = TrendIdx/(max(TrendIdx)) * SeasonIdx

Year = c(rep(2006,12),rep(2007,12),rep(2008,12),rep(2009,12),rep(2010,7))
Month = c(rep(1:12,4),1:7)

df.ts = data.frame(Year,Month,SeasonIdx,TrendIdx,TsIdx)

## Combine training and test, add new features
df.comb = rbind(train[,-c(81,82)],test)
df.comb$HouseAge = df.comb$YrSold - df.comb$YearBuilt    ## age of house when sold
df.comb$RemodAge = df.comb$YrSold - df.comb$YearRemodAdd ## age of remodeling 


## Correct a possible typo in the data set
df.comb[which(df.comb$GarageYrBlt == 2207),]$GarageYrBlt = 2007

df.comb$GarageAge = df.comb$YrSold - df.comb$GarageYrBlt   ## age of garage

## Some NAs have actual meaning

# Create a dummy to represent whether the house has access to street
df.comb$AccStreet = ifelse(is.na(df.comb$LotFrontage),0,1)

# Create a dummy to represent whether the house has access to street
df.comb$AccAlley = ifelse(is.na(df.comb$Alley),0,1)

# Whether the house has a basement(1 or 0)
df.comb$Basement = ifelse(is.na(df.comb$BsmtQual),0,1) 

# Pool(1 or 0)
df.comb$Pool = ifelse(is.na(df.comb$PoolQC),0,1) 

# Fireplace(1 or 0)
df.comb$Fireplace = ifelse(is.na(df.comb$FireplaceQu),0,1)

# Garage (1 or 0)
df.comb$Garage = ifelse(is.na(df.comb$GarageCond),0,1)

# Fence(1 or 0)
df.comb$Fenc = ifelse(is.na(df.comb$Fence),0,1)

# New Home(1 or 0)
df.comb$NewHome = ifelse(df.comb$SaleType == "New",1,0)

# Whether sale condition is normal (1 or 0)
df.comb$SaleCondNorm = ifelse(df.comb$SaleCondition == "Normal",1,0)
  
##############################################################
df.macro = read.csv("macro.csv",header=TRUE)
df.mac = data.frame(cbind(df.ts,df.macro))
df.full = merge(df.comb,df.mac,by.x=c("YrSold","MoSold"),by.y=c("Year","Month"))
df.full = df.full %>% arrange(Id)
#############################################################
df.train = df.full[1:1460,]
df.train = df.train[-c(600,689,1182),]  ## Remove outliers from train set
df.test =  df.full[1461:2919,]
#################################################################
SalePrice = train$SalePrice[-c(600,689,1182)]
r.train = cbind(df.train,SalePrice)
write.csv(r.train,"ryyy_train.csv")
write.csv(df.test,"ryyy_test.csv")
#################################################################


