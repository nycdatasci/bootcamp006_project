df = read.table("/Users/sharanduggal/Documents/Datascience/Projects/WebScrapingProjects/Insider/Insider_Text.txt", sep = "\t", header = TRUE)
colnames(df) = df[1 ,]
df = df [-1,]
names(df)


View(df)

############### Insider Analaysis ###############

library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)
library(data.table)
str(dfi)
summary(dfi)
View(dfi)

dfi = dfi %>% mutate(Day0 = Close1 - Price)
dfi = dfi %>% mutate(Day1 = Close2 - Close1)
dfi = dfi %>% mutate(Day2 = Close3 - Close1)
dfi = dfi %>% mutate(Day3 = Close4 - Close1)
dfi = dfi %>% mutate(Day4 = Close5 - Close1)
dfi = dfi %>% mutate(Day5 = Close6 - Close1)


detach(dfi)

dfi = dfi %>% mutate(Day0.P = ((Close1 - Price)/Price)*100)

dfi = dfi %>% mutate(Day1.P = ((Close2 - Close1)/Close1)*100)
dfi = dfi %>% mutate(Day2.P = ((Close3 - Close1)/Close1)*100)
dfi = dfi %>% mutate(Day3.P = ((Close4 - Close1)/Close1)*100)
dfi = dfi %>% mutate(Day4.P = ((Close5 - Close1)/Close1)*100)
dfi = dfi %>% mutate(Day5.P = ((Close6 - Close1)/Close1)*100)
dfi[ , 41:52] = round(dfi[ , 41:52],2)

dfi$month = substr(dfi$Date,6,7)

dfi = dfi %>% mutate(Day1.V = ((Volume2 - Volume1)/Volume1)*100)
dfi = dfi %>% mutate(Day2.V = ((Volume3 - Volume1)/Volume1)*100)
dfi = dfi %>% mutate(Day3.V = ((Volume4 - Volume1)/Volume1)*100)
dfi = dfi %>% mutate(Day4.V = ((Volume5 - Volume1)/Volume1)*100)
dfi = dfi %>% mutate(Day5.V = ((Volume6 - Volume1)/Volume1)*100)
dfi[ , 54:58] = round(dfi[ , 54:58], 2)

dfi[ , 54][is.infinite(dfi [ , 54])]  = NA
dfi[ , 54][is.nan(dfi [ , 54])]  = NA
dfi[ , 55][is.infinite(dfi [ , 55])]  = NA
dfi[ , 55][is.nan(dfi [ , 55])]  = NA
dfi[ , 56][is.infinite(dfi [ , 56])]  = NA
dfi[ , 56][is.nan(dfi [ , 56])]  = NA
dfi[ , 57][is.infinite(dfi [ , 57])]  = NA
dfi[ , 57][is.nan(dfi [ , 57])]  = NA
dfi[ , 58][is.infinite(dfi [ , 58])]  = NA
dfi[ , 58][is.nan(dfi [ , 58])]  = NA

dfi$Valu = dfi$Price*dfi$Share

str(dfi)

summary(dfi$Day1.V)

View(dfi)
remove.cols = c("Symbol", "Date", "LastSale", "Parts", "Groups", "Shares", "SharePrice", "Value", "Remaining")
dfi = dfi[ , !names(dfi) %in% remove.cols]


####################################################################################################################################################################################
############################################################  END OF VARIABLE CREATION AND DATA CLEANING ################################ 
####################################################################################################################################################################################
?substr
str(dfi)
#OVERALL DENSITY CURVES

par(mfrow = c(2,2))

a = ggplot(dfi, aes(Day1.P, color=Side)) + geom_density() + xlab("Percentage Change Vs. Day 1") + 
  scale_color_brewer(palette = "Set3", direction = 1) + theme_dark() + ggtitle("Price change one day after transaction")

b = ggplot(dfi, aes(Day2.P, color=Side)) + geom_density() + xlab("Percentage Change Vs. Day 1") + 
  scale_color_brewer(palette = "Set3", direction = 1) + theme_dark() + ggtitle("Price change two days after transaction")

c = ggplot(dfi, aes(Day3.P, color=Side)) + geom_density() + xlab("Percentage Change Vs. Day 1") + 
  scale_color_brewer(palette = "Set3", direction = 1) + theme_dark() + ggtitle("Price change three days after transaction")

d = ggplot(dfi, aes(Day4.P, color=Side)) + geom_density() + xlab("Percentage Change Vs. Day 1") + 
  scale_color_brewer(palette = "Set3", direction = 1) + theme_dark() + ggtitle("Price change four days after transaction")

e = ggplot(dfi, aes(Day5.P, color=Side)) + geom_density() + xlab("Percentage Change Vs. Day 1") + 
  scale_color_brewer(palette = "Set3", direction = 1) + theme_dark() + ggtitle("Price change five days after transaction")

grid.arrange(a,b,c,d,e)

#ON AVERAGE, WHAT PERCENTAGE WOULD YOU EARN ON DAY1,2,3,4,5. ON BUY SIDE. ON SELL SIDE.
#ON AVERAGE, WHAT PERCENTAGE WOULD YOU EARN ON DAY1,2,3,4,5. ON BUY SIDE. ON SELL SIDE.
#ON AVERAGE, WHAT PERCENTAGE WOULD YOU EARN ON DAY1,2,3,4,5. ON BUY SIDE. ON SELL SIDE.
#ON AVERAGE, WHAT PERCENTAGE WOULD YOU EARN ON DAY1,2,3,4,5. ON BUY SIDE. ON SELL SIDE.
#### All stocks
overall.percent.data = dfi %>% group_by(Side) %>% summarise(Day1.Avg= mean(Day1.P, na.rm = TRUE), Day2.Avg= mean(Day2.P, na.rm = TRUE), 
                                     Day3.Avg= mean(Day3.P, na.rm = TRUE), Day4.Avg= mean(Day4.P, na.rm = TRUE), 
                                     Day5.Avg= mean(Day5.P, na.rm = TRUE))
  
price.change = melt(overall.percent.data, id.vars = "Side") %>% ggplot(aes(x = variable, y = value, fill = Side)) + 
  geom_bar(stat = "identity", position = "identity") + coord_flip() + ggtitle("% Price Change Post Trans. -- All Stocks") + 
  scale_fill_brewer(palette = "Set3") + theme_dark() + scale_x_discrete(labels = c("Day 1", "Day 2", "Day 3", "Day 4", "Day5")) +
  xlab("") + ylab("Percentage Change") + geom_text(data = subset(melt(overall.percent.data, id.vars = "Side"), (value>0 | value < 0.06)), aes(x = variable, y = value-0.1, label = paste0(round(value,2),"%")))  + 
  theme(plot.title = element_text(size = 24, face="bold"))  + ylim(-1.15, 3.5)


#### Stocks under 5
overall.percent.data = dfi %>% group_by(Side) %>% filter(Close1 < 5) %>%   
  summarise(Day1.Avg= mean(Day1.P, na.rm = TRUE), Day2.Avg= mean(Day2.P, na.rm = TRUE), 
                                                            Day3.Avg= mean(Day3.P, na.rm = TRUE), Day4.Avg= mean(Day4.P, na.rm = TRUE), 
                                                            Day5.Avg= mean(Day5.P, na.rm = TRUE))

price.less.5 = melt(overall.percent.data, id.vars = "Side") %>% ggplot(aes(x = variable, y = value, fill = Side)) + 
  geom_bar(stat = "identity", position = "identity") + coord_flip() + ggtitle("% Price Change Post Trans. -- Stocks < $5") + 
  scale_fill_brewer(palette = "Set3") + theme_dark() + scale_x_discrete(labels = c("Day 1", "Day 2", "Day 3", "Day 4", "Day5")) +
  xlab("") + ylab("Percentage Change") + geom_text(aes(x = variable, y = value+0.2, label = paste0(round(value,2),"%"))) + 
  theme(plot.title = element_text(size = 24, face="bold")) + ylim(-1.15, 3.5)


#### Stocks over 5
overall.percent.data = dfi %>% group_by(Side) %>% filter(Close1 > 5) %>%   
  summarise(Day1.Avg= mean(Day1.P, na.rm = TRUE), Day2.Avg= mean(Day2.P, na.rm = TRUE), 
            Day3.Avg= mean(Day3.P, na.rm = TRUE), Day4.Avg= mean(Day4.P, na.rm = TRUE), 
            Day5.Avg= mean(Day5.P, na.rm = TRUE))

price.more.5 = melt(overall.percent.data, id.vars = "Side") %>% ggplot(aes(x = variable, y = value, fill = Side)) + 
  geom_bar(stat = "identity", position = "identity") + coord_flip() + ggtitle("% Price Change Post Trans. -- Stocks > $5") + 
  scale_fill_brewer(palette = "Set3") + theme_dark() + scale_x_discrete(labels = c("Day 1", "Day 2", "Day 3", "Day 4", "Day5")) +
  xlab("") + ylab("Percentage Change") + geom_text(data = subset(melt(overall.percent.data, id.vars = "Side"), value>0.02), aes(x = variable, y = value+0.2, label = paste0(round(value,2),"%"), check_overlap = TRUE)) + 
  theme(plot.title = element_text(size = 24, face="bold")) + ylim(-1.15, 3.5)

grid.arrange(price.change, price.less.5, price.more.5, ncol = 3)


#ON AVERAGE, HOW DOES VOLUME CHANGE OVER THE 5 DAYS. 
#ON AVERAGE, HOW DOES VOLUME CHANGE OVER THE 5 DAYS. 
#ON AVERAGE, HOW DOES VOLUME CHANGE OVER THE 5 DAYS. 
#ON AVERAGE, HOW DOES VOLUME CHANGE OVER THE 5 DAYS. 

overall.volume = dfi %>% group_by(Side) %>% 
  summarise(Day1.Avg= mean(Day1.V, na.rm = TRUE), Day2.Avg= mean(Day2.V, na.rm = TRUE), 
            Day3.Avg= mean(Day3.V, na.rm = TRUE), Day4.Avg= mean(Day4.V, na.rm = TRUE), 
            Day5.Avg= mean(Day5.V, na.rm = TRUE))

volume.melted = melt(overall.volume, id.vars = "Side") %>% ggplot(aes(x = variable, y = value, fill = Side)) + 
  geom_bar(stat = "identity", position = "dodge") + ggtitle("% Change in Volume Post Transaction -- All Stocks") + 
  scale_fill_brewer(palette = "Set2") + theme_dark() + scale_x_discrete(labels = c("Day 1 Vs. Transaction Day", "Day 2 Vs. Transaction Day", "Day 3 Vs. Transaction Day", "Day 4 Vs. Transaction Day", "Day5 Vs. Transaction Day")) +
  xlab("") + ylab("Percentage Change") +ylim (0,70) + theme(plot.title = element_text(size = 24, face="bold"))# + ylim(-1.15, 3.5)

volume.melted


overall.volume = dfi %>% group_by(Side) %>% filter(Close1 < 5) %>%  
  summarise(Day1.Avg= mean(Day1.V, na.rm = TRUE), Day2.Avg= mean(Day2.V, na.rm = TRUE), 
            Day3.Avg= mean(Day3.V, na.rm = TRUE), Day4.Avg= mean(Day4.V, na.rm = TRUE), 
            Day5.Avg= mean(Day5.V, na.rm = TRUE))

volume.under.5 = melt(overall.volume, id.vars = "Side") %>% ggplot(aes(x = variable, y = value, fill = Side)) + 
  geom_bar(stat = "identity", position = "dodge") + ggtitle("% Change in Volume Post Transaction -- Stocks < $5") +
  scale_fill_brewer(palette = "Set2") + theme_dark() + scale_x_discrete(labels = c("Day 1 Vs. Transaction Day", "Day 2 Vs. Transaction Day", "Day 3 Vs. Transaction Day", "Day 4 Vs. Transaction Day", "Day5 Vs. Transaction Day")) +
  xlab("") + ylab("Percentage Change") + theme(plot.title = element_text(size = 24, face="bold"))

volume.under.5

overall.volume = dfi %>% group_by(Side) %>% filter(Close1 > 5) %>%  
  summarise(Day1.Avg= mean(Day1.V, na.rm = TRUE), Day2.Avg= mean(Day2.V, na.rm = TRUE), 
            Day3.Avg= mean(Day3.V, na.rm = TRUE), Day4.Avg= mean(Day4.V, na.rm = TRUE), 
            Day5.Avg= mean(Day5.V, na.rm = TRUE))

volume.over.5 = melt(overall.volume, id.vars = "Side") %>% ggplot(aes(x = variable, y = value, fill = Side)) + 
  geom_bar(stat = "identity", position = "dodge") + ggtitle("% Change in Volume Post Transaction -- Stocks > $5") +
  scale_fill_brewer(palette = "Set2") + theme_dark() + scale_x_discrete(labels = c("Day 1 Vs. Transaction Day", "Day 2 Vs. Transaction Day", "Day 3 Vs. Transaction Day", "Day 4 Vs. Transaction Day", "Day5 Vs. Transaction Day")) +
  xlab("") + ylab("Percentage Change") +ylim (0,70) + theme(plot.title = element_text(size = 24, face="bold"))# + ylim(-1.15, 3.5)

volume.over.5

grid.arrange(volume.melted, volume.under.5, volume.over.5, nrow = 3)


#### OVERALL RELATONSHIP BETWEEN VALUE OF SHARES AND ABSOLUTE PRICE CHANGE.

dfi %>% filter(Valu < 10000000) %>% ggplot(aes(x = Valu, y = Day5, color = Side)) + geom_point() + facet_grid(Side ~ .) + 
  scale_color_brewer() + theme_dark() + ggtitle("Price Change 5 days after transaction Vs. Value of Transaction") +
  xlab("Value of Transaction (# of Shares X Price Purchased/Sold)") + ylab("Absoltue Price Change: Day 5 Minus Transaction Day") +
  theme(plot.title = element_text(size = 24, face="bold"))

dfi %>% filter(Valu < 10000000) %>% ggplot(aes(x = Valu, y = Day5.P, color = Side)) + geom_point() + facet_grid(Side ~ .) +
  scale_color_brewer() + theme_dark() + ggtitle("Percentage Price Change 5 days after transaction Vs. Value of Transaction") +
  xlab("Value of Transaction (# of Shares X Price Purchased/Sold)") + ylab("Percentage Price Change: Day 5 Minus Transaction Day") +
  theme(plot.title = element_text(size = 24, face="bold"))


### Price VS. ABSOLUTE PRICE CHANGE
dfi %>% filter(Price < 1000) %>% ggplot(aes(x = Price, y = Day5, color = Side)) + geom_point() + facet_grid(Side ~ .) + 
  scale_color_brewer() + theme_dark() + ggtitle("Price Change 5 days after transaction Vs. Value of Transaction") +
  xlab("Value of Transaction (# of Shares X Price Purchased/Sold)") + ylab("Absoltue Price Change: Day 5 Minus Transaction Day") +
  theme(plot.title = element_text(size = 24, face="bold"))



#WHAT IS THE DESCRIPTIVE RELATIONSHIP BETWEEN MARKET CAP, SECTOR, INDUSTRY, STOCK PRICE RANGE AND CHANGE IN STOCK PRICE
#########################################################################################################################################
################################################SEASONALITY IN NUMBER OF INSIDER TRANSACTIONS###########################################
#########################################################################################################################################
names(dfi)
dfi$Date2 = as.Date(dfi$Date1)

time.transactions = dfi %>% group_by(Side, month) %>% filter(Date2 > "2015-07-31" & Date2 < "2016-08-01") %>% summarise(trans = n())
View(time.transactions)

ggplot(time.transactions, aes(x = month, y = trans)) + geom_line(aes(group = Side, color = Side)) + 
  scale_x_discrete(limits = c("08", "09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07"), labels = 
                     c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "March", "April", "May", "Jun", "Jul")) + theme_dark() +
  scale_color_brewer() + ggtitle("Number of Transactions Per Month -- Past Year") + ylab("# of Transactions") + 
  xlab("Month") + theme(plot.title = element_text(size = 24, face="bold"))

###### Profit and Loss by month ######

dfi.red = subset(dfi, select = c(Side, month, Day5.P))
dfi.red = na.omit(dfi.red)
summary(dfi.red)

profit.by.month = dfi.red %>% group_by(Side, month) %>% summarise(Monthly.Performance = mean(Day5.P))

profit.by.month %>% ggplot(aes(x = month, y = Monthly.Performance, fill = Side)) + 
  geom_bar(stat = "identity", position= "dodge") + theme_dark() + ggtitle("Percentage Price Change 5 Days Post Transaction -- by Month") +
  theme(plot.title = element_text(size = 24, face="bold")) + ylab("Monthly Performance -- Percentage") + xlab("Month") + 
  scale_fill_brewer(palette = "Set3") + scale_x_discrete(limits = c("08", "09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07"), labels = 
                     c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "March", "April", "May", "Jun", "Jul"))
  

SPY = getSymbols("SPY",src="yahoo", from = "2015-08-01", auto.assign = FALSE)
candleChart(SPY)


#########################################################################################################################################
################################################PEFROMANCE BY SECTOR ###########################################
#########################################################################################################################################

# dfi.red = subset(dfi, select = c(Side, Sector, Day5.P))
# dfi.red = na.omit(dfi.red)
# summary(dfi.red)
library(googleVis)
Sector = dfi %>% group_by(Sector) %>% filter(Side == "Buy") %>% summarise(Sector.Performance = mean(Day5.P, na.rm = TRUE)) 
View(Sector)

Gauge =  gvisGauge(Sector, 
                    options=list(min=-5, max=5, greenFrom=1,
                                 greenTo=5, yellowFrom=0, yellowTo=1,
                                 redFrom=-5, redTo=0, width=1000, height=800))

plot(Gauge)

##SELL SIDE
Sector = dfi %>% group_by(Sector) %>% filter(Side == "Sell") %>% summarise(Sector.Performance = mean(Day5.P, na.rm = TRUE)) 

Gauge =  gvisGauge(Sector, 
                   options=list(min=-5, max=5, greenFrom=-5,
                                greenTo=-1, yellowFrom=-1, yellowTo=0,
                                redFrom=0, redTo=5, width=1000, height=800))

plot(Gauge)


#### LINEAR REGRESSION BETWEEN STOCK PRICE ON DAY 5, 4, 3, 2, 1 (without volume) AND --- 
#FOR EACH SIDE
#MARKET CAP, INDUSTRY, SECTOR, EXCHANGE, MONTH, VOLUME CHANGE ON DAY1, VALUE OF TRADE

############################################################################################################################################
############################################################################################################################################
#################################################### MACHINE LEARNING STUFF ##############################################
############################################################################################################################################
############################################################################################################################################


##CREATING SUBSET OF DATA SET 

regdata = subset(dfi, select = c("Side", "OwnerRel", "Share", "Price", "Valu", "Left", "MarketCap", "IPOyear", "Sector", "Industry", "Exchange", "month", "Day1.V", "Close1", "Day5.P", "Day5"))
regdata$MarketCapMult = substr(regdata$MarketCap, nchar(regdata$MarketCap), nchar(regdata$MarketCap))
regdata$MarketCap = gsub("\\$|B|M", "", regdata$MarketCap)
regdata$MarketCap = as.numeric(regdata$MarketCap)

regdata$MarketCapNew = ifelse(regdata$MarketCapMult == "B", regdata$MarketCap*1000000000,
                              ifelse(regdata$MarketCapMult == "M", regdata$MarketCap*1000000,regdata$MarketCap))

regdata$MarketCapNew = round(regdata$MarketCapNew, 2)
regdata = subset(regdata, select = -c(MarketCapMult))
names(regdata)

############################ PRINCIPAL COMPONENT DATA ############################
pca.data = subset(regdata, select = c(Share, Price, Left, Valu, MarketCapNew,Day1.V, IPOyear))
names(pca.data) = c("Share", "Price", "Shares.Left", "Value", "Market.Cap", "Change.in.Volume", "IPO.Year")

library(stats)
library(psych)
cors = cor(pca.data, use = "complete")

fa.parallel(cors, n.obs = 28769, fa = "pc", n.iter = 100) #WHAT IS N.ITER AGAIN? IS IT THE ITERS FOR THE RED DOTTED LINE?
abline(h = 1)

pc_insider = principal(cors, nfactors = 4, rotate = "none") 
cluster = c("Value", "Share", "Price")


factor.plot(pc_insider, labels = colnames(cors), jiggle = TRUE, title = "Insider Variables: PCA") 
#HOW DO YOU INTERPRET A THREE OR MORE DIMENSIONAL FACTOR PLOT
############# MULTIPLE LINEAR REGRESSION ###########
summary(regdata)
summary(regdata.buy)

regdata.buy = regdata[ regdata$Side == "Buy", -1] 
model.buy.under5 = lm(Day5.P ~ . - Day5 - Close1 - Industry - OwnerRel  - month - Price - Share -MarketCap, data = regdata.buy)
summary(model.buy.under5)

library(car)
par(mfrow = c(2,2))
plot(model.buy.under5)
influencePlot(model.buy.under5)
vif(model.buy.under5)

regdata.buy = regdata.buy[- c(129,6192), ]

regdata.sell = regdata[ regdata$Side == "Sell", -1] 
model.sell.under5 = lm(Day5.P ~ . - Day5 - Close1 - Industry - OwnerRel - month  - Price - Share, data = regdata.sell)
summary(model.sell.under5)


#######################  KNN #######################

knndata = subset(regdata, select =  -c(IPOyear, Day5, OwnerRel, MarketCap, Close1, Industry))
knndata$outcome = ifelse(knndata$Day5.P < -1.5, "Drop", 
                         ifelse(knndata$Day5.P > 1.5, "Rise", "Mediocre"))
knndata$outcome = as.factor(knndata$outcome)
summary(knndata)

#Removing records with NA in predictor variables.
#head(knndata,100)
knndata = na.omit(knndata)
# knndata = knndata[!is.na(knndata$Day1.V), ]
# knndata = knndata[!is.na(knndata$MarketCapNew), ]
# knndata = knndata[!is.na(knndata$Day5.P), ]

#Standardizing Continuous variables
normalize = function(x) {
  y = (x - min(x))/(max(x) - min(x))
  y
}

knndata$Share = normalize(knndata$Share)
knndata$Valu =  normalize(knndata$Valu)
knndata$Price =  normalize(knndata$Price)
knndata$Left = normalize(knndata$Left)
knndata$MarketCapNew = normalize(knndata$MarketCapNew)
knndata$Day1.V = normalize(knndata$Day1.V)

#Convert categorical data to 0 1 fields
kd = model.matrix( ~ .,knndata) 
kd = cbind(kd, knndata$outcome)
kd  = as.data.frame(kd)
kd = kd[ , -c(1, 36, 38,39,40)]
names(kd)
setnames(kd, old = "V41", new = "outcome")
summary(kd)
### Creating KNN training and test sets

kd.complete = kd
kd[ kd$month08 == 1, "outcome"] = NA  #Introducing NAs for all August records
summary(kd.complete)

#######  K = 167 
sqrt(nrow(kd)) #comes out to 167
library(deldir)
kd.167NN = kNN(kd, k = 167)
kd.167.results = table(kd.complete[kd.complete$month08 == 1, "outcome"], kd.167NN[kd.167NN$outcome_imp == TRUE, "outcome"])
kd.167.results

chisq.test(kd.167.results)
kd.167.results = matrix(kd.167.results)
dim(kd.167.results) = c(3,3)
chisq.test(kd.167.results)

kd %>% group_by(outcome) %>% summarise(sum(SideSell))

# knndata$outcome.complete = knndata$outcome
# knndata[ knndata$month == "08", "outcome"] = NA  #Introducing NAs for all August records








### Splitting into training and testing data sets

complete = knndata[!is.na(knndata$outcome), ]
complete = subset(complete, select = -c(outcome, month))
missing = knndata[is.na(knndata$outcome),  c(-11, -16, -17)]
  

str(missing)

library(kknn)
insider.euclidean.169 = kknn(outcome.complete ~ ., complete, missing, k = 169, distance = 2)
insider.euclidean.1 = kknn(outcome.complete ~ ., complete, missing, k = 1, distance = 2)

summary(insider.euclidean.169)
names(insider.euclidean.169)

fitted = insider.euclidean.1$fitted.values
original = knndata %>% filter(month == "08") %>% group_by(outcome.complete) %>% summarise(n())
summary(insider.euclidean.169)

knndata %>% filter(month == "08") %>% group_by(Side, outcome) %>% summarise(n())
knndata %>% filter(month != "08") %>% group_by(Side) %>% summarise(n())
names(knndata)




