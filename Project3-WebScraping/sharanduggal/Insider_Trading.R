library(dplyr)
library(quantmod)

Insiders = read.csv("/Users/sharanduggal/Documents/Datascience/Projects/WebScrapingProjects/Insider/Past_Year_Transactions", sep = "\t")

insider = Insiders

insider$Share = gsub(",", "", insider$Shares)
insider$Price = gsub(",", "", insider$SharePrice)
insider$Valu = gsub(",", "", insider$Value)
insider$Left = gsub(",", "", insider$Remaining)
insider$Valu = substring(insider$Valu, 2, nchar(insider$Valu))
insider$Price = substring(insider$Price, 2, nchar(insider$Price))
insider$Date = as.character(insider$Date)
insider[ ,3:8] = sapply(insider[ ,3:8], as.character)
insider$Symbol = toupper(insider$Symbol)
insider$Symbol = gsub("\\(|\\)", "", insider$Symbol)
insider$Symbol = gsub("\\[|\\]", "", insider$Symbol)
insider$Symbol = gsub("\\'", "", insider$Symbol)
insider$Symbol = gsub("NYSE: ", "", insider$Symbol)
insider$Symbol = gsub("NYSE:", "", insider$Symbol)


#insider[ ,14:17] = sapply(insider[ ,14:17], as.double) #CHANGE THESE TO NUMERIC SUCCESSFULLY
View(insider)
insider$Share = as.numeric(insider$Share)
insider$Left = as.numeric(insider$Left)
insider$Price = as.numeric(insider$Price)
#insider$Valu = as.numeric(insider$Valu)


### Getting stock symbols and associated info
tickers = stockSymbols()
str(tickers)
View(tickers)
ins = left_join(insider, tickers, by = "Symbol")

### Removing Unmatched records and Symbols that do not retrieve info from Quantmod #HCACU, LCA, QPACU, SNFCA
ins = ins[!is.na(ins$Name), ]
ins = ins[!(ins$Symbol %in% c("HCACU", "LCA", "QPACU", "SNFCA")) , ]

#View(ins.reduced) = head(ins)
symbols = ins$Symbol
dates = ins$Date

###Function to get Quantmod data and transpose it in an appendable format, i.e. single row for 6 days' worth of data. 
fiver = function(x,y) {
  temp = getSymbols(x, src='yahoo', from=y, auto.assign = FALSE)
  temp = as.data.frame(temp)
  #temp$Date = rownames(temp)
  rownames(temp) = NULL
  temp = temp[1:6, ]
  x1 = temp[ 1, 5:6]
  colnames(x1) = c("Volume1", "Close1")
  x2 = temp[ 2, 5:6]
  colnames(x2) = c("Volume2", "Close2")
  x3 = temp[ 3, 5:6]
  colnames(x3) = c("Volume3", "Close3")
  x4 = temp[ 4, 5:6]
  colnames(x4) = c("Volume4", "Close4")
  x5 = temp[ 5, 5:6]
  colnames(x5) = c("Volume5", "Close5")
  x6 = temp[ 6, 5:6]
  colnames(x6) = c("Volume6", "Close6")
  Symbol = x
  Date = y
  stock.row = cbind(Symbol,Date,x1,x2,x3,x4,x5, x6)
}


### Breaking up data into 5 parts to make it easier to download quantmod data and transpose it -- 28,000 rows in total.
ins = ins %>% mutate(Groups = ifelse(X <= 8000, 1, 
                                     ifelse(X > 8000 & X <= 16000, 2, 
                                            ifelse(X>16000 & X <= 24000, 3, 
                                                   ifelse(X>24000 & X<=32000, 4,
                                                          ifelse(X>32000 & X <41000, 5, 5))))))  
  
symbols = ins %>% filter(Groups == 5) %>% select(Symbol)
dates = ins %>% filter(Groups == 5) %>% select(Date)
symbols = unlist(symbols)
dates = unlist(dates)
typeof(dates)

stockprice5 = data.frame(Symbol = NA, Date = NA, Volume1 = NA, Close1 = NA, Volume2 = NA, Close2 = NA, 
                   Volume3 = NA, Close3 = NA, Volume4 = NA, Close4 = NA, Volume5 = NA, Close5 = NA,  
                   Volume6 = NA, Close6 = NA)

symbol= unique(symbols)


for (i in 1:length(symbols)){
  new.row = fiver(symbols[i], dates[i])
  stockprice5 = rbind(stockprice5, new.row)
}

### Cleaning and merging the 5 parts of the read-in stock prices
rownames(stockprice) = NULL
rownames(stockprice2) = NULL
rownames(stockprice3) = NULL
rownames(stockprice4) = NULL
rownames(stockprice5) = NULL
stockprice = stockprice[!is.na(stockprice$Symbol), ]
stockprice2 = stockprice2[!is.na(stockprice2$Symbol), ]
stockprice3 = stockprice3[!is.na(stockprice3$Symbol), ]
stockprice4 = stockprice4[!is.na(stockprice4$Symbol), ]
stockprice5 = stockprice5[!is.na(stockprice5$Symbol), ]
prices = rbind(stockprice, stockprice2, stockprice3, stockprice4, stockprice5)
write.csv(prices, file = "/Users/sharanduggal/Documents/Datascience/Projects/WebScrapingProjects/Insider/prices.csv")

library(data.table)
setnames(ins, old = c("Symbol","Date"), new = c("Symbol1", "Date1"))
dfi = cbind(ins,prices)
write.csv(dfi, file = "/Users/sharanduggal/Documents/Datascience/Projects/WebScrapingProjects/Insider/Insider_Final.csv")
write.csv(dfi, file = "/Users/sharanduggal/Documents/Datascience/Projects/WebScrapingProjects/Insider_Backup.csv")

write.table(dfi, file = "/Users/sharanduggal/Documents/Datascience/Projects/WebScrapingProjects/Insider/Insider_Text.txt", sep='\t', 
            row.names=FALSE)

ins %>% group_by(Groups) %>% summarise(n())
View(stockprice)


# for (i in 1378:length(symbol)){
#   getSymbols(symbol[i], from = "2016-08-08", src = "yahoo", auto.assign = FALSE)
# }

#getSymbols("RMD", from = "2016-08-04", src = "yahoo", auto.assign = FALSE)



