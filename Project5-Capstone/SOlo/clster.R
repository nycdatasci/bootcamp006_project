x <- Crashes

#Splitting the Date.Time column into Data and Time

x$Date <- substr(x$Date.Time, 1,10)
x$Time <- substr(x$Date.Time, 12,19)

#Removing the unnecessary data

x <- subset( x, select = -Date.Time )
x <- subset( x, select = -Crash.Number )
x <- subset( x, select = -Street.Number )
x <- subset( x, select = -Cross.Street )
x <- subset( x, select = -Location )
x <- subset( x, select = -Street.Name )
x <- subset( x, select = -Coordinates )
x <- subset( x, select = -Steet.Name )


#Change the Data string into Date format
x$Date <- as.Date(x$Date, "%m/%d/%Y")

#Remove missing values
x <- x[complete.cases(x),]

#d <- dist(as.matrix(x))        # find distance matrix 
#hc <- hclust(d)                # apply hirarchical clustering 
#plot(hc)                       # plot the dendrogram



xNumeric = lapply(x, as.numeric)
xNumeric <- data.frame(xNumeric)
xNumeric <- subset( xNumeric, select = -Time )


# Prepare Data
xNumeric <- na.omit(xNumeric) # listwise deletion of missing
mydata <- scale(xNumeric) # standardize variables


# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 4) # 4 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)


# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=4, border="red")



# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)


# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model



# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 4)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)







# Time Series Forecasting

table(x$Date)
myts <- data.frame(table(x$Date))
myts$Var1 <- as.Date(myts$Var1, "%Y-%m-%d")


years <- format(myts$Var1, "%Y")
tab <- table(years)
tab

tim <- data.frame(tab)
names(tim)[1] <- "years"
names(tim)[2] <- "Freq"
tim <- tim[1:7,]
arima(tim$years, order = c(1,0,0))

f <- forecast(arima(tim$years, order = c(1,0,0)))
(forecast(f, 1))


ggplot(tim , aes(tim$years, tim$Freq)) + geom_point() + xlab("Years") + ylab("Frequency")

