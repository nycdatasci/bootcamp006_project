library(corrplot)
library(car)
library(dplyr)
library(ggplot2)

data = as.data.frame(read.csv('/Users/dk1306/Desktop/gofundme.csv'))[,-1]

str(data)
head(data)
summary(data)
sapply(data,sd)
sapply(data, class)
cor(data)


corrplot(cor(data[,-1]))

data_category = data %>%
                    group_by(category)%>%
                    summarise("Target" = mean(target),
                              "Current" = mean(current),
                              "complete"= mean(percent_complt),
                              "People" = mean(people),
                              "Average_Contribution"= mean(avg_contrb),
                              "Shares"= mean(shares),
                              "Days"= mean(days ))
  

#######################################################
                  
p <- plot_ly(
  x = data_category$category,
  y = data_category$Shares,
  type = "bar"
  
  )%>%
  layout(
    title = "Average Number of Facebook Shares of a Fundraising Campaign",
    xaxis = list(title = "Category"),
    yaxis = list(title = "Number of Facebook Shares"),
    margin = list(l = 65)
)

p

#######################################################

p <- plot_ly(
  x = data_category$category,
  y = data_category$Target,
  type = "bar"
  
)%>%
  layout(
    title = "Average Targeted $ of a Fundraising Campaign",
    xaxis = list(title = "Category"),
    yaxis = list(title = "Dollars"),
    margin = list(l = 65)
  )

p

#######################################################

p <- plot_ly(
  x = data_category$category,
  y = data_category$Current,
  type = "bar"
  
)%>%
  layout(
    title = "Average curret $ raised by a Fundraising Campaign",
    xaxis = list(title = "Category"),
    yaxis = list(title = "Dollars"),
    margin = list(l = 65)
  )

p

#######################################################

p <- plot_ly(
  x = data_category$category,
  y = data_category$complete,
  type = "bar"
  
)%>%
  layout(
    title = "Average current completion percentage of a Fundraising Campaign",
    xaxis = list(title = "Category"),
    yaxis = list(title = "%"),
    margin = list(l = 65)
  )

p

#######################################################

p <- plot_ly(
  x = data_category$category,
  y = data_category$Average_Contribution,
  type = "bar"
  
)%>%
  layout(
    title = "Average per person $ contribution  to a Fundraising Campaign",
    xaxis = list(title = "Category"),
    yaxis = list(title = "Dollars"),
    margin = list(l = 65)
  )

p

#######################################################

p <- plot_ly(
  x = data_category$category,
  y = data_category$Days,
  type = "bar"
  
)%>%
  layout(
    title = "Average Number of days a Fundraising Campaign is running",
    xaxis = list(title = "Category"),
    yaxis = list(title = "Number of days"),
    margin = list(l = 65)
  )

p

#######################################################

p <- plot_ly(
  x = data_category$category,
  y = data_category$People,
  type = "bar"
  
)%>%
  layout(
    title = "Average Number of people contributing to  a Fundraising Campaign",
    xaxis = list(title = "Category"),
    yaxis = list(title = "Number of People"),
    margin = list(l = 65)
  )

p

#######################################################

#All Applications
corrplot(cor(data[,-1]))


#Sports
data_subcategory = filter(data, category ==  levels(data$category)[7])
corrplot(cor(data_subcategory[,-1]))


#Emergencies
data_subcategory = filter(data, category ==  levels(data$category)[4])
corrplot(cor(data_subcategory[,-1]))



#Number of days plot  

plot_ly(data, y =  days, color = category, type = "box")
