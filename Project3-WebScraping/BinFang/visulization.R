library(dplyr)
library(ggplot2)
library(plotly)
library(data.table) 
library(tidyr)

setwd("/Users/binfang/Documents/NYCDSA/project/Project_3/data")

files = list.files(pattern = ".csv")
temp = lapply(files, fread, sep=",")
car_data <- rbindlist(temp)
colnames(car_data) = c("Price", "Mileage", "City", "State", "Engine", "Transmission", 
                "Drive Type", "Fuel Type", "MPG City", "MPG Highway", 
                "Exterior Color", "Year", "Make", "Model")

car_data[car_data == "N/A"] = NA
car_data[car_data == "N"] = NA
car_data[car_data == "A"] = NA

# Separate words by initial capitalized letter
sep_string = function(x){gsub('([[:upper:]])', ' \\1', x)}
car_data$City = lapply(car_data$City, sep_string)

Metropolitan = c(rep("Atlanta", 1000), rep("Chicago", 1000), rep("DC", 1000),
                 rep("Denver", 1000), rep("Houston",1000), rep("Los Angeles", 1000),
                 rep("Miami", 1000), rep("New York", 1000), rep("Phoenix", 1000), 
                 rep("Seattle", 1000))
car_data = cbind(car_data, Metropolitan)
car_data = data.frame(car_data)


cols.num = c("Price", "Mileage", "MPG.City", "MPG.Highway")
car_data[cols.num] = sapply(car_data[cols.num],as.numeric)


# Summarize average price, mileage, MPG City/Hwy
car_summary = aggregate(cbind(Price, Mileage, MPG.City, MPG.Highway)
                        ~ Metropolitan, data = car_data, FUN = mean, na.rm=T)

## Bar Plot
setwd("/Users/binfang/Documents/NYCDSA/project/Project_3/results")
car_summary_melt1 = melt(car_summary[,c('Metropolitan', 'Price', 
                                        'Mileage')],id.vars = 1)
car_summary_melt2 = melt(car_summary[,c('Metropolitan', 'MPG.City', 
                                        'MPG.Highway')],id.vars = 1)

ggplot(car_summary_melt1, aes(x = Metropolitan, y = value)) +
  geom_bar(aes(fill = variable),position = "dodge", stat="identity") + 
  scale_fill_brewer(palette="Set1") + xlab("City") +
  ylab("Price/Mileage") + theme_bw() + ggtitle("")
# ggsave("barplot1.png", plot = last_plot())

ggplot(car_summary_melt2, aes(x = Metropolitan, y = value)) +
  geom_bar(aes(fill = variable),position = "dodge", stat="identity") + 
  scale_fill_brewer(palette="Set1") + xlab("City") +
  ylab("MPG") + theme_bw() + ggtitle("")

# p = plot_ly(x=car_summary_melt1$Metropolitan, y=car_summary_melt1$value, 
#         type = "bar", color = car_summary_melt1$variable) %>%
#   layout(xaxis = list(title = "City"), 
#          yaxis = list(title = "Price/Mileage"), 
#          title = "")
# Sys.setenv("plotly_username"="fangbin08") 
# Sys.setenv("plotly_api_key"="h70zcu93k6")
# plotly_IMAGE(p, format = "png", out_file = "barplot1.png")
# 
# p = plot_ly(x=car_summary_melt2$Metropolitan, y=car_summary_melt2$value, 
#         type = "bar", color = car_summary_melt2$variable) %>%
#   layout(xaxis = list(title = "City"), 
#          yaxis = list(title = "MPG"), 
#          title = "")
# Sys.setenv("plotly_username"="fangbin08") 
# Sys.setenv("plotly_api_key"="h70zcu93k6")
# plotly_IMAGE(p, format = "png", out_file = "barplot2.png")

## Scatter plot
# pal = RColorBrewer::brewer.pal(nlevels(car_data$Metropolitan), "Set1")
# p = plot_ly(data = car_data, x = Price, y = Mileage, color = Metropolitan,
#         colors = pal, mode = "markers") %>%
#   layout(xaxis = list(title = "Price"), 
#          yaxis = list(title = "Mileage"), 
#          title = "")
# Sys.setenv("plotly_username"="fangbin08") 
# Sys.setenv("plotly_api_key"="h70zcu93k6")
# plotly_IMAGE(p, format = "png", out_file = "scatter1.png")



ggplot(car_data, aes(x=Price, y=Mileage, color=Metropolitan)) + 
  geom_point() + scale_color_brewer(palette="Spectral") + xlab("Price") +
  ylab("Mileage") + theme_bw() + ggtitle("")

ggplot(car_data, aes(x=MPG.City, y=MPG.Highway, color=Metropolitan)) + 
  geom_point() + scale_color_brewer(palette="Spectral") + xlab("MPG City") +
  ylab("MPG Highway") + theme_bw() + ggtitle("") 


# Summarize the top 5 car makes
car_make_summary = aggregate(cbind(Make, Model)
                        ~ Metropolitan, data = car_data, FUN = length)
car_make_summary = aggregate(cbind(Make, Model) ~ Metropolitan, 
                             data = car_data, function(x) unique(x))


make_uni = data.frame(with(car_data, table(Metropolitan, Make)))
make_uni = make_uni[order(make_uni$Metropolitan),]

# aggregate(Freq ~ Metropolitan, data = make_uni, max)
make_uni_order = arrange(make_uni, desc(Freq), Metropolitan)




require(data.table)
make_uni_top3 = data.table(make_uni, key="Freq")
make_uni_top3 = make_uni_top3[, head(.SD, 3), by=Freq]







