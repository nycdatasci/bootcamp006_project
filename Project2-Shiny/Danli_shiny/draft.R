library(dplyr)
library(fmsb)
?radarchart
str(nes)
max_ <- rep(10, 14)
min_ <- rep(0, 14)
nesRadar <- rbind(max_, min_, nes)
nesRadar <- select(nesRadar, -c(Code, Economy))
radarchart(nesRadar[1:5, ], axistype = 1,
           pcol=rgb(0.2,0.5,0.5,0.9),
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), vlcex=0.5)

test <- filter(nes, nes$Economy == "USA")

nesRadar$Economy[-c(1,2)]

myNesRadar <- 
  nesRadar %>%
    filter(Economy %in% c("10", "0", "CHINA")) %>%
    select(-c(Code, Economy))

radarchart(myNesRadar, axistype = 1,
           pcol="RdGy", plwd = 2.5,
           cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,20,5), vlcex=0.5)

names(aps)

arg <- c("Perceived.Capabilities", "Perceived.Opportunities")
arg[1]

test1 <- top_n(aps, 2, aps$Perceived.Opportunities)

test2 <- prop("x", as.symbol(arg[1]))
??prop
