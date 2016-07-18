setwd("/Users/binfang/Documents/Data_Academy/project/Project_1/matlab_codes")
anoma_avg = readMat("anoma_avg.mat")


year = c("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991",
              "1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
              "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
table_temp = rbind(year, anoma_avg$ano.east.win)

anoma_east = data.frame(Year =  as.factor(table_temp[1,]), Temperature = as.numeric(table_temp[2,]), 
                        Rainfall = as.numeric(table_temp[3,]), Snow_Cover = as.numeric(table_temp[4,]), 
                        Soil_Wetness = as.numeric(table_temp[5,]), Vegetation_Transipiration = as.numeric(table_temp[6,]))

###########################################

df = melt(anoma_east[, c("Year", "Temperature", "Rainfall", "Snow_Cover", 
"Soil_Wetness", "Vegetation_Transipiration")], id="Year")
ggplot(df) + geom_line(aes(x=Year, y=value, color=variable, group = 1))  + 
facet_wrap( ~ variable, scales="free") + scale_x_discrete(breaks=c("1980","1985","1990",
"1995","2000","2005","2010","2015"), labels=c("80","85","90","95","00","05","10","15")) +
geom_area(aes(x=Year, y=value, color=variable, group = 1), stat="identity", fill="black", alpha=.3) + 
  geom_rect(aes(x=Year, y=value, xmin = "1982", xmax = "1983",ymin = -Inf, ymax = Inf), 
            fill="deeppink",stat="identity", alpha = 0.01) +
  geom_rect(aes(x=Year, y=value, xmin = "1997", xmax = "1998",ymin = -Inf, ymax = Inf),
            fill="deeppink",stat="identity", alpha = 0.01) +
  geom_rect(aes(x=Year, y=value, xmin = "2014", xmax = "2015",ymin = -Inf, ymax = Inf),
            fill="deeppink",stat="identity", alpha = 0.01) +
  geom_rect(aes(x=Year, y=value, xmin = "1988", xmax = "1989",ymin = -Inf, ymax = Inf),
            fill="cyan",stat="identity", alpha = 0.01) +
  geom_rect(aes(x=Year, y=value, xmin = "2007", xmax = "2008",ymin = -Inf, ymax = Inf),
            fill="cyan",stat="identity", alpha = 0.01) +
  labs(title="Variable Anomalies of U.S. East")  

# ggsave("us_east.pdf",width = 25, height = 12.5, units = "cm")



######################################################################################

year = c("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991",
         "1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
         "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
table_temp = rbind(year, anoma_avg$ano.west.win)

anoma_west = data.frame(Year =  as.factor(table_temp[1,]), Temperature = as.numeric(table_temp[2,]), 
                        Rainfall = as.numeric(table_temp[3,]), Snow_Cover = as.numeric(table_temp[4,]), 
                        Soil_Wetness = as.numeric(table_temp[5,]), Vegetation_Transipiration = as.numeric(table_temp[6,]))

###########################################
df = melt(anoma_west[, c("Year", "Temperature", "Rainfall", "Snow_Cover", 
                         "Soil_Wetness", "Vegetation_Transipiration")], id="Year")
ggplot(df) + geom_line(aes(x=Year, y=value, color=variable, group = 1))  + 
  facet_wrap( ~ variable, scales="free") + scale_x_discrete(breaks=c("1980","1985","1990",
                                                                     "1995","2000","2005","2010","2015"), labels=c("80","85","90","95","00","05","10","15")) +
  geom_area(aes(x=Year, y=value, color=variable, group = 1), stat="identity", fill="black", alpha=.3) + 
  geom_rect(aes(x=Year, y=value, xmin = "1982", xmax = "1983",ymin = -Inf, ymax = Inf), 
            fill="deeppink",stat="identity", alpha = 0.01) +
  geom_rect(aes(x=Year, y=value, xmin = "1997", xmax = "1998",ymin = -Inf, ymax = Inf),
            fill="deeppink",stat="identity", alpha = 0.01) +
  geom_rect(aes(x=Year, y=value, xmin = "2014", xmax = "2015",ymin = -Inf, ymax = Inf),
            fill="deeppink",stat="identity", alpha = 0.01) +
  geom_rect(aes(x=Year, y=value, xmin = "1988", xmax = "1989",ymin = -Inf, ymax = Inf),
            fill="cyan",stat="identity", alpha = 0.01) +
  geom_rect(aes(x=Year, y=value, xmin = "2007", xmax = "2008",ymin = -Inf, ymax = Inf),
            fill="cyan",stat="identity", alpha = 0.01) +
  labs(title="Variable Anomalies of U.S. West")  

# ggsave("us_west.pdf",width = 25, height = 12.5, units = "cm")

