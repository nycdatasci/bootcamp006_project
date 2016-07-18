library (dplyr) 
library (ggplot2) 
library (ggthemes) 
library(zipcode) 
library (lubridate) 
library(choroplethr)
library (choroplethrZip) 
library(labeling)
library(chron)

setwd("C:\\Users\\trichna\\Documents\\NYCDSA\\Project 1")
NYPD_collision = read.csv("NYPD_Motor_Vehicle_Collisions.csv",stringsAsFactors = FALSE)
#841,853 observations


NYPD_collision$DATE = as.Date(NYPD_collision$DATE, "%m/%d/%Y")
NYPD_collision$Year = year(NYPD_collision$DATE)

tbl_NYPD = tbl_df(NYPD_collision)

str(NYPD_collision)
unique(NYPD_collision$BOROUGH)
 
#Total number of Individuals killed by years

grp_by_borough = NYPD_collision %>% filter(BOROUGH != "") %>% group_by(BOROUGH,Year)
#636,343 observations


Borough_fatalities_by_years = grp_by_borough %>% dplyr::summarise(total_num_fatalities = sum(NUMBER.OF.PERSONS.KILLED)) 

#Total number of People killed by years
Total_fatalities_by_years = ggplot(data = Borough_fatalities_by_years, aes(x = Year, y = total_num_fatalities)) 
Total_fatalities_by_years + geom_bar(aes(fill = BOROUGH), stat = 'identity') + theme_economist() + theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Total Fatalities by Year') + ylab("Total number of Fatalities\n") + xlab("\nYear")  + theme(axis.text.x = element_text(vjust = 0, angle = 0, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5))
#We see a decreasing trend overall in terms number of fatalities by years


#As a part of the next step, I further investigated the fatalities among the three groups - motorists, pedestrians and cyclists to understand if the big picture of declining trend holds good within each of the five boroughs or if there were 
#any exceptions observed within the boroughs.

#Total number of Motorists killed by years
Borough_mot_ped_cyc_fatalities_by_years = grp_by_borough %>% dplyr::summarise(total_num_motorist_fatalities = sum(NUMBER.OF.MOTORIST.KILLED),total_num_ped_fatalities = sum(NUMBER.OF.PEDESTRIANS.KILLED), total_num_cyclist_fatalities = sum(NUMBER.OF.CYCLIST.KILLED)  ) 

Total_mot_fatalities_by_years = ggplot(data = Borough_mot_ped_cyc_fatalities_by_years, aes(x = Year, y = total_num_motorist_fatalities)) 
Total_mot_fatalities_by_years + geom_bar(aes(fill = BOROUGH), stat = 'identity') + facet_wrap(~BOROUGH)   + theme_economist() +  theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Total number of Motorist Fatalities by Year') + ylab("Number of Fatalities\n") + xlab("\nYear")  + theme(axis.text.x = element_text(vjust = 0, angle = 0, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5))
#Although the overall trend shows a decline in the number of motorist deaths in Brooklyn, Manhattan and Queens, Bronx and Staten Island seem to have the opposite
#trend. 

#Total number of Pedestrians killed by years
Total_ped_fatalities_by_years = ggplot(data = Borough_mot_ped_cyc_fatalities_by_years, aes(x = Year, y = total_num_ped_fatalities)) 
Total_ped_fatalities_by_years + geom_bar(aes(fill = BOROUGH), stat = 'identity') + facet_wrap(~BOROUGH)   + theme_economist() +  theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Total number of Pedestrian Fatalities by Year') + ylab("Number of Fatalities\n") + xlab("\nYear")  + theme(axis.text.x = element_text(vjust = 0, angle = 0, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5))
#Brooklyn seems to be different from other boroughs where we see an increasing trend in number of pedestrian fatalities. This 
#shows that additional effort needs to made in increasing the overall pedestrian safety in Brooklyn

#Total number of cyclists killed by years
Total_cycl_fatalities_by_years = ggplot(data = Borough_mot_ped_cyc_fatalities_by_years, aes(x = Year, y = total_num_cyclist_fatalities)) 
Total_cycl_fatalities_by_years + geom_bar(aes(fill = BOROUGH), stat = 'identity') + facet_wrap(~BOROUGH)   + theme_economist()  + theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Total number of Cyclist Fatalities by Year') + ylab("Number of Fatalities\n") + xlab("\nYear")  + theme(axis.text.x = element_text(vjust = 0, angle = 0, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5))
# Brooklyn and Queens seem to have a higher number of cyclist related fatalities compared to other boroughs. This probably shows
#that NYPD and city officials need to focus more on targeted measures that would improve safety of cyclists


# -------------------------------------------------------------------
#Analysis of fatalities and injuries by Contributing factor

unique(NYPD_collision$CONTRIBUTING.FACTOR.VEHICLE.1) #returns 46 unique values, 43 are really unique after excluding NA, ""
contributing_factor = grp_by_borough %>% filter(CONTRIBUTING.FACTOR.VEHICLE.1 != "" & CONTRIBUTING.FACTOR.VEHICLE.1 != 'Unspecified' ) %>% group_by(BOROUGH,CONTRIBUTING.FACTOR.VEHICLE.1)

contributing_factor_analysis = contributing_factor  %>% dplyr::summarise(num_killed = sum(NUMBER.OF.PERSONS.KILLED), num_injured = sum(NUMBER.OF.PERSONS.INJURED))


top5_contributing_factor_fatalities = contributing_factor_analysis  %>% arrange(desc(num_killed))  %>% group_by(BOROUGH)  %>%  do(head(., n = 6)) %>% select(BOROUGH, CONTRIBUTING.FACTOR.VEHICLE.1, num_killed)

top5_contributing_factor_fatalities = transform(top5_contributing_factor_fatalities,CONTRIBUTING.FACTOR.VEHICLE.1 =reorder(CONTRIBUTING.FACTOR.VEHICLE.1,-num_killed) )

Total_fatalities_by_contributing_factors = ggplot(data = top5_contributing_factor_fatalities, aes(x = CONTRIBUTING.FACTOR.VEHICLE.1, y = num_killed))
Total_fatalities_by_contributing_factors + geom_bar(aes(fill = BOROUGH), stat = 'identity') + theme_economist() + theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Total Fatalities by Contributing Factors') + ylab("Total number of Fatalities\n") + xlab("\nContributing Factor")  + theme(axis.text.x = element_text(vjust = 0, angle = 90, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
# the plot shows the top 8 factors that account for nearly 50% of the accident related fatalities. 

top5_contributing_factor_injuries = contributing_factor_analysis  %>% arrange(desc(num_injured))  %>% group_by(BOROUGH)  %>%  do(head(., n = 8)) %>% select(BOROUGH, CONTRIBUTING.FACTOR.VEHICLE.1, num_injured)

top5_contributing_factor_injuries = transform(top5_contributing_factor_injuries,CONTRIBUTING.FACTOR.VEHICLE.1 =reorder(CONTRIBUTING.FACTOR.VEHICLE.1,-num_injured) )
Total_injuries_by_contributing_factors = ggplot(data = top5_contributing_factor_injuries, aes(x = CONTRIBUTING.FACTOR.VEHICLE.1, y = num_injured))
Total_injuries_by_contributing_factors + geom_bar(aes(fill = BOROUGH), stat = 'identity') + theme_economist() + theme(legend.position = "right") + theme(legend.text=element_text(size=5)) + ggtitle('Total Injuries by Contributing Factors') + ylab("Total number of Injured\n") + xlab("\nContributing Factor")  + theme(axis.text.x = element_text(vjust = 0, angle = 90, hjust = 0.5)) + theme(legend.position = "right") + theme(legend.text=element_text(size=10)) + theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
# the plot shows the top 5 factors that account for nearly 50% of the accident related injuries. 

NYPD_collision %>%  filter(BOROUGH != "") %>% summarise(N_INJURED = sum(NUMBER.OF.PERSONS.INJURED), N_killed = sum(NUMBER.OF.PERSONS.KILLED))
#159176 injured & 749 killed 

#---------------------------------------------------------------------------
#Analysis of trend in fatalities and injuries across years based on contributing factors

temp = grp_by_borough %>% filter(CONTRIBUTING.FACTOR.VEHICLE.1 != "" & CONTRIBUTING.FACTOR.VEHICLE.1 != 'Unspecified' )
target = c("Traffic Control Disregarded", 'Driver Inattention/Distraction', 'Failure to Yield Right-of-Way', 'Following Too Closely','Driver Inexperience','Fatigued/Drowsy','Physical Disability' , 'Alcohol Involvement','Passenger Distraction' , 'Aggressive Driving/Road Rage')
contributing_factor_year =temp %>%  filter(CONTRIBUTING.FACTOR.VEHICLE.1  %in% target )  %>% group_by(CONTRIBUTING.FACTOR.VEHICLE.1, Year)  %>% summarise(YN_INJURED = sum(NUMBER.OF.PERSONS.INJURED), YN_killed = sum(NUMBER.OF.PERSONS.KILLED), YN_total = sum(YN_killed,YN_INJURED))


unique(contributing_factor_year$CONTRIBUTING.FACTOR.VEHICLE.1)


g = ggplot(data = contributing_factor_year, aes(x = Year  ))
g + geom_line(aes(y = YN_killed, colour = CONTRIBUTING.FACTOR.VEHICLE.1)) +    facet_wrap(~CONTRIBUTING.FACTOR.VEHICLE.1)  + xlab('Year') + ylab('Number of Fatalities/Injuries') + ggtitle('Total Fatalities by years based on contributing factors') + theme_economist() + theme(legend.text=element_text(size=8)) + theme(axis.text.x = element_text(size = 8 , angle = 90, hjust = 0)) + theme(axis.text.y = element_text(size = 8 , angle = 0, hjust = 0)) + theme(plot.title = element_text(hjust = 0.5))
#Most of the factors in the top 10 category show a declining trend except 'Traffic control disregarded' and 'Following too closely' which are slightly erratic



#--------------------------------------------------------------------

#Analysis of the fatalities by Time of the day

grp_by_borough$upd_time = times(paste0(grp_by_borough$TIME,":00"))
str(grp_by_borough) # check if upd_time is of time data type


Morning_fatalities = grp_by_borough %>% filter(upd_time > times('06:00:00') & upd_time < times('12:00:00')) %>% dplyr::summarise(Morning_fatalities = sum(NUMBER.OF.PERSONS.KILLED))
Afternoon_fatalities = grp_by_borough %>% filter(upd_time > times('12:00:00') & upd_time < times('17:00:00')) %>% dplyr::summarise(Afternoon_fatalities = sum(NUMBER.OF.PERSONS.KILLED)) 
Evening_fatalities = grp_by_borough %>% filter(upd_time > times('17:00:00'))  %>% dplyr::summarise(Evening_fatalities = sum(NUMBER.OF.PERSONS.KILLED)) 
Night_fatalities = grp_by_borough %>% filter(upd_time > times('00:00:00') & upd_time < times('06:00:00')) %>% dplyr::summarise(Night_fatalities = sum(NUMBER.OF.PERSONS.KILLED)) 

#Issue : while checking for time using chron package (times function), i was not able
#to select the rows where time later than 9 pm and less than midnight.  00:00:00 hours
#does not seem to work

concat_list = list(Morning_fatalities, Afternoon_fatalities, Evening_fatalities, Night_fatalities) 
#temp = cbind(Morning_fatalities,Afternoon_fatalities,Evening_fatalities,Late_Evening_fatalities,Overnight_fatalities )
#Time_of_day_fatalities = temp %>% select(BOROUGH,Year,Morning_fatalities,Afternoon_fatalities,Evening_fatalities,Late_Evening_fatalities,Overnight_fatalities )
#Time_of_day_fatalities1 = temp %>% select(1,2,3,6,9,12,15)

#Issue: cbind cannot be used when we want to de-duplicate the common column 
#from the dataframes being binded. So, I have used merge function recursively using Reduce


Time_of_day_fatalities = Reduce(function(x,y) merge(x,y), concat_list) 
Time_of_day_fatalities = mutate(Time_of_day_fatalities, Total_fatalities = apply(Time_of_day_fatalities[,3:6], 1, sum)) 


g = ggplot(data = Time_of_day_fatalities, aes(x = Year  ))
g + geom_line(aes(y = Morning_fatalities, color='Morning - 06:00 to 12:00 hours')) + geom_line(aes(y = Afternoon_fatalities, color='Afternoon - 12:00 to 17:00 hours')) + geom_line(aes(y = Evening_fatalities, color='Evening - 17:00 to Midnight hours')) + geom_line(aes(y = Night_fatalities, color='Overnight - Midnight to 06:00 hours')) + facet_grid(~BOROUGH)  + xlab('Year') + ylab('Number of Fatalities') + ggtitle('Total Fatalities based on time of the day') + theme_economist() + theme(legend.text=element_text(size=8)) + theme(axis.text.x = element_text(size = 8 , angle = 90, hjust = 0)) + theme(axis.text.y = element_text(size = 8 , angle = 0, hjust = 0)) + theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------------------------------

#Analysis of the fatalities and injuries by location (zip)


  dplyr::summarise(NYPD_collision,n_distinct(ZIP.CODE))  ## returns 193 zip codes
  na_flag = is.na(grp_by_borough)
  colnames(grp_by_borough)[colSums(na_flag) > 0] ## returns 4 columns : "ZIP.CODE"  "LATITUDE"  "LONGITUDE" "zip"  
  
#Remove NA values from zip column  
  
  grp_by_borough = grp_by_borough %>% mutate(zip = as.character(ZIP.CODE))
  zip_not_na = grp_by_borough[complete.cases(grp_by_borough[,'zip']),] #returns 636,263 observations. 80 observations had NA for zip
#Determine total number of fatalities and injuries at a zzip code level
   grp_by_zip = zip_not_na %>% group_by(BOROUGH,zip) %>%  dplyr::summarise(num_killed = sum(NUMBER.OF.PERSONS.KILLED), num_injured = sum(NUMBER.OF.PERSONS.INJURED)) 
  
  zipcodes = grp_by_zip[-1]  
  zipcodes$region = clean.zipcodes(zipcodes$zip)
  
  grp_by_zip[duplicated(grp_by_zip$zip),] ## returns '11208','11237','11385','11421'
  grp_by_zip[grp_by_zip$zip %in% c('11385','11421'),]
  zipcodes[zipcodes$zip %in% c('11208','11237','11385','11421'),]
  zipcodes = zipcodes %>% filter(!zip %in% c('11208','11237','11385','11421', '11242', '11249', '10000', '10048', '10123', '10281', '11695')) 
  ###################################
 # BOROUGH   zip num_killed num_injured
#  <chr> <chr>      <int>       <int>
#  1  BROOKLYN 11208          8        2402
#  2 BROOKLYN 11237          2         845
#  3 BROOKLYN 11385          0          10
#  4 BROOKLYN 11421          0           0
#  5   QUEENS 11208          1          42
#  6   QUEENS 11237          2          67
#  7   QUEENS 11385          6        1533
#  8   QUEENS 11421          6         619
#  ###################################
  
  
 ## Regions not mappable - '11242', '11249', '10000', '10048', '10123', '10281', '11695'
  
  fatalities_by_zip = zipcodes %>% select(region, value = num_killed)
  injuries_by_zip =  zipcodes %>% select(region, value = num_injured)
  
  Total_fatalities_by_zipcodes <- zip_choropleth(fatalities_by_zip, zip_zoom=fatalities_by_zip$region, title = "Total Fatalities by Zip Codes", legend = "Number of Total Fatalities", num_colors = 5) 
  Total_Injuries_by_zipcodes <- zip_choropleth(injuries_by_zip, zip_zoom=injuries_by_zip$region, title = "Total Injuries by Zip Codes", legend = "Number of Total Injuries", num_colors = 9) 
  
  #Observation: South eastern Brooklyn, Northern Bronx and southern Queens have the most
  #number of fatalities and injuries
  
 