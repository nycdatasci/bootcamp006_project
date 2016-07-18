## Load Libraries and read data

library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)

SalaryData=read.csv("Salary.csv",stringsAsFactors = F)

## Getting the lat/lon of the State of America (excluding Alaska & Hawaii),DC included
DC=data.frame(state.name="District of Columbia",Abr="DC")
Gcode=geocode(as.character(States$state.name))
States= data.frame(state.name,Abr=state.abb) %>%
        rbind(DC) %>%
        filter(!state.name %in% c("Alaska","Hawaii")) %>%
        cbind(geocode(as.character(States$state.name)))
States[45,3]=-120
States[45,4]=47

# Question1: what is the data-related jobs demand distribution across United States?
##1.1 Create a table for demand mapping

SalaryData_Join=inner_join(SalaryData,States,by=c("WORK_STATE"="Abr"))

Number_Of_Demand= filter(SalaryData_Join,JOB_TITLE_SUBGROUP %in% c("data scientist","data analyst","business analyst")) %>%
        group_by(WORK_STATE) %>%
        count(WORK_STATE)

Number_Of_Demand=select(Number_Of_Demand,WORK_STATE=WORK_STATE,Number=n)
Demand_table=inner_join(Number_Of_Demand,States,by=c("WORK_STATE"="Abr"))


##1.2 mapping the demand on USA_State Map using size
Demand_table$Number=as.numeric(Demand_table$Number)
usa_center = as.numeric(geocode("United States"))
USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="panel")
graph1=USAMap +
        geom_point(aes(x=lon, y=lat,size=Number), data=Demand_table, col="blue", alpha=0.4,show.legend = T) +
        scale_size_area(max_size = 20)+
        ggtitle("Demand on Data-related jobs across USA(sample)")
plot(graph1)

## Question2: which city in CA,NY,NJ,TX have more demand on data-related jobs?
Cities= filter(SalaryData,WORK_STATE %in% c("NY","NJ","CA","TX")) %>%
        filter(JOB_TITLE_SUBGROUP %in% c("data scientist","data analyst","business analyst")) %>%
        group_by(WORK_CITY) %>%
        count(WORK_CITY) %>%
        arrange(desc(n)) %>%
        slice(1:8)

g=ggplot(Cities,aes(reorder(x=WORK_CITY,n,desc),y=n))      
g+geom_bar(stat = "identity",aes(fill=WORK_CITY))+
        ggtitle("Demand on data-related job across cities")+
        xlab("Cities")





# Question3 :which states provides higher average salary for data-related jobs?
##3.1 Create a table for salary mapping
Salary_state= filter(SalaryData_Join,JOB_TITLE_SUBGROUP %in% c("data scientist","data analyst","business analyst")) %>%
        group_by(WORK_STATE) %>%
        summarise(Avg_pay=mean(PAID_WAGE_PER_YEAR))
Salary_table=inner_join(Salary_state,States,by=c("WORK_STATE"="Abr"))
Salary_table=Salary_table[,-c(4,5)]



##3.2 mapping the salary on USA_State Map using continuous color
Salary_table$state.name=tolower(Salary_table$state.name)
mapdata=map_data("state")
Salary_table_join=inner_join(Salary_table,mapdata,by=c("state.name"="region"))
g1=ggplot(data = Salary_table_join,aes(x=long,y=lat))
graph2=g1+geom_polygon(aes(color="darkblue",group = group,fill=Avg_pay),show.legend = F) +
        scale_fill_continuous(low = "orange", high = "darkblue")+
        ggtitle("Average Salary for Data-related job Across USA(sample)")
plot(graph2)

#Question4  Which companys in CA,NY or WA provide more salary than others?

##4.1 choose those companies that hire more than 8 employees
High_Pay_Comp= filter(SalaryData,WORK_STATE %in% c("NY","WA","CA")) %>%
        filter(JOB_TITLE_SUBGROUP %in% c("data scientist","data analyst","business analyst")) %>%
        group_by(EMPLOYER_NAME) %>%
        count(EMPLOYER_NAME) %>%
        filter(n>=8) 
Salary_Rank=filter(SalaryData,WORK_STATE %in% c("NY","WA","CA")) %>%
        filter(JOB_TITLE_SUBGROUP %in% c("data scientist","data analyst","business analyst")) %>%
        inner_join(High_Pay_Comp) %>%
        group_by(EMPLOYER_NAME) %>%
        summarise(AVG_salary=mean(PAID_WAGE_PER_YEAR)) %>%
         arrange(desc(AVG_salary)) %>%
        slice(1:10)


##4.2  List those companies with highest salaries
g2=ggplot(Salary_Rank,aes(reorder(x=EMPLOYER_NAME,AVG_salary,desc),y=AVG_salary))      
       g2+geom_bar(stat = "identity",aes(fill=EMPLOYER_NAME))+
               ggtitle("Companies pay highest salary")+
               xlab("Companys")
               

       