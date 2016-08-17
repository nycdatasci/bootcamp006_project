
## load the libraries
library(rjson)
library(tidyr)
library(dplyr)



Geo_data= read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv',stringsAsFactors = F)
Geo_data=read.csv("Geo_code.csv",stringsAsFactors = F)
Geo_data=Geo_data[,c("COUNTRY","CODE")]

###########  Load data collected from web scraping(txt,json file)
## Read data (txt) collected from web scraping about the global top 500 website 
Topsite=read.table("record.txt",col.names = "Top_Globe",stringsAsFactors = F)
Topsite_country=read.table("record3.txt",col.names = "countryName_G",stringsAsFactors = F,sep="\n")
Top_Inf=cbind(Topsite,Topsite_country)

## navigate the path of a json file from local computer
json_file = "Site.json"

## import the json file from into R environment as a list
json_data = fromJSON(paste(readLines(json_file), collapse=""))





########### Data Preparation
## impute missing data
which(names(json_data)=="Myanmar")
json_data[[78]][500]=""

## Convert the list into dataframe
df=as.data.frame(json_data,stringsAsFactors = F)

## df1 is the  dataframe is the original dataframe for analytics; df2 is a fraction of df1
df1=cbind(Global_rank=c(1:500),indicator=c(500:1),Top_Inf,df)
df2=cbind(Global_rank=c(1:500),indicator=c(500:1),Top_Inf)

## reshape the data structure for visualization
df1_reshape=gather(df1,country,Website,Afghanistan:Zimbabwe)





###########  Modeling
## New standard came out
Popularity_By_countries= select(df1_reshape,Website) %>%
                        group_by(Website) %>%
                        summarise(number_of_country=n()) %>%
                        arrange(desc(number_of_country))
Popularity_By_countries$importance1=128- Popularity_By_countries$number_of_country   

## Combine the two standards of rank
Join_table=left_join(df2,Popularity_By_countries,by=c("Top_Globe"="Website"))
Join_table$rank_by_countries=rank(Join_table$importance1,ties.method="min")

## Derive a new metric here
Join_table_bias=cbind(Join_table,bias=Join_table$Global_rank - Join_table$rank_by_countries)





########### Dataframe for visualization
## The Pos_biased table for visualization
Pos_bias_table= filter(Join_table_bias,Join_table_bias$bias>=200)
Pos_bias_table$bias_website=Pos_bias_table$Top_Globe
Pos_bias_table_join=left_join(df1_reshape,Pos_bias_table[,c(3,4,8,9)],by=c("Website"="Top_Globe"))
Pos_bias_table_join=left_join(Pos_bias_table_join,Geo_data,by=c("country"="COUNTRY"))
write.csv(Pos_bias_table_join,"Pos_bias_table_join.csv")

## The Neg_biased table for visulization
Neg_bias_table=filter(Join_table_bias,Join_table_bias$bias<=-200)
Neg_bias_table$bias_website=Neg_bias_table$Top_Globe
Neg_bias_table_join=left_join(df1_reshape,Neg_bias_table[,c(3,4,8,9)],by=c("Website"="Top_Globe"))
Neg_bias_table_join=left_join(Neg_bias_table_join,Geo_data,by=c("country"="COUNTRY"))
write.csv(Neg_bias_table_join,"Neg_bias_table_join.csv")

## The Neutural table for visualization
Neutural_table=filter(Join_table_bias,Join_table_bias$bias %in% c(-199:199)) %>%
              arrange(Global_rank) %>%
              head(100)
Neutural_table$Neutural_website=Neutural_table$Top_Globe
Neutural_table_join=left_join(df1_reshape,Neutural_table[,c(3,4,8,9)],by=c("Website"="Top_Globe"))
Neutural_table_join=left_join(Neutural_table_join,Geo_data,by=c("country"="COUNTRY"))
write.csv(Neutural_table_join,"Neutural_table_join.csv")





## The options for shiny app
choice_of_neutral=Neutural_table$Neutural_website
write.csv(choice_of_neutral,"choice_of_neutral.csv")

choice_of_Posbias=Pos_bias_table$bias_website
write.csv(choice_of_Posbias,"choice_of_Posbias.csv")

choice_of_Negbias=Neg_bias_table$bias_website
write.csv(choice_of_Negbias,"choice_of_Negbias.csv")





##### Visualization of the composition of countries in Global Top 500 sites
Comp_Country=df2
## Get the index of the websites' name that include "Google"
Index1=grep("Google",Comp_Country$Top_Globe)
## Google and its' branches should be United States' 
Comp_Country$countryName_G[Index1]="United States"

## Get the index of websites' name that include "Amazon"
Index2=grep("Amazon",Comp_Country$Top_Globe)
## Amazon and its' branches should be United States'
Comp_Country$countryName_G[Index2]="United States"

## Get the index of websites' name that include "ebay"
Index3=grep("Ebay",Comp_Country$Top_Globe)
Comp_Country$countryName_G[Index3]="United States"

write.csv(Comp_Country,"Comp_Country.csv")



#### Visualization of scatterplot
scatterplot_table=select(Join_table_bias,2,3,4,7)
scatterplot_table$indicator2=500-Join_table_bias$importance1
scatterplot_table=filter(scatterplot_table,countryName_G %in% 
                 c("United States","China","India","Japan","Russia","Indonesia",
                   "Germany","United Kingdom","Turkey"))
scatterplot_table$indicator=jitter(scatterplot_table$indicator)
scatterplot_table$indicator2=jitter(scatterplot_table$indicator2)

##### Visualization of the composition of countries in Global Top 500 sites

## Get the index of the websites' name that include "Google"
Index1=grep("Google",scatterplot_table$Top_Globe)
## Google and its' branches should be United States' 
scatterplot_table$countryName_G[Index1]="United States"

## Get the index of websites' name that include "Amazon"
Index2=grep("Amazon",scatterplot_table$Top_Globe)
## Amazon and its' branches should be United States'
scatterplot_table$countryName_G[Index2]="United States"

## Get the index of websites' name that include "ebay"
Index3=grep("Ebay",scatterplot_table$Top_Globe)
scatterplot_table$countryName_G[Index3]="United States"

write.csv(scatterplot_table,"scatterplot_table.csv")













