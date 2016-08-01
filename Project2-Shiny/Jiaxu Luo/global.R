
## load the dataset "Fortune" 
## create a variable for ui sliderinput of industries
Fortune=read.csv("Fortune.csv",stringsAsFactors = F)
choice_of_industry=c(unique(Fortune$Sector))

## Fun_Map function returns the data.frame to package "googleVis" to plot the Map
Fuc_Map=function (a,b,c){
        library(dplyr)
        mm=filter(Fortune,!is.na(Market.value..m))  %>%
                filter(Year==b) %>%
                filter(Sector==c) %>%
                group_by(Country) %>%
                summarise(Count=n())
        return(mm)
}

## Fuc_Bubble function returns the data.frame to package "bubbles" to plot the Bubbles
Fuc_Bubble=function (a,b,c){
        library(dplyr)
        mm=filter(Fortune,!is.na(Market.value..m))  %>%
                filter(Year==b) %>%
                filter(Sector==c) 
        return(mm)
}

## Fuc_Treemap function returns the data.frame to package"d3treeR" to plot the Treemap
Fuc_Treemap=function(a,b,c){
        library(dplyr)
        mm=filter(Fortune,!is.na(Market.value..m)) %>%
                filter(Year==b)
        tm=treemap(
                mm,
                index=c("Sector","Country"),
                vSize="Market.value..m",
                vColor="Market.value..m",
                type="value"
        )
        return(tm)
}

## Fuc_Hist function returns the data.frame to package"googleVis" to plot the histogram
Fuc_Hist=function(a,b,c){
        library(dplyr)
        Fortune[,"P.e.ratio"]=as.numeric(Fortune[,"P.e.ratio"])
        mm=filter(Fortune,!is.na(P.e.ratio)) %>%
                filter(Year==b) %>%
                filter(Sector==c) %>%
                select(Company,P.e.ratio) %>%
                arrange(desc(P.e.ratio)) %>%
                head(8)
        return(mm)
}