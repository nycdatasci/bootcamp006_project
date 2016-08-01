library(shiny)
library(shinydashboard)
library(DT)
library(reshape2)
library(googleVis)
library(ggplot2)
if (!require(bubbles)) install.packages('bubbles')
library(bubbles)
if (!require(fields)) install.packages('fields')
library(fields)
if (!require(corrplot)) install.packages('corrplot')
library(corrplot)
library(dplyr)

suppressPackageStartupMessages(library(googleVis))
# convert matrix to dataframe

load('./test.RData')
load('./sectorData.RData')
load('./sectorGrowthData.RData')
load('./byYear.RData')
data = list()
data$'personal income per capita' <- income.perCapita
data$'population' <- population
noRegions<-c('personal income per capita','population')
usRegions<-unique(GDP_regions[['per capita real GDP']]$GeoName)
usRegion2Abbr<-c('US','NE','ME','GL','PL','SE','SW','RM','FW')
names(usRegion2Abbr)<-usRegions
heatXChoices<-c('states','US regions','sectors')
heatYChoices<-c('years','sectors')
heatRelative<-c('absolute','relative')
support_byYear<-c("nominal GDP","real GDP","nominal GDP growth","real GDP growth")
rela_abso<-"Absolute or Relative:"
ts_cs<-"Correlation Type"
US_GDP_Visual<-'US GDP Data Visualization'


sectorsAbbreviations=c("Agriculture", "Mining", "Util", "Construt.",                                    
"Manufac.", "Wholesale", "Retail", "Trans", "IT",                                     
"Finance", "R estate", "Profess.", "Manage", "waste",    
"Edu.", "Health care", "entertain.", "food & hotel",                 
"Other Xgov.", "Govern.")

names(sectorsAbbreviations) <- sectors
names(sectors) <- sectorsAbbreviations

SectorAbbre<-function(x) { return(sectorsAbbreviations[x]) }  

SectorAbbre<-Vectorize(SectorAbbre)

choice <- c(c('personal income per capita','population'),names(GDP_states))
for (key in names(GDP_states)) {  data[[key]]<-GDP_states[[key]] } 

states <- postCodes$US.State.

ConvertNum<-function(num) {
  
  if (!is.numeric(num)) {return(num)}
  if (abs(num)<=1) {return(paste0(floor(num*1000)/10,'%'))}
  else if (abs(num)<1000 & abs(num)>1) {return(as.character(num))}
  else if (abs(num) < 1e5) {
    x = floor(num/1e2)/1e1
    return(paste0(x,'K'))}
  else if (abs(num) < 1e8) {
    x = floor(num/1e4)/1e2
    return(paste0(x,'M'))}
  else if (abs(num) < 1e10) {
    x = floor(num/1e7)/1e2
    return(paste0(x,'B')) }
  else {
    x = floor(num/1e10)/1e2
    return(paste0(x,'T')) }  
}

ConvertNum<-Vectorize(ConvertNum)

FindStateAbbreviation<-function(name) {

  z<-filter(postCodes, US.State.==name)
  if (nrow(z)<1) {return(name)}
  else {return(z$Abbreviation.)}
}

FindStateAbbreviation<-Vectorize(FindStateAbbreviation)

FindRegionAbbreviation<-function(name)  { usRegion2Abbr[[name]] }  

FindRegionAbbreviation<-Vectorize(FindRegionAbbreviation)

NormalizeDFAlongRows<-function(DF) {
  
  keys<-rownames(DF)
  names<-names(DF)
  is.numeric.col<-sapply(DF,is.numeric)
  numericCols<-names[is.numeric.col]
  DF2<-DF[,numericCols]
  
  for (key in keys) DF2[key,]<-DF2[key,]/(sum(DF2[key,],na.rm=T)+1e-10)
  DF[,numericCols]<-DF2[,numericCols]
  return(DF) 
}

NormalizeDFAlongColumns<-function(DF) {
  
  names<-names(DF)
  is.numeric.col<-sapply(DF,is.numeric)
  for (x in names[is.numeric.col]) DF[[x]]<-DF[[x]]/(sum(DF[[x]],na.rm=T)+1e-10)
  
  DF
}

TreatAsMissing<-function(DF,bound=1e16){
  
  names<-names(DF)
  is.numeric.col<-sapply(DF,is.numeric)
  
  for (x in names[is.numeric.col]) {
    bad <- abs(DF[[x]])>bound
    DF[[x]][bad] <- NA
  }
  return(DF)
}

StackDFs<-function(myList,newColName) {
  
  myNames<-names(myList)
  DF<-as.data.frame(matrix(nrow=0,ncol=length(myNames)))
  # a list of data frame of the same dim (1,n)
  for (key in myNames) {
    x <- myList[[key]]
    DF<-rbind(DF,x)
  }

  DF[[1]]<-myNames
  S      <-names(DF)
  S[1]   <-newColName
  
  return(DF)
}