data1 <- read.csv("F:/scraping/data1.csv")
View(data1)
output <- read.csv("F:/scraping/output.csv")
View(output)

data = merge(data1, output, by = "NAME")
View(data)


hist(data1$RATING)




#load up the ggmap library
library(ggmap)
infile <- "input"


# get the address list, and append "Ireland" to the end to increase accuracy 
# (change or remove this if your address already include a country etc.)
addresses = data1$ADDRESS
addresses = paste0(addresses, ", ")

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

#now we add the latitude and longitude to the main data
data1$lat <- geocoded$lat
data1$long <- geocoded$long
data1$accuracy <- geocoded$accuracy

#finally write it all to the output files
saveRDS(data, paste0("../data/", infile ,"_geocoded.rds"))
write.table(data, file=paste0("../data/", infile ,"_geocoded.csv"), sep=",", row.names=FALSE)



library(googleVis)
AndrewMap <- gvisMap(df, df1$lat, df1$long,
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)


library(ggplot2)
ggplot(data1, aes(data1$RATING)) + geom_histogram()
ggplot(df1, aes(df1$RATING)) + geom_histogram() + xlab("Rating") + ggtitle("Distribution Of Restaurant Rating")
library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, df1$long, df1$lat, popup = df1$NAME)
m







