# Find center point for google map search 
# by using median Longitude and Latitude
find_map_cent <- function (LON, LAT) {
  return (c(median(LON, na.rm = T),
            median(LAT, na.rm = T))
  )
}