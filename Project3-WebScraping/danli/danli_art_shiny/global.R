library('dplyr')
artist <- read.csv('www/artist_final.csv', header = TRUE, stringsAsFactors = FALSE, na.strings=c("","NA"))

# Create a function that switch artist & museums as values and keys
create_idx <- function(key, vals) {
  for(val in vals) {
    if(val %in% names(my_index)) {
      my_index[[val]] <<- unique(c(my_index[[val]], key))
    } else {
      my_index[[val]] <<- key
    }
  }
}

# Apply the function to our dataset, create a list with museums as the keys and artists as the value
my_index <- list()
for(i in 1:nrow(artist)) {
  key = artist$genre[i]
  vals = unlist(strsplit(artist$museums[i], ", "))
  create_idx(key, vals)
}