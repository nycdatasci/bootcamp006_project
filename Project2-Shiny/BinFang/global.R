
# Read in ratio tables
ratio_data <- list()

for(ratio in c("crop", "forest", "grass", "special", "urban", "misc")) {
  # print(ratio)
  var <- read.csv(paste0("https://raw.githubusercontent.com/fangbin08/usda/master/",
                         ratio, "_ratio.csv"))
  var$hover <- with(var, paste(state))
  ratio_data[[ratio]] <- var 
  for (col_year in colnames(var)[4:17]) {# Draw ranking plot
    # print(col_year)
    ranking = data.frame(as.character(var$code[1:50]),
                         var[col_year][1:50,])
    names(ranking) <- c("State", "Percentage")
    ranking_sort <- ranking[order(-ranking$Percentage),] 
  }
}

# Read in acre tables
acre_data <- list()

for(acre in c("crop", "forest", "grass", "special", "urban", "misc")) {
  
  var_acre <- read.csv(paste0("https://raw.githubusercontent.com/fangbin08/usda/master/",
                              acre, ".csv"))
  var_acre$hover <- with(var_acre, paste(state))
  acre_data[[acre]] <- var_acre 
}


# Draw Map

l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

 landtypes <- names(ratio_data)
# landtypes <- c("Cropland", "Grassland pasture and range", "Forest-use land",
               # "Special uses", "Urban area", "Other or miscellaneous land uses")
years <- colnames(var)[4:17]
codes <- var$code
palettes <- c("YlGnBu", "Greens", "Blues", "Purples", "OrRd", "Greys")
names(palettes) <- landtypes

#-------------------------------------------------------------------------------------
#Draw time series plot
#Define a function to transpose table
t_transpose <- function(var, var_trans) {
  var_trans = var[51:61, 4:17]
  cname = as.character(var[51:61, 2])
  var_trans = t(var_trans)
  rname = as.character(c("1945", "1949", "1954", "1959", "1964", "1969", 
                         "1974", "1978", "1982", "1987", "1992" ,"1997",
                         "2002", "2007"))
  var_trans = cbind(rname, (var_trans))
  rownames(var_trans) = c(1:14)
  colnames(var_trans) = c(as.character("year"), cname)
  var_trans = data.frame(var_trans)
  var_trans[, 1:12] = lapply(var_trans[, 1:12], as.character)
  var_trans[, 2:12] = lapply(var_trans[, 2:12], as.numeric)
  return(var_trans)
}


#-------------------------------------------------------------------------------------
#Plot scatter of the top 3 land cover types
acre_data_top3 <- list()

for(acre in c("crop", "forest", "grass")) {
  var_acre_top3 <- read.csv(paste0("https://raw.githubusercontent.com/fangbin08/usda/master/",
                                   acre, "_ratio.csv"), nrows = 50)
  acre_data_top3[[acre]] <- var_acre_top3
}

#-------------------------------------------------------------------------------------
# Text for "About data" tab
intro_text1 = paste0("The Land cover/use Data was downloaded from USDA-ERS (United States Department",
                   "of Agriculture, Economic Research Service) data products center website at: ",
                   "http://www.ers.usda.gov/data-products/major-land-uses.aspx",
                   "The ERS has been dedicating on providing major land use estimates in the United States for over",
                   "50 years, collected from 14 Major Land Uses reports by region and state from 1945 to 2007.",
                   "14 Major Land Uses reports by region and state from 1945 to 2007.", sep = "\n")
                  
intro_text2 = paste("The R packages used in this app include:",
                     " shinydashboard,",
                     " ggplot2,",
                     " plotly,",
                     " dplyr.",
                     " Source code could be downloaded from the following Github repository:",
                     " https://github.com/fangbin08/usda",
                      sep = "\n")

