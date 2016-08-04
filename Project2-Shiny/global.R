library(plotly)

# Read in ratio tables
ratio_data <- list()
ratio_data_nohover <- list()
for(ratio in c("crop", "forest", "grass", "special", "urban", "misc")) {
  # print(ratio)
  var <- read.csv(paste0("https://raw.githubusercontent.com/fangbin08/usda/master/",
                         ratio, "_ratio.csv"))
  state_name = sort(as.character(var$state))
  regions = var$regions
  var_nohover <- var  
  var$hover <- with(var, paste(state))
  ratio_data[[ratio]] <- var
  ratio_data_nohover[[ratio]] <- var_nohover
  var
  # print(ratio_data[[ratio]])
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
ratio_acre_nohover <- list()
for(acre in c("crop", "forest", "grass", "special", "urban", "misc")) {
  
  var_acre <- read.csv(paste0("https://raw.githubusercontent.com/fangbin08/usda/master/",
                              acre, ".csv"))
  var_acre_nohover <- var_acre  
  var_acre$hover <- with(var_acre, paste(state))
  acre_data[[acre]] <- var_acre
  ratio_acre_nohover[[acre]] <- var_acre_nohover
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
landtypes_name <- c("Cropland", "Grassland pasture and range", "Forest-use land",
"Special uses", "Urban area", "Other or miscellaneous")
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
# Read and generate spreadsheet by state
value_list = {}
for(ratio in c("crop", "forest", "grass", "special", "urban", "misc")) {
  var_state <- read.csv(paste0("https://raw.githubusercontent.com/fangbin08/usda/master/",
                               ratio, "_ratio.csv"))
  value_list <- c(value_list, var_state[1:61, 4:17])
}
value_list = data.frame(value_list)
value_list = t(value_list)

type_list = c("crop", "forest", "grass", "special", "urban", "misc")

list1 = as.list(rep("crop", 14))
list2 = as.list(rep("forest", 14))
list3 = as.list(rep("grass", 14))
list4 = as.list(rep("special", 14))
list5 = as.list(rep("urban", 14))
list6 = as.list(rep("misc", 14))

type_list_long = cbind(list1, list2, list3, list4, list5, list6)
type_list_long = data.frame(unlist(type_list_long))
year_list = colnames(var)[4:17]


var_temp = read.csv("https://raw.githubusercontent.com/fangbin08/usda/master/crop_ratio.csv", 
                    fileEncoding="UTF-8-BOM")
state_list = as.character(var_temp[1:61, 2])
year_list_long = rep(year_list, 6)
value_list = cbind(year_list_long, type_list_long, value_list)
a = as.character(state_list)
colnames(value_list) = c(as.character("year"), as.character("type"), 
                         state_list)
rownames(value_list) = NULL

