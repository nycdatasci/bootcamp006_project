library(shiny)
library(leaflet)
library(rjson)
library(tools)

# turn warning msg off
# options(warn = -1)
# options(warn = 0) #to turn warning back on

museum = read.csv("./data/tripadvisor_merged.csv", stringsAsFactors = FALSE)[, -1]
# truncate unnecessary columns in Shiny 
# museum = museum[, -c(:223)]

pal = colorFactor( c('#e78ac3', '#a6d854','#fc8d62', '#ffd92f','#66c2a5', '#8da0cb'), museum$Rating)

type_data = fromJSON(paste(readLines("./data/museum_types.json"), collapse=""))
link_data = fromJSON(paste(readLines("./data/museum_img_link.json"), collapse=""))
cat_data = fromJSON(paste(readLines("./data/clean_category.json"), collapse=""))
tag_data = fromJSON(paste(readLines("./data/tags_cloud.json"), collapse=""))

get_common_tag = function(data, selected_m, suggested_m){
    # get tags or types in common
    common = c()
    for (m in selected_m){
        common = append(common, intersect(unlist(data[m]), unlist(data[suggested_m])))
    }
    common = unique(common[!common %in% "on display"])
    common = sapply(common, toTitleCase)
    return (common)
}

