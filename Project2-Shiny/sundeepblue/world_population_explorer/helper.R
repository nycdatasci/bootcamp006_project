######################################################
# by Chuan Sun (sundeepblue at gmail dot com)
# https://twitter.com/sundeepblue
# 7/31/2016
# Shiny project @ NYC Data Science Academy
######################################################

library(ggmap)
library(dplyr)
library(plotly)

# ------------------------------------------------------------------------------
# setwd("~/Bootcamp/projects/shiny_project/world_population_explorer/")

# world_ogr is used to render the leaflet map in server.R file
world_ogr <-
    readOGR("data/countries/ne_50m_admin_0_countries",
            'ne_50m_admin_0_countries',
            encoding = 'UTF-8')

# load population data frame
population_df = read.csv("data/population_both_gender.csv", header = T)

# used for the items of drop down widget in ui
world_area_list = c(
    "World level overview",
    
    "Eastern Africa",
    "Middle Africa",
    "Northern Africa",
    "Southern Africa",
    "Western Africa",
    
    "Eastern Asia",
    "Central Asia",
    "Southern Asia",
    "South-Eastern Asia",
    "Western Asia",
    
    "Eastern Europe",
    "Northern Europe",
    "Southern Europe",
    "Western Europe",
    
    "Caribbean",
    "Central America",
    "South America",
    "NORTHERN AMERICA",
    
    "Australia/New Zealand",
    "Melanesia",
    "Micronesia",
    "Polynesia"
)

world_continent_list = c(
    "Entire world",
    "Africa",
    "Asia",
    "Europe",
    "Caribbean",
    "America",
    "Australia/New Zealand",
    "Melanesia",
    "Micronesia",
    "Polynesia"
)

# ------------------------------------------------------------------------------
get_start_and_end_index_of_area = function(area_name) {
    # the number pairs correspond the row index of countries in each area in data frame 'population_df'
    start_and_end_index_of_areas = switch(
        area_name,
        "World level overview" = c(1, 10),
        
        "Eastern Africa" = c(14, 34),
        "Middle Africa" = c(35, 44),
        "Northern Africa" = c(45, 52),
        "Southern Africa" = c(53, 58),
        "Western Africa" = c(59, 76),
        
        "Eastern Asia" = c(78, 86),
        "Central Asia" = c(88, 93),
        "Southern Asia" = c(94, 103),
        "South-Eastern Asia" = c(104, 115),
        "Western Asia" = c(116, 134),
        
        "Eastern Europe" = c(136, 146),
        "Northern Europe" = c(147, 160),
        "Southern Europe" = c(161, 177),
        "Western Europe" = c(178, 187),
        
        "Caribbean" = c(189, 215),
        "Central America" = c(216, 224),
        "South America" = c(225, 239),
        "NORTHERN AMERICA" = c(240, 245),
        
        "Australia/New Zealand" = c(247, 249),
        "Melanesia" = c(250, 255),
        "Micronesia" = c(256, 263),
        "Polynesia" = c(264, 273)
    )
    return(start_and_end_index_of_areas)
}

# ------------------------------------------------------------------------------
# create vectors of country/place names, used for global sphere view in ui.R

eastern_africa = c(
    "Burundi",
    "Comoros",
    "Djibouti",
    "Eritrea",
    "Ethiopia",
    "Kenya",
    "Madagascar",
    "Malawi",
    "Mauritius",
    "Mayotte",
    "Mozambique",
    "Réunion",
    "Rwanda",
    "Seychelles",
    "Somalia",
    "South Sudan",
    "Uganda",
    "United Republic of Tanzania",
    "Zambia",
    "Zimbabwe"
)
middle_africa = c(
    "Angola",
    "Cameroon",
    "Central African Republic",
    "Chad",
    "Congo",
    "Democratic Republic of the Congo",
    "Equatorial Guinea",
    "Gabon",
    "Sao Tome and Principe"
)
northern_africa = c("Algeria",
                    "Egypt",
                    "Libya",
                    "Morocco",
                    "Sudan",
                    "Tunisia",
                    "Western Sahara")
southern_africa = c("Botswana", "Lesotho", "Namibia", "South Africa", "Swaziland")
western_africa = c(
    "Benin",
    "Burkina Faso",
    "Cabo Verde",
    "Côte d'Ivoire",
    "Gambia",
    "Ghana",
    "Guinea",
    "Guinea-Bissau",
    "Liberia",
    "Mali",
    "Mauritania",
    "Niger",
    "Nigeria",
    "Saint Helena",
    "Senegal",
    "Sierra Leone",
    "Togo"
)
africa = c(eastern_africa,
           middle_africa,
           northern_africa,
           southern_africa,
           western_africa)

eastern_asia = c(
    "China",
    "China, Hong Kong SAR",
    "China, Macao SAR",
    "Dem. People's Republic of Korea",
    "Japan",
    "Mongolia",
    "Republic of Korea"
)
central_asia = c("Kazakhstan",
                 "Kyrgyzstan",
                 "Tajikistan",
                 "Turkmenistan",
                 "Uzbekistan")
southern_asia = c(
    "Afghanistan",
    "Bangladesh",
    "Bhutan",
    "India",
    "Iran (Islamic Republic of)",
    "Maldives",
    "Nepal",
    "Pakistan",
    "Sri Lanka"
)
south_eastern_asia = c(
    "Brunei Darussalam",
    "Cambodia",
    "Indonesia",
    "Lao People's Democratic Republic",
    "Malaysia",
    "Myanmar",
    "Philippines",
    "Singapore",
    "Thailand",
    "Timor-Leste",
    "Viet Nam"
)
western_asia = c(
    "Armenia",
    "Azerbaijan",
    "Bahrain",
    "Cyprus",
    "Georgia",
    "Iraq",
    "Israel",
    "Jordan",
    "Kuwait",
    "Lebanon",
    "Oman",
    "Qatar",
    "Saudi Arabia",
    "State of Palestine",
    "Syrian Arab Republic",
    "Turkey",
    "United Arab Emirates",
    "Yemen"
)
asia = c(eastern_asia,
         central_asia,
         southern_asia,
         south_eastern_asia,
         western_asia)

eastern_europe = c(
    "Belarus",
    "Bulgaria",
    "Czech Republic",
    "Hungary",
    "Poland",
    "Republic of Moldova",
    "Romania",
    "Russian Federation",
    "Slovakia",
    "Ukraine"
)
northern_europe = c(
    "Channel Islands",
    "Denmark",
    "Estonia",
    "Faeroe Islands",
    "Finland",
    "Iceland",
    "Ireland",
    "Isle of Man",
    "Latvia",
    "Lithuania",
    "Norway",
    "Sweden",
    "United Kingdom"
)
southern_europe = c(
    "Albania",
    "Andorra",
    "Bosnia and Herzegovina",
    "Croatia",
    "Gibraltar",
    "Greece",
    "Holy See",
    "Italy",
    "Malta",
    "Montenegro",
    "Portugal",
    "San Marino",
    "Serbia",
    "Slovenia",
    "Spain",
    "TFYR Macedonia"
)
western_europe = c(
    "Austria",
    "Belgium",
    "France",
    "Germany",
    "Liechtenstein",
    "Luxembourg",
    "Monaco",
    "Netherlands",
    "Switzerland"
)
europe = c(eastern_europe,
           northern_europe,
           southern_europe,
           western_europe)

caribbean = c(
    "Anguilla",
    "Antigua and Barbuda",
    "Aruba",
    "Bahamas",
    "Barbados",
    "British Virgin Islands",
    "Caribbean Netherlands",
    "Cayman Islands",
    "Cuba",
    "Curaçao",
    "Dominica",
    "Dominican Republic",
    "Grenada",
    "Guadeloupe",
    "Haiti",
    "Jamaica",
    "Martinique",
    "Montserrat",
    "Puerto Rico",
    "Saint Kitts and Nevis",
    "Saint Lucia",
    "Saint Vincent and the Grenadines",
    "Sint Maarten (Dutch part)",
    "Trinidad and Tobago",
    "Turks and Caicos Islands",
    "United States Virgin Islands"
)

central_america = c(
    "Belize",
    "Costa Rica",
    "El Salvador",
    "Guatemala",
    "Honduras",
    "Mexico",
    "Nicaragua",
    "Panama"
)
south_america = c(
    "Argentina",
    "Bolivia (Plurinational State of)",
    "Brazil",
    "Chile",
    "Colombia",
    "Ecuador",
    "Falkland Islands (Malvinas)",
    "French Guiana",
    "Guyana",
    "Paraguay",
    "Peru",
    "Suriname",
    "Uruguay",
    "Venezuela (Bolivarian Republic of)"
)
northern_america = c(
    "Bermuda",
    "Canada",
    "Greenland",
    "Saint Pierre and Miquelon",
    "United States of America"
)
america = c(central_america, south_america, northern_america)

austrialia_newzeland = c("Australia", "New Zealand")

melanesia = c("Fiji",
              "New Caledonia",
              "Papua New Guinea",
              "Solomon Islands",
              "Vanuatu")
micronesia = c(
    "Guam",
    "Kiribati",
    "Marshall Islands",
    "Micronesia (Fed. States of)",
    "Nauru",
    "Northern Mariana Islands",
    "Palau"
)
polynesia = c(
    "American Samoa",
    "Cook Islands",
    "French Polynesia",
    "Niue",
    "Samoa",
    "Tokelau",
    "Tonga",
    "Tuvalu",
    "Wallis and Futuna Islands"
)

all_country_names = c(africa, asia, europe, caribbean, america, austrialia_newzeland, melanesia, micronesia, polynesia)

# create another data frame in which each row corresponds to a country
population_of_countries_df = population_df %>%
    filter(Place %in% all_country_names) %>%
    select(-Variant,-Index)

# below is used to retrieve the longitude and latitude of a country via ggmap, and save to a csv file
# retrieve_and_write_country_location(all_country_names) 

# load the saved country location data frame. The loaded data frame is a 3-column table. 
# column names are: country name, lon, lat. 
# this is used for by threejs global sphere to draw the country location
country_locations = read.csv("data/country_location.csv", sep=",", header=T)

# ------------------------------------------------------------------------------
get_country_names_by_area = function(area) {
    names = switch(
        area,
        "Entire world" = all_country_names,
        "Africa" = africa,
        "Asia" = asia,
        "Europe" = europe,
        "Caribbean" = caribbean,
        "America" = america,
        "Australia/New Zealand" = austrialia_newzeland,
        "Melanesia" = melanesia,
        "Micronesia" = micronesia,
        "Polynesia" = polynesia
    )
    return(names)
}

# convert decimal number to a certain precision. eg: 12.34567 --> 12.34
specify_decimal <- function(x, k) {
    format(round(x, k), nsmall = k)
}

get_population_and_ratio_of_country_in_a_year = function(country_name, year) {
    p = population_of_countries_df %>% select(Place, ends_with(as.character(year)))
    # the first column of p is country names, the second column is population number
    
    total_population_all_countries = sum(p[, 2])
    country_population_in_one_year = p %>% filter(Place == country_name)
    
    population = country_population_in_one_year[2]
    ratio =  population / total_population_all_countries
    ratio = specify_decimal(ratio, 4)
    
    return(c(population, ratio))
}

draw_plotly_population_by_continent = function(start_row_index, end_row_index) {
    world_p = population_df[start_row_index:end_row_index,]
    place_column_index = 3
    population_number_column_index_range = 6:71
    # we only consider all the years between 1950 and 2015
    years = seq(1950, 2015, 1)
    
    # draw first plotly curve. all the rest curves are drawn using "add_trace"
    xformat = list(title = "Years")
    yformat = list(title = "Population (in thousand)")
    pn = world_p[1, place_column_index]
    p = plot_ly(
        x = years,
        y = as.numeric(world_p[1, population_number_column_index_range]),
        mode = "lines",
        name = pn
    )
    
    # draw the rest plotly curves
    for (i in 2:nrow(world_p)) {
        place_name = world_p[i, place_column_index]
        p = p %>% add_trace(
            x = years,
            y = as.numeric(world_p[i, population_number_column_index_range]),
            mode = "lines",
            evaluate = T,
            name = place_name
        ) %>%
            layout(xaxis = xformat, yaxis = yformat)
    }
    p
}

# retrieve the longitude and latitude of a country via ggmap, and save to a csv file
retrieve_and_write_country_location = function(country_names) {
    lon = c()
    lat = c()
    for (i in 1 : length(country_names)) {
        loc = geocode(country_names[i])
        lon = c(lon, as.numeric(loc[1]))
        lat = c(lat, as.numeric(loc[2]))
    }
    
    country_location = data.frame(country_names, lon, lat)
    write.table(country_location, "country_location.csv", sep=",", row.names=F)
}
