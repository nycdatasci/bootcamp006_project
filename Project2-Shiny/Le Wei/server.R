library(maps)
library(ggplot2)
library(dplyr)
library(shiny)
load('my_data.RData')

region_list <- list()
region_list[["Asia"]] <- c("China","North Korea","Japan","Mongolia","South Korea",
                           "Taiwan","Afghanistan","Bangladesh","Bhutan","India",
                           "Iran","Kazakhstan","Kyrgyzstan","Maldives","Nepal",
                           "Pakistan","Sri Lanka","Tajikistan","Turkmenistan",
                           "Uzbekistan","Brunei Darussalam","Cambodia","Indonesia",
                           "Laos","Malaysia","Myanmar","Philippines","Singapore",
                           "Thailand","Timor-Leste","Vietnam","Armenia","Azerbaijan",
                           "Bahrain","Cyprus","Georgia","Iraq","Israel","Jordan",
                           "Kuwait","Lebanon","Oman","Palestine")

region_list[["Africa"]] <- c("Burundi","Comoros","Djibouti","Eritrea","Ethiopia",
                             "Kenya","Madagascar","Malawi","Mauritius","Mayotte",
                             "Mozambique","Rwanda","Seychelles","Somalia","South Sudan",
                             "Uganda","Tanzania","Zambia","Zimbabwe","Angola","Cameroon",
                             "Central African Republic","Chad","Congo","Democratic Republic of the Congo",
                             "Equatorial Guinea","Gabon","Sao Tome and Principe","Algeria",
                             "Egypt","Morocco","Sudan","Tunisia","Western Sahara","Botswana",
                             "Lesotho","Namibia","South Africa","Swaziland","Benin","Burkina Faso",
                             "Cape Verde","Ivory Coast","Gambia","Ghana","Guinea","Guinea-Bissau",
                             "Liberia Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leone","Togo")

region_list[["Europe"]] <- c("Belarus","Bulgaria","Czech Republic","Hungary","Poland",
                             "Republic of Moldova","Romania","Russia","Slovakia","Ukraine",
                             "Channel Islands","Denmark","Estonia","Finland","Iceland",
                             "Latvia","Lithuania","Norway","Sweden","UK","Albania",
                             "Bosnia and Herzegovina","Croatia","Greece","Italy","Malta",
                             "Montenegro","Portugal","Serbia","Slovenia","Spain","TFYR Macedonia",
                             "Austria","Belgium","France","Germany","Luxembourg","Netherlands",
                             "Switzerland")

region_list[["NorthAmerica"]] <- c("USA", "Canada")

shinyServer(
    function(input, output) {
        
        filtered <- reactive({
            if(input$region == "World")
                return(WWW)
            else {
                return(WWW %>% filter(region %in% region_list[[input$region]]))
            }
        })
        output$map <- renderPlot({
          title = paste0(input$type, "(", input$region, ")")
          code = paste0('g = ggplot(filtered()) + geom_polygon(aes(x=long, y=lat, group=group, fill=', 
                        input$type,
                        ')) +ggtitle(title)')
          eval(parse(text = code))
          return(g)
          })
        })
        
        