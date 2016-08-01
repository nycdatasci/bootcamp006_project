#### PACKAGES #####

library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)
library(car)
library(RColorBrewer)
library(reshape2)
library(shinyRGL)
library(stargazer)

#### STARTING DATA ####

var = c("bach_or_more", "veterans",  "percapita_income",  "hh_med_income", "fraction_votes", "over65", "white", "black", "hispanic", "asian", "female", "below_poverty")
names(var) = c("% Bachelors", "Total Veterans", "Per Capita Income", "Household Median Income", "Fraction of Votes", "% Over 65", "% White", " % Black", " % Hispanic", "% Asian", " % Female", "% Below Poverty")

pres = read.csv("pres.csv")
county = read.csv("county_facts.csv")
county = county[,-3]
usa.states <- readOGR(dsn = "/Users/jdejager/Desktop/RDocs/sex", layer = "states", verbose = FALSE)
options(digits = 10 , scipen = 10)
repdem = read.csv("repdem.csv")
dem = read.csv('dem.csv')
rep = read.csv('rep.csv')
var = c("bach_or_more", "veterans",  "percapita_income",  "hh_med_income", "fraction_votes", "over65", "white", "black", "hispanic", "asian", "female", "below_poverty")
names(var) = c("% Bachelors", "Total Veterans", "Per Capita Income", "Household Median Income", "Fraction of Votes", "% Over 65", "% White", " % Black", " % Hispanic", "% Asian", " % Female", "% Below Poverty")

###STATE LEVEL DATA #####
county = county %>% 
  group_by(state_abbreviation) %>% 
  summarise_each(funs(mean))

##### BIPARTY SIMPLE MULTIPLE REGRESSION #####
bipartyfit = lm(fraction_votes ~ 0 + hh_med_income + bach_or_more + party, data = repdem)

###### REPUBLICAN SIMPLE MULTIPLE REGRESSION ######

repfit = lm(fraction_votes ~ 0 + hh_med_income + bach_or_more + candidate, data = rep)

###### DEMOCRAT SIMPLE MULTIPLE REGRESSION ######

demfit = lm(fraction_votes ~ 0 + hh_med_income + bach_or_more + candidate + hs_or_more + hispanic, data = dem)

############# CANDIDATE DATA ###############

##### BERNIE #####
bern = pres[pres$candidate == "Bernie Sanders",]
bern = bern[, -c(1,3)]
bern = bern %>% 
  group_by(state_abbreviation, party, candidate) %>% 
  summarise_each(funs(mean))
bern.merge = merge(bern,county, by = "state_abbreviation")
usa.states.bern = usa.states
usa.states.bern@data = data.frame(usa.states@data, bern.merge[match(usa.states.bern@data[,"STATE_ABBR"], bern.merge[,"state_abbreviation"]),])
usa.states.bern@data = na.omit(usa.states.bern@data)

#### HILLARY #####
hillary = pres[pres$candidate == "Hillary Clinton",]
hillary = hillary[, -c(1,3)]
hillary = hillary %>% 
  group_by(state_abbreviation, party, candidate) %>% 
  summarise_each(funs(mean))
hillary.merge = merge(hillary,county, by = "state_abbreviation")
usa.states.hillary = usa.states
usa.states.hillary@data = data.frame(usa.states@data, hillary.merge[match(usa.states.hillary@data[,"STATE_ABBR"], hillary.merge[,"state_abbreviation"]),])
usa.states.hillary@data = na.omit(usa.states.hillary@data)

##### DONALD #####
donald = pres[pres$candidate == "Donald Trump",]
donald = donald[, -c(1,3)]
donald = donald %>% 
  group_by(state_abbreviation, party, candidate) %>% 
  summarise_each(funs(mean))
donald.merge = merge(donald,county, by = "state_abbreviation")
usa.states.donald = usa.states
usa.states.donald@data = data.frame(usa.states@data, donald.merge[match(usa.states.donald@data[,"STATE_ABBR"], donald.merge[,"state_abbreviation"]),])
usa.states.donald@data = na.omit(usa.states.donald@data)

#### TED CRUZ ####
tedcruz = pres[pres$candidate == "Ted Cruz",]
tedcruz = tedcruz[,-c(1,3)]
tedcruz = tedcruz %>% 
  group_by(state_abbreviation, party, candidate) %>% 
  summarise_each(funs(mean))
tedcruz.merge = merge(tedcruz,county, by = "state_abbreviation")
usa.states.tedcruz = usa.states
usa.states.tedcruz@data = data.frame(usa.states@data, tedcruz.merge[match(usa.states.tedcruz@data[,"STATE_ABBR"], tedcruz.merge[,"state_abbreviation"]),])
usa.states.tedcruz@data = na.omit(usa.states.tedcruz@data)

#### JOHN KASICH #####
kasich = pres[pres$candidate == "John Kasich",]
kasich = kasich[, -c(1,3)]
kasich = kasich %>% 
  group_by(state_abbreviation, party, candidate) %>% 
  summarise_each(funs(mean))
kasich.merge = merge(kasich,county, by = "state_abbreviation")
usa.states.kasich = usa.states
usa.states.kasich@data = data.frame(usa.states@data, kasich.merge[match(usa.states.kasich@data[,"STATE_ABBR"], kasich.merge[,"state_abbreviation"]),])
usa.states.kasich@data = na.omit(usa.states.kasich@data)

##### By Candidate Regressions ####
bernfit = lm(fraction_votes ~ female + white + hispanic + bach_or_more + home_owner_rate + hh_med_income + over65 + black + asian + below_poverty + percapita_income, data = bern.merge)
hilfit = lm(fraction_votes ~ female + white + hispanic + bach_or_more + home_owner_rate + hh_med_income + over65 + black + asian + below_poverty + percapita_income, data = hillary.merge)
donaldfit = lm(fraction_votes ~ female + white + hispanic + bach_or_more + home_owner_rate + hh_med_income + over65 + black + asian + below_poverty + percapita_income, data = donald.merge)
tedfit = lm(fraction_votes ~ female + white + hispanic + bach_or_more + home_owner_rate + hh_med_income + over65 + black + asian + below_poverty + percapita_income, data = tedcruz.merge)
kasichfit = lm(fraction_votes ~ female + white + hispanic + bach_or_more + home_owner_rate + hh_med_income + over65 + black + asian + below_poverty + percapita_income, data = kasich.merge)

########### UI & SERVER ##################

#### UI #####

ui = navbarPage("2016 Primary Results",
                
          tabPanel("Introduction",
                            h1("Analysis of the 2016 Presidential Primaries"),
                   br(),
                   br(),
                  
                   
                   img(src = "http://image.syracuse.com/home/syr-media/width620/img/politics_impact/photo/20162550-mmmain.jpg"),
                   
                   br(),
            

                            br(),
                            h1("Table Of Contents"),
                   br(),
                            h4("1. Spatial Visualization of Bernie Sanders Primary Results"),
                   
                   br(),
                            h4("2. Spatial Visualization of Hillary Clinton Primary Results"),
                   br(),
                            h4("3. Spatial Visualization of Donald Trump Primary Results"),
                   br(),
                            h4("4. Spatial Visualization of Ted Cruz Primary Results"),
                   br(),
                            h4("5. Spatial Visualization of John Kasich Primary Results"),
                   br(),
                            h4("6. Regression Analysis"),
                   br(),
                            h4("7. 3D Visualization of How Education and Income Affected Voting Resutls for eac Candidate"),
                   br(),
                   br(),
                   
                   h4("Data Source"),
                   p("Source: ",a("Primary Results from CNN | Compiled by Ben Hammer",href=
                                    "https://www.kaggle.com/benhamner/2016-us-election")),
                   
                   p("Description: ","Demographic data, as well as vote results for each state")),

         tabPanel("Bernie Map",
                  tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                  leafletOutput("mapbern", height = 1400),
                  absolutePanel(top = 100, right = 20,
                                selectInput("xcol", "Variables", var)
                                )
                  ),
                
                
                  tabPanel("Hillary Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("maphillary", height = 1400),
                         absolutePanel(top = 100, right = 20,
                         #sidebarPanel(top = 1, right = 3,
                                      selectInput("xcol1", "Variables", var))),
                
                tabPanel("Donald Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("mapdonald", height = 1400),
                         absolutePanel(top = 100, right = 20,
                         #sidebarPanel(top = 1, right = 3,
                                      selectInput("xcol2", "Variables", var))),
                
                tabPanel("Ted Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("mapted", height = 1400),
                         absolutePanel(top = 100, right = 20,
                         #sidebarPanel(top = 1, right = 3,
                                      selectInput("xcol3", "Variables", var))),
                
                tabPanel("Kasich Map",
                         tags$style(type = "text/css", "#map {height: calc(200vh - 100px) !important;}"),
                         leafletOutput("mapkasich", height = 1400),
                         absolutePanel(top = 100, right = 20,
                         #sidebarPanel(top = 1, right = 3,
                                      selectInput("xcol4", "Variables", var))),
                
                
                
                tabPanel("Regressions",
                         sidebarLayout(
                             sidebarPanel(top = 10, right = 2,
                                      selectInput("dataset2",label = "Select a dataset", choices = c("Donald"= "donald.merge", 
                                                                                                    "Bernie" = "bernie.merge",
                                                                                                    "Ted Cruz" = "tedcruz.merge",
                                                                                                    "Kasich" = "kasich.merge",
                                                                                                   "Hillary" = "hillary.merge"))),
                        mainPanel(
                                                    
                             tabsetPanel(
                                                              
                                      tabPanel("Models",      
                                        helpText("The Response variable is:"),
                                         verbatimTextOutput("responseVar2"),
                                           br(),
                                            helpText("The current model is:"),
                                        
                                             verbatimTextOutput("modelEQ2")))))),
    
                                                  
  
        tabPanel("3D Visualizer",                 
         
         
         
         p("When creating a model, it can be very helpful to visualize both the data and the model.
           Often we wish to create a prediction model for a response variable on more than one predictors. 
           In the case of a single response and two predictors, we must use a third dimension to visualize the 
           the data and model."),
         p("In this app, you will be able to  visualize the data and explore the effectiveness of different models
           for a numerical response variable. "),
         
         sidebarLayout(
           
           
           
           
           sidebarPanel(top = 10, right = 2, width = 2,
             
             selectInput("dataset",label = "Select a dataset", choices = c("Republican"= "rep", 
                                                                           "Democrats" = "dem",
                                                                           "Both Parties" = "repdem"
                                                                           
                                                                           
             )),
             
             ##To do the 2D this you need to uncomment the end and add the third option
             conditionalPanel(condition = "input.dataset == 'rep'",
                              radioButtons("expTypes1", label = "Fraction of Votes = ", 
                                           choices = list("Bachelor % + Median Income + Candidate" = 1), 
                                                        
                                           selected = 1)),
             
             conditionalPanel(condition = "input.dataset == 'dem'",
                              radioButtons("expTypes2", label = "Fraction of Votes = ", 
                                           choices = list("Bachelor % + Median Income + Candidate" = 1), 
                                           selected = 1)),
             
             conditionalPanel(condition = "input.dataset == 'repdem'",
                              radioButtons("expTypes3", label = "Fraction of Votes = ", 
                                           choices = list("Bachelor % + Median Income + Candidate + Party" = 1), 

                                           selected = 1))
             
             
             
           ),
           mainPanel(webGLOutput("troisPlot",width="1100px",height="1100px"))
               
             
             
             
           )
         )#
         )

#### SERVER ####            
                
server = function(input, output){
  

  output$mapbern <- renderLeaflet({
    qpal <- colorQuantile("Blues", usa.states.bern@data[,input$xcol], n = 10)
    state_popup <- paste(usa.states.bern@data[,input$xcol])
    leaflet(usa.states.bern) %>% addTiles()  %>%  
      addPolygons(stroke = F, fillOpacity = 0.7, popup = ~state_popup ,color = ~qpal(usa.states.bern@data[, input$xcol])) %>%
      setView(lng = -95, lat = 37., zoom = 5)  %>% addLegend(position = "topleft",
                                                             pal = qpal, values = ~usa.states.bern@data[,input$xcol1])
    
  })
  
  output$maphillary <- renderLeaflet({
    qpal <- colorQuantile("Blues", usa.states.hillary@data[,input$xcol1], n = 10)
    state_popup1 <- paste(usa.states.hillary@data[,input$xcol1])
    leaflet(usa.states.hillary) %>% addTiles()  %>%  
      addPolygons(stroke = F, fillOpacity = 0.7, popup = ~state_popup1, color = ~qpal(usa.states.hillary@data[, input$xcol1])) %>%
      setView(lng = -95, lat = 37., zoom = 5) %>% addLegend(position = "topleft",
                                                            pal = qpal, values = ~usa.states.hillary@data[,input$xcol1])
    
  })
  
  output$mapdonald <- renderLeaflet({
    qpal <- colorQuantile("OrRd", usa.states.donald@data[,input$xcol2], n = 10)
    state_popup2 <- paste(usa.states.donald@data[,input$xcol2])
    leaflet(usa.states.donald) %>% addTiles()  %>%  
      addPolygons(stroke = F, fillOpacity = 0.7, popup = ~state_popup2, color = ~qpal(usa.states.donald@data[, input$xcol2])) %>%
      setView(lng = -95, lat = 37., zoom = 5) %>% addLegend(position = "topleft",
                                                            pal = qpal, values = ~usa.states.donald@data[,input$xcol1])
    
  })
  
  output$mapted <- renderLeaflet({
    qpal <- colorQuantile("OrRd", usa.states.tedcruz@data[,input$xcol3], n = 10)
    state_popup3 <- paste(usa.states.tedcruz@data[,input$xcol2])
    leaflet(usa.states.tedcruz) %>% addTiles()  %>%  
      addPolygons(stroke = F, fillOpacity = 0.7, popup = ~state_popup3, color = ~qpal(usa.states.tedcruz@data[, input$xcol3])) %>%
      setView(lng = -95, lat = 37., zoom = 5)  %>% addLegend(position = "topleft",
                                                             pal = qpal, values = ~usa.states.tedcruz@data[,input$xcol1])
    
  })
  
  output$mapkasich <- renderLeaflet({
    qpal <- colorQuantile("OrRd", usa.states.kasich@data[,input$xcol4], n = 10)
    state_popup4 <- paste(usa.states.kasich@data[,input$xcol2])
    leaflet(usa.states.kasich) %>% addTiles()  %>%  
      addPolygons(stroke = F, fillOpacity = 0.7, popup = ~state_popup4,color = ~qpal(usa.states.kasich@data[, input$xcol4])) %>%
      setView(lng = -95, lat = 37., zoom = 5) %>% addLegend(position = "topleft",
                                                            pal = qpal, values = ~usa.states.kasich@data[,input$xcol1])
    
  })
  
  output$troisPlot <- renderWebGL({
    
    if (input$dataset == "rep")
    {
      if (input$expTypes1 == 1){
        #         par3d(scale=c(1,1,0.2),cex=.6)
        #         points3d(dfiris$Sepal.Length,dfiris$Sepal.Width,dfiris$Petal.Area)
        #         axes3d()
        #         title3d(xlab="Sepal Length",ylab="Sepal Width",zlab="Petal Area")
        scatter3d(rep$hh_med_income, rep$fraction_votes, rep$bach_or_more, groups = as.factor(rep$candidate),surface=FALSE, ellipsoid = TRUE, grid = FALSE, fit = "smooth", ylab = "% of Votes", zlab = "Bacelors", xlab = "Median Inc") 
        
      }
      
    }else if (input$dataset == "dem")
    {
      if (input$expTypes2 == 1){
        #         par3d(scale=c(0.02,1,0.2),cex=.5)
        #         points3d(dfcars$hp,dfcars$wt,dfcars$mpg)
        #         axes3d()
        #         title3d(xlab="Gross Horsepower",ylab="Weight (lb/1000)",zlab="Miles / (US) Gallon")
        scatter3d(dem$hh_med_income, dem$fraction_votes, dem$bach_or_more, groups = as.factor(dem$candidate), surface=FALSE, ellipsoid = TRUE, grid = FALSE, ylab = "% of Votes", zlab = "Bacelors", xlab = "Median Inc") 
      }
      
      
    } else if (input$dataset == "repdem"){
      if (input$expTypes3 == 1){
        #         par3d(scale=c(1,.5,2),cex=.5)
        #         points3d(states$Murder,states$HSGrad,states$LifeExp)
        #         axes3d()
        #         title3d(xlab="Murders per 100,000",ylab="Precent High-School Graduates",zlab="Life Expectancy")
        scatter3d(repdem$hh_med_income, repdem$fraction_votes, repdem$bach_or_more, groups = as.factor(repdem$party),surface=FALSE, ellipsoid = TRUE, grid = FALSE, ylab = "% of Votes", zlab = "Bacelors", xlab = "Median Inc")
      } 
      
    }     
  })
  
  output$responseVar <- renderPrint({
    if (input$dataset == "rep"){
      paste("Fraction Votes for Republicans")
    }else if (input$dataset == "dem"){
      paste("Fraction Votes for Democrat")
    }else if (input$dataset == "repdem"){
      paste("Fraction Votes for Party")
    }
  })
  
  output$responseVar2 <- renderPrint({
    if (input$dataset2 == "donald.merge"){
      paste("Fraction Votes for Republicans")
    }else if (input$dataset2 == "bernie.merge"){
      paste("Fraction Votes for Democrat")
    }else if (input$dataset2 == "tedcruz.merge"){
      paste("Fraction Votes for Party")
    }else if (input$dataset2 == "kasich.merge"){
      paste("Fraction Votes for Party")
    }else if (input$dataset2 == "hillary.merge"){
      paste("Fraction Votes for Party")
    }
  })
  
  output$modelEQ2 <- renderPrint({
    
    if (input$dataset2 == "donald.merge"){
        #summary(donaldfit)$coefficients
        stargazer(donaldfit, type = "text", covariate.labels=c("% Females","% White", "% Hispanic","% With Bachelor's or More","Home Owner Rate","Household Median Income", "% Over 65", "% Black", "% Asian", "% Below Poverty", "Per Capita Income"))

      
    }else if (input$dataset2 == "bernie.merge"){
        summary(bernfit)$coefficients
        stargazer(bernfit, type = "text", covariate.labels=c("% Females","% White", "% Hispanic","% With Bachelor's or More","Home Owner Rate","Household Median Income", "% Over 65", "% Black", "% Asian", "% Below Poverty" , "Per Capita Income"))
        
    }else if (input$dataset2 == "tedcruz.merge"){
        #summary(tedfit)$coefficients 
        stargazer(tedfit, type = "text", covariate.labels=c("% Females","% White", "% Hispanic","% With Bachelor's or More","Home Owner Rate","Household Median Income", "% Over 65", "% Black", "% Asian", "% Below Poverty" , "Per Capita Income"))
      
    }else if (input$dataset2 == "kasich.merge"){
      #summary(tedfit)$coefficients 
      stargazer(kasichfit, type = "text",covariate.labels=c("% Females","% White", "% Hispanic","% With Bachelor's or More","Home Owner Rate","Household Median Income", "% Over 65", "% Black", "% Asian", "% Below Poverty" , "Per Capita Income"))
      
    }else if (input$dataset2 == "hillary.merge"){
      #summary(tedfit)$coefficients 
      stargazer(hilfit, type = "text", covariate.labels=c("% Females","% White", "% Hispanic","% With Bachelor's or More","Home Owner Rate","Household Median Income", "% Over 65", "% Black", "% Asian", "% Below Poverty" , "Per Capita Income"))
    }
  })
}


shinyApp(ui, server)

