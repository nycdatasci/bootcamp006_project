## app.R ##
# Shiny Project: Global Trade (Coffee)
# Chia-An Chen, 07/31/2016

# load libraries
library(dplyr)
library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
library(plotly)

# read the pre-proccessed coffee dataset
coffee = read.csv("./data/coffee_master_df.csv", header = TRUE, sep = ",")[2:12]

# generate the dataframe for plotting working hour vs. coffee consumption
coffee_consum_hr = filter(coffee, Attribute_Description == "Domestic Consumption") %>%
    select (2,3,6,7)
coffee_consum_hr= coffee_consum_hr[complete.cases(coffee_consum_hr),]
coffee_consum_hr_df = group_by(coffee_consum_hr, region) %>%
    summarise("mean_kg" = mean(kg/num_person, na.rm =TRUE), 
              "mean_working_hr" =  mean(working_hr*1000000/num_person, na.rm =TRUE),
              "kg_per_hr" = mean(kg/working_hr, na.rm = TRUE))

# get lists of selections for user to choose from
attribute_lst = as.character(unique(coffee$Attribute_Description))
region_lst = as.character(unique(coffee$region))
eu_names = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
             "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
             "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", 
             "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")

# find icon here
# http://fontawesome.io/icons/

# UI
ui = dashboardPage(
    dashboardHeader(title = span(tagList(icon("coffee"), "Coffee: Global Trade")),
                    titleWidth = 250),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("General View", tabName = "map", icon = icon("map")),
            menuItem("Trend", tabName = "line", icon = icon("line-chart")),
            menuItem("Correlation", tabName = "corr", icon = icon("lightbulb-o")),
            menuItem("Fun Facts", tabName = "funf", icon = icon("smile-o")),
            menuItem("Info", tabName = "sorc", icon = icon("info")),
            helpText("About Author",  align = "center"),
            menuItemOutput("lk_in"),
            menuItemOutput("blg")
        )),  
    dashboardBody(
        # customize color for header and slider
        tags$head(tags$style(HTML('/* logo */
                                .skin-blue .main-header .logo {
                                background-color: #543005;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #543005;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #8c510a;
                                } 

                                /* toggle button when hovered  */                    
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #543005;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #252525;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #01665e;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #35978f;
                                }'))),
        
        # customize color for boxes
        tags$style(HTML("
                      /* reset the box color when status = 'primary' */
                      .box.box-solid.box-primary>.box-header {
                      color:#ffffff;
                      background:#35978f
                      }
                      
                      .box.box-solid.box-primary{
                      border-bottom-color:#f5f5f5;
                      border-left-color:#f5f5f5;
                      border-right-color:#f5f5f5;
                      border-top-color:#35978f;}

.js-irs-0 .irs-bar {
border-top-color: #c7eae5;
                        border-bottom-color: #c7eae5;
                        } 
                        
                        .js-irs-0 .irs-bar-edge {
                        border-color: #c7eae5;
                        }
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
            background: #c7eae5}


                      
                      ")),
        
        
        
        tabItems(
            # map tab
            tabItem(tabName = "map",
                    #                     fluidRow(infoBoxOutput("maxBox"),
                    #                              infoBoxOutput("minBox"),
                    #                              infoBoxOutput("avgBox")),
                    fluidRow( 
                        box(sliderInput("year_range", "Year", min = 1960, max = 2015, 
                                        value = c(1960, 2015), 
                                        animate = animationOptions(interval = 500),
                                        step = 5), width = 6),
                        box(selectizeInput("attr", "Category", 
                                           attribute_lst, 
                                           selected = "Domestic Consumption"),  width = 3),
                        box(selectizeInput("unit_map", "Unit", 
                                           c("kg" = "kg","kg/gdp" = "kg_per_gdp",
                                             "kg/person" = "kg_per_person"),
                                           selected = "kg_per_person"), width = 3)
                        # box(numericInput("top_n", "Top n Ranking", 10, min = 5, max = 20, step = 5), width = 4)
                    ),
                    fluidRow(
                        box(status = "primary", solidHeader = TRUE, width = 6,
                            title = textOutput("map_info"),
                            htmlOutput("world_map", align = "center")),
                        box(htmlOutput("bar_chart", align = "center"), width = 6),
                        helpText("Average is calculated within the year range selected", align = "right"))
                    
                    
                    
            ),
            # line chart tab
            tabItem(tabName = "line",
                    box(checkboxGroupInput("checked", "Category", 
                                           attribute_lst,
                                           selected = attribute_lst[c(5,8)]), width = 4), 
                    
                    box(width = 4, selectizeInput("country_nm", "Country",
                                                  region_lst, 
                                                  selected = "European Union")),
                    box(width = 4, selectizeInput("type_for_line", "Unit", 
                                                  c("kg" = "kg","kg/gdp" = "kg_per_gdp",
                                                    "kg/person" = "kg_per_person"), 
                                                  selected = "kg_per_gdp")),
                    box(status = "primary", solidHeader = TRUE,
                        title = textOutput("line_info"),
                        htmlOutput("line_chart", width = "100%"), width = 8)
                    
            ),
            
            # correlation tab based on year
            tabItem(tabName = "corr",
                    fluidRow(
                        box(selectizeInput("year_corr","Year", seq(1960, 2015), selected = "2015"), width = 4),
                        box(selectizeInput("attr_x", "x axis", attribute_lst, selected = "Imports"), width = 4),
                        box(selectizeInput("attr_y", "y axis", attribute_lst, selected = "Domestic Consumption"), width = 4)
                    ),
                    fluidRow(
                        column(width = 12, align = "center", box(plotlyOutput("corr_chart"), width = 12))
                    ),
                    helpText("Dashed line repersents the average kg of the atribute selected", align = "right"),
                    helpText("Click and drag to zoom in on a cluster of markers", align = "right")
                    
            ),
            
            # fun facts tab
            tabItem(tabName = "funf",
                    # working hour vs. coffee consumption
                    fluidRow(
                        box(title = "Work More, Drink More?", 
                            status = "primary", 
                            solidHeader = TRUE,
                            htmlOutput("bubble_chart"), width = 12)
                    ),
                    helpText("The size of bubbles corresponds to coffee consumption/annual working hour (kg/hr)", align = "right")
            ),
            # source tab
            tabItem(tabName = "sorc",
                    box(width = 12, status = "primary", solidHeader = TRUE, title = "Info",
                        tags$p("This app visualizes the global trade of coffee from 1960 to 2015."),
                        tags$b("Units for General View, Trend, and Correlation tabs"),
                        tags$div("kg: average of total kg", tags$br(),
                                 "kg/person: average of total kg divided by population", tags$br(),
                                 "kg/gdp: average total kg divided by GDP in USD"),
                        tags$br(),
                        tags$b("General View: Map & Bar Chart"),
                        tags$div("The coffee data groups all countries in the European Union together. 
                                 In order to visualize the data on the map for countries in the EU, 
                                 the data for the EU is passed manually to all these 28 countries: 
                                 Austria, Belgium, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, 
                                 Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, 
                                 Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, 
                                 Romania, Slovakia, Slovenia, Spain, Sweden, and United Kingdom. 
                                 The values for all the countries in the EU are the same, however, 
                                 as each country reflects the single EU value in the database."),
                        tags$br(),
                        tags$b("Correlation"),
                        tags$p("Log transformation was applied to prevent data from compressing to the bottom left. 
                                Jittering points was applied to reduce overlapping, which results in producing negative values for some data points.
                               If only a few countries are shown, it means the data was not available."),
                        tags$b("Fun Facts"),
                        tags$div("The units used in the plot in this tab are different from plots in other tabs. Please refer to below for clarification.", tags$br(),
                                 "mean_kg: average total kg/population", tags$br(), 
                                 "mean_working_hr: 1,000,000*average annual working hour/population", tags$br(),
                                 "kg_per_hr: average total kg/annual working hour")
                    ),
                    box(width = 12, status = "primary", solidHeader = TRUE, title = "Data Source", 
                        tags$a(href="http://apps.fas.usda.gov/psdonline/psdDownload.aspx", 
                               "Production, supply, and distribution of coffee from United States Department of Agriculture"),
                        tags$br(),
                        tags$a(href="http://databank.worldbank.org/data/reports.aspx?source=2&series=SP.POP.TOTL&country=#", 
                               "Total population from World Databank"),
                        tags$br(),
                        tags$a(href="http://databank.worldbank.org/data/reports.aspx?source=2&series=NY.GDP.MKTP.CD&country=#", 
                               "GDP from World Databank"),
                        tags$br(),
                        tags$a(href="https://stats.oecd.org/Index.aspx?DataSetCode=ANHRS", 
                               "Annual working hour from OECD")) 
                    
            )
        )
    )
)

# Server
server = function(input, output) {
    # generate map title based on input
    output$map_info = renderText({
        if (input$year_range[1] != input$year_range[2]){
            txt = paste(input$attr,"in the World from", input$year_range[1],"to",input$year_range[2])   
        }
        else {
            txt = paste(input$attr,"in the World in", input$year_range[1])
        }
        txt
    }) 
 
    # map
    coffee_by_year = reactive({
        validate(
            need(input$attr != "", "Please select a category"),
            need(input$unit_map != "", "Please select a unit")
        )
        
        # since population and gdp data from 1960 to 1965 don't exsit
        # validate the input accordingly
        if (input$year_range[2]  <= 1965) {
            validate(
                need(input$unit_map == "kg", "Please select unit as kg for data before 1965")
            )
        }

        df_whole_eu = coffee %>% 
            filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
            filter(Attribute_Description == input$attr) %>%
            group_by(region) %>%
            summarize_("Mean" = paste0("mean(", input$unit_map, ", na.rm= TRUE)"))
        
        # check if data for European Union is missing 
        if ("European Union" %in% df_whole_eu$region == FALSE) {
            df = data.frame("region" = "European Union", "Mean" = 0)
            df_whole_eu = rbind(df_whole_eu, df)
        }
        # In order to show data in EU,
        # manipulate data in European Union to induvidual countries in EU
        df_indiv_eu = data.frame("region" = eu_names, "Mean"= df_whole_eu[df_whole_eu$region == "European Union",]$Mean)
        coffee_for_map = rbind(df_whole_eu, df_indiv_eu)
        coffee_for_map
    })
    output$world_map = renderGvis({
        gvisGeoChart(coffee_by_year()[complete.cases(coffee_by_year()), ], "region", "Mean", 
                     option = list(colors = "['#f5f5f5', '#01665e']"
                                   # height = 600
                     ))
    })
    
    # bar chart to see ranking
    coffee_by_country = reactive({
        validate(
            need(input$attr != "", "Please select a category"),
            need(input$unit_map != "", "Please select a unit")
            
        )
        
        if (input$year_range[2]  <= 1965) {
            validate(
                need(input$unit_map == "kg", "Please select unit as kg for data before 1965")
            )}
        
        coffee %>%
            filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
            filter(Attribute_Description == input$attr) %>%
            group_by(region) %>%
            summarize_(Mean = paste0("mean(", input$unit_map, ", na.rm= TRUE)")) %>%
            arrange(desc(Mean))
    }) 
    
    output$bar_chart = renderGvis({
        #gvisBarChart(coffee_by_country()[1:input$top_n,],
        gvisBarChart(coffee_by_country()[1:10,],
                     options = list(height = 350, width = 600,
                                    colors = "#5ab4ac",
                                    legend = "{position: 'none'}",
                                    titleTextStyle="{fontSize: 16 }",
                                    title = paste("Top 10 Countries in",input$attr)
                     ))
    })
    
    # generate line chart title based on user's selection
    output$line_info = renderText({
        txt = paste(input$type_for_line,"vs. Year")
        txt
    })
    
    
    # line chart (time series) to see how attributes change over time for a particular country
    coffee_over_time = reactive({
        validate(
            need(input$checked != "", "Please check at least one variable in category"),
            need(input$country_nm != "", "Please select a country"),
            need(input$type_for_line != "", "Please select a unit")
        )
        
        coffee_subset = coffee %>% filter(region == input$country_nm) 
        n = length(input$checked)
        df = data.frame("year" = min(coffee_subset$year): max(coffee_subset$year))
        for (i in 1:n) {
            # filter df based on country selected
            temp_df = filter(coffee_subset, Attribute_Description == input$checked[i])
            # select column year and data type selected
            temp_df = temp_df[,c(4, which(names(temp_df) == input$type_for_line))]
            names(temp_df) = c("year", input$checked[i])
            df = full_join(df, temp_df) %>% arrange(year)
        }
        df = unique(df)
        df
    })
    # render line chart (time series)
    output$line_chart = renderGvis(
        gvisLineChart(coffee_over_time(),
                      option = list(
                          # title = paste("Changes in", input$type_for_line,"vs. Year"),
                          hAxis = "{title:'Year'}",
                          legend= "{position: 'bottom'}",
                          height = 400
                      ))
    )
    
    # plot to see correlation between two attributes
    coffee_correlation = reactive({
        validate(
            # need(input$cntry != "", "Please select a country"),
            need(input$year_corr != "", "Please select a year"),
            need(input$attr_x != "", "Please select the variable for x axis"),
            need(input$attr_y != "", "Please select the variable for y axis"),
            need(input$attr_y != input$attr_x, "Please avoid selecting same attributes for both axes")
        )
        
        df = filter (coffee, year == input$year_corr) %>%
            group_by(Attribute_Description, region) %>%
            summarise("mean_kg" = mean(kg, na.rm = TRUE))
        
        # reformat dataframe for plotting purpose
        x_df = df[df$Attribute_Description == input$attr_x,][,c(2,3)]
        names(x_df) = c("region", input$attr_x)
        y_df = df[df$Attribute_Description == input$attr_y,][,c(2,3)]
        names(y_df) = c("region", input$attr_y)
        df_for_plot = inner_join(x_df, y_df)
        
        # log transformation
        df_for_plot[input$attr_x] = log(df_for_plot[input$attr_x]+1)
        df_for_plot[input$attr_y] = log(df_for_plot[input$attr_y]+1)
        
        # chech if it's an empty dataframe
        validate(
            need(dim(df_for_plot)[1] !=0, "No available data for this combination")
        )
        df_for_plot
    })
    
    # render scatter plot for correlation
    output$corr_chart = renderPlotly({
        coffee_corr = as.data.frame(coffee_correlation())
        x_mean = mean(coffee_corr[,2], na.rm = TRUE)
        y_mean = mean(coffee_corr[,3], na.rm = TRUE)
        g = ggplot(coffee_corr, aes(x = coffee_corr[,2], y = coffee_corr[,3], fill = region))
        p = g + # geom_point(color="darkslategray4", alpha = .8, size = 3) + 
            geom_jitter(color="darkslategray4", alpha = .8, size = 3) +
            labs(x = paste0("log(",input$attr_x,") (kg)"), y = paste0("log(",input$attr_y,") (kg)"), 
                 title = paste(input$attr_y, "vs.", input$attr_x)) +
            geom_vline(xintercept = x_mean, linetype="dashed", size = .2) + 
            geom_hline(yintercept = y_mean, linetype="dashed", size = .2) +
            theme_bw() + theme(legend.position ="none")
        ggplotly(p)
    })
    
    # fun facts chart: annual working hour vs. coffee consumption
    output$bubble_chart = renderGvis(
        gvisBubbleChart(coffee_consum_hr_df, idvar = "region",
                        xvar= "mean_kg",
                        yvar = "mean_working_hr",
                        colorvar = "region", sizevar= "kg_per_hr",
                        options =list(
                            title = "Annual Working Hour vs. Coffee Consumption",
                            vAxis ="{minValue:-100, maxValue:500, title:'Annual Working Hour (hr*1000000/year/person)'}",
                            hAxis ="{title:'Coffee Consumption (kg/year/person)'}",
                            sizeAxis = "{minSize: 20,  maxSize: 40}", 
                            height = 500))
    )
    
    # link to my LinkedIn
    output$lk_in = renderMenu ({
        menuItem("LinkedIn", icon = icon("linkedin-square"),
                 href = "https://www.linkedin.com/in/chiaanchen")
    })
    
    # link to my blog
    output$blg = renderMenu ({
        menuItem("Blog", icon = icon("link"),
                 href = "http://blog.nycdatascience.com/author/annecool37/")
    })
    
}

# run app
shinyApp(ui, server)