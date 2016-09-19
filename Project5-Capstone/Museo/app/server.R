library(shiny)
library(leaflet)
library(rjson)
library(tools)

shinyServer(function(input, output) {
    
    observe({
        recom_lst = museumInput()
        
        # check if the user have selected museums
        if (length(recom_lst) == 0){
            return({
                output$reminder = renderUI({HTML("<h5>Please Select At Least One Museum :-) </h5>")})
                for (i in seq(0:5)){
                    my_i = i-1
                    ouput_name = paste0('name_', my_i)
                    output[[ouput_name]] = renderUI({})
                    ouput_name = paste0('img_', my_i)
                    output[[ouput_name]] = renderUI({})
                    ouput_name = paste0('content_', my_i)
                    output[[ouput_name]] = renderUI({})
                    ouput_name = paste0('add_', my_i)
                    output[[ouput_name]] = renderUI({})
                    ouput_name = paste0('des_', my_i)
                    output[[ouput_name]] = renderUI({})
                    ouput_name = paste0('common_', my_i)
                    output[[ouput_name]] = renderUI({})
                }
            })
        }
        
        # create row names 
        output$reminder = renderUI({HTML("")})
        output$name_0 = renderUI({HTML("<br/>")})
        output$img_0 = renderUI({HTML("<br/>")})
        output$common_0 = renderUI({HTML('<p align = "right"><br/><b>Tags in Common:<b><br/></p>')})
        output$add_0 = renderUI({HTML('<p align = "right"><br/><b>Address:</b><br/></p>')})
        output$des_0 = renderUI({HTML('<p align = "right"><br/><b>Description:</b><br/></p>')})
        
        output$content_0 = renderUI({
            HTML(paste('<p align = "right"><br/><br/><b>Similarity Score:</b>', 
                       "<br/><br/><b>Rating:</b>",
                       "<br/><br/><b>Type:</b><br/></p>"))
        })
        
        # in case there are less than 5 museums that are being recommended
        len = length(recom_lst)
        if (len >= 5) {num = 5}
        else {num = len}
        
        # render suggested museums
        for (i in seq(1:num)){
            local({
                # it's crucial to store i in another variable to allow i to be passed into render functions
                my_i = i
                
                name = recom_lst[my_i][[1]][[1]]
                idx = which(museum$MuseumName == name)
                
                # display museum name
                ouput_name = paste0('name_', my_i)
                output[[ouput_name]] = renderUI({
                    HTML(paste0('<h4><b>', my_i,'.<br/>', name,"</b></h4>"))
                })  
                
                # display museum image
                ouput_name = paste0('img_', my_i)
                output[[ouput_name]] = renderUI({
                    HTML(paste('<img src =',link_data[name],'align="middle" width = "200", height = "150" >'))
                })  
                
                # display museum similarity score, rating, and category
                ouput_name = paste0('content_', my_i)
                output[[ouput_name]] = renderUI({
                    HTML(paste("<br/><br/>", format(round(recom_lst[my_i][[1]][[2]], 5), nsmall = 5),
                               # "<br/><br/>", recom_lst[my_i][[1]][[2]], 
                               "<br/><br/>", format(round(museum$PreciseRating[idx], 2), nsmall = 2), #format(round(x, 2), nsmall = 2)
                               "<br/><br/>", paste0(unlist(cat_data[name]), collapse = "<br>")
                    ))
                })
                
                # display tags in common
                ouput_name = paste0('common_', my_i)
                output[[ouput_name]] = renderUI({
                    HTML(paste("<br/>", get_common_tag(tag_data, input$selected_museum, name)))
                    # HTML(paste("<br/><br/>",substring(get_common(tag_data, input$selected_museum, name), 3)))
                })
                
                # display address
                ouput_name = paste0('add_', my_i)
                output[[ouput_name]] = renderUI({
                    HTML(paste("<br/>", museum$Address[idx]))
                })
                
                # display description
                ouput_name = paste0('des_', my_i)
                output[[ouput_name]] = renderUI({
                    if (museum$Description[idx] != "") {
                        HTML(paste('<br/>', museum$Description[idx], '</p>'))
                        # HTML(paste('<br/><b>Description:</b><br/> <p align = "justify">', museum$Description[idx], '</p>'))
                    }
                    else{
                        HTML("<br/>N/A") 
                    }
                    
                })    
            })
        }
        
        
        
    })
    
    # get the recommendation if the recommendation button is hit 
    museumInput = eventReactive(input$recommend_btn,{
        # get museum names
        museum_names = ""
        for (m in input$selected_museum){
            museum_names = paste(museum_names, m, sep = ';')
        }
        
        # print (museum_names)
        command_line = paste0('/Users/annecool37/anaconda2/bin/python get_sorted_suggestion.py "', museum_names, '"')
        
        # print (command_line)
        # pass the selected museum names into python to compute consine similarity
        system(command_line)
        # read the file generate from python
        top_data = fromJSON(paste(readLines('sorted_suggestion.json'), collapse=""))
        top_data
    })
    
    output$map = renderLeaflet({
        museum_complete = museum[which(!is.na(museum$Latitude)),]
        
        # 1559 museums are complete cases
        leaflet(museum_complete) %>%
            addProviderTiles("Stamen.Toner") %>% # 31.910864, -71.433555
            setView(-71.433555,31.910864, zoom = 3) %>%
            addCircleMarkers(~Langtitude, ~Latitude, clusterOptions = markerClusterOptions(),
                             fillColor = ~pal(Rating), fillOpacity = 0.7, stroke=FALSE,
                             popup = ~paste('<b><font color="#0a0a0a" size = "4">',MuseumName,'</font></b><font color="#595959"><br/>',
                                            'Rating:',format(round(PreciseRating, 2), nsmall = 2),'<br/><br/>#',Rank, 'of ',TotalThingsToDo,'things to do in this city<br/><br/>','Address:',Address, 
                                            '<br/><br/>','Description:','<br/>',Description, '</font>')) %>%
            addLegend("bottomright", pal = pal, values = ~ museum_complete$Rating,
                      title = "Rating", opacity = 0.8)
    })
    
    
    mapInput = eventReactive(input$go,{
        # Note: if nothing is selected, just display the whole data 
        total_row = nrow(museum)
        
        # get idx for museum type selected
        if (is.null(input$museum_type)){
            type_idx = c(1:total_row)
        }
        else {
            target_name_master = c()
            for (t in input$museum_type) {
                target_name_master = append(target_name_master, type_data[t][[1]])
            }
            type_idx = which(as.character(museum$MuseumName) %in% target_name_master)
        }
        
        # get index for length of visit selected
        if (is.null(input$visit_length)){
            length_idx = c(1:total_row)
        }
        else {
            length_idx = which(museum$LengthOfVisit %in% input$visit_length)
        }
        
        # get index for 'good for' & 'other option' selected
        if (is.null(input$good_for) & is.null(input$other_option)){
            idx_checked = c(1:total_row)
        }
        else {
            idx_checked = c()
            check_list = c(input$good_for, input$other_option)
            
            for (item in check_list) {
                idx = which(museum[,item] == 1)
                idx_checked = append(idx_checked, idx) 
            }
            
            if("free.admission" %in% input$other_option){
                idx = which(museum[, "free.entry"] == 1)
                idx_checked = append(idx_checked, idx) 
                idx_to_add= which(museum$Fee == 'No ')
                idx_checked = append(idx_checked, idx_to_add)
                idx_to_remove = which(museum$Fee == 'Yes ')
                idx_checked = idx_checked[! idx_checked %in% idx_to_remove]
            }
            
            
        }
        
        # find intersect of idx
        idx_final = intersect(type_idx, length_idx)
        idx_final = intersect(idx_final, idx_checked)
        
        # get new museum dataframe based on selected index
        new_museum = museum[idx_final,]
        new_museum
    })
    
    observe({
        # observe the user selection to filter the museums that satisfy the criteria
        new_museum = mapInput()
        museum_complete = new_museum[which(!is.na(new_museum$Latitude)),]
        leafletProxy("map", data = museum_complete) %>%
            clearMarkerClusters() %>%
            addCircleMarkers(~Langtitude, ~Latitude, clusterOptions = markerClusterOptions(),
                             fillColor = ~pal(Rating), fillOpacity = 0.7, stroke = FALSE,
                             popup = ~paste('<b><font color="#0a0a0a" size = "4">',MuseumName,'</font></b><font color="#595959"><br/>',
                                            'Rating:',format(round(PreciseRating, 2), nsmall = 2),'<br/><br/>#',Rank, 'of ',TotalThingsToDo,'things to do in this city<br/><br/>','Address:',Address, 
                                            '<br/><br/>','Description:','<br/>',Description, '</font>'))
    })
    
    
    
    
})    
