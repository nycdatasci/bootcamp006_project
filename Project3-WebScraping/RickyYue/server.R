server = function(input, output,session) { 

   cat_select1 <- reactive({
     if(input$cat1 == "All"){
       c(unique(df_avail$category))
     } else {
       input$cat1
     }
   })
   
   typ_select2 <- reactive({
     input$type2
   })
   
   
   cat_select2 <- reactive({
     if(input$cat2 == "All"){
       c(unique(df_avail$category))
     } else {
       input$cat2
     }
   })
   
   typ_select3 <- reactive({
     input$type3
   })
   
   
   cat_select3 <- reactive({
     if(input$cat3 == "All"){
       c(unique(df_avail$category))
     } else {
       input$cat3
     }
   })
   

   mer_select3 <- reactive({
       input$merchant3
   })
   
   
   typ_select4 <- reactive({
     input$type4
   })
   
   typ_select5 <- reactive({
     input$type5
   })

   df_tab1 <- reactive({
      df_by_type = df_avail %>% filter(category==cat_select1())
      col_len = max(df_by_type$count)
      cols=list()
      for(i in 1:nrow(df_by_type)){
        num_yes = df_by_type$avail_y[i]
        num_nay = df_by_type$avail_n[i]
        num_nas = col_len - num_yes - num_nay
        cols[[i]] = as.factor(c(rep(1,num_yes),rep(0,num_nay),rep(NA,num_nas)))
      }
      
      df_tab1 = data.frame(do.call(cbind,cols))
      colnames(df_tab1) = df_by_type$category
      df_tab1
   })
   
   
   df_tab2 <- reactive({
     df_tab2 = df %>% group_by(opt_hour,type,category) %>%
       filter(type==typ_select2()) %>% filter(category %in% cat_select2()) %>%
       summarize(total.count=n(),total.value=sum(value))
     df_tab2
   })
   
   output$myCatSelection <- renderUI({
     selectInput("merchant3",label=h4("Merchant"), 
                 choices=unique(df[which(df$category==input$cat3),]$merchant),
                 selected=unique(df[which(df$category==input$cat3),]$merchant)[1],
                 multiple=TRUE)
   })
   
   df_tab3 <- reactive({
     
       df_tab3 = df %>% group_by(opt_hour,type,category,merchant) %>%
         filter(type == typ_select3()) %>% filter(category == cat_select3()) %>%
         filter(merchant %in% mer_select3()) %>%
         summarize(total.count=n(),total.value=sum(value))
  

   })
   
   
   df_tab4 <- reactive({
     data_select_table=data_merged %>% filter(type==typ_select4()) %>% 
       group_by(opt_hour,merchant) %>% 
       mutate(cnt=n(),val=sum(value)) %>%
       select(discount,merchant,opt_hour,cnt,val) %>% 
       group_by(merchant) %>% 
       mutate(cnt_mean = round(mean(cnt)),val_mean=mean(val),cnt_diff = max(cnt)-min(cnt),val_diff=max(val)-min(val)) %>%
       select(merchant,discount,cnt_mean,val_mean,cnt_diff,val_diff) %>%
       distinct(merchant)
   })
   
   df_tab5 <- reactive({
     df_3d=data_merged %>% filter(type==typ_select5()) %>% 
        group_by(opt_hour,merchant) %>% 
        mutate(cnt=n(),val=sum(value)) %>%
        select(discount,merchant,opt_hour,cnt,val) %>% 
        group_by(merchant) %>% 
        mutate(cnt_mean = round(mean(cnt)),val_mean=mean(val),cnt_diff = max(cnt)-min(cnt),val_diff=max(val)-min(val)) %>%
        select(merchant,discount,cnt_mean,val_mean,cnt_diff,val_diff) %>%
        distinct(merchant)
   })

  output$myLikertChart = renderPlot({
    levels = list(c("Out of Stock", "Available"))
    sjp.setTheme(theme = "blank")#,axis.textsize = .85)
    sjp.likert(df_tab1(), legendLabels=levels,geom.colors = c("red","green"))
    
  })
  
  output$myCountChart1 = renderPlotly({
  
    count.plot1=ggplot(df_tab2(), aes(x=opt_hour, y=total.count,color=category,group=category)) +
      geom_line()+
      ggtitle(as.character(typ_select2())) +
      labs(x="Time",y="Total Count")+
      geom_point(size=3) +
      scale_x_discrete(limit = sort(unique(df_tab2()$opt_hour))) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))

    ggplotly(count.plot1)
  })


  output$myValueChart1 = renderPlotly({
    value.plot1=ggplot(df_tab2(), aes(x=opt_hour, y=total.value,color=category,group=category)) +
      geom_line()+
      labs(x="Time",y="Total Value")+
      geom_point(size=3) +
      scale_x_discrete(limit = sort(unique(df_tab2()$opt_hour))) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    ggplotly(value.plot1)
    
  })
  

  output$myCountChart2 = renderPlot({
    count.plot2=ggplot(df_tab3(), aes(x=opt_hour, y=total.count,color=merchant,group=merchant)) +
      geom_line()+
      ggtitle(as.character(cat_select3())) +
      labs(x="Time",y="Total Count")+
      geom_point(size=3) +
      scale_x_discrete(limit = sort(unique(df_tab3()$opt_hour))) +
      theme(plot.title = element_text(size=24,color="black"))+
      theme(axis.title = element_text(size=20,color="black"))+
      theme(axis.text = element_text(size=15))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    count.plot2
    
  })
  
  output$myValueChart2 = renderPlot({
    value.plot2=ggplot(df_tab3(), aes(x=opt_hour, y=total.value,color=merchant,group=merchant)) +
      geom_line()+
      labs(x="Time",y="Total Value")+
      geom_point(size=3) +
      scale_x_discrete(limit = sort(unique(df_tab3()$opt_hour))) +
      theme(plot.title = element_text(size=24,color="black"))+
      theme(axis.title = element_text(size=20,color="black"))+
      theme(axis.text = element_text(size=15))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    value.plot2
    
  })
  
  output$myTable = renderDataTable({
    ta=df_tab4()
    colnames(ta)=c("Merchant","Discount(%)","# of Cards Avail.","Total Card Value","Max. Count Diff.","Max. Value Diff.")
    ta
  })
  
  
  output$my2dChart = renderPlotly({
    p = plot_ly(df_tab5(),x=cnt_mean,y=cnt_diff,mode="markers",color=discount,colors = "YlOrRd",text = merchant)%>%
    layout(xaxis = list(title = "Average # of Stock"), yaxis = list(title = "Range of Stock"))
    p
  })

}



