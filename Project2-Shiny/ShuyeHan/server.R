install.packages("devtools")
require(devtools)
install_github("flovv/Roxford")
install.packages('randomForest')
install.packages('gridExtra')
install.packages('gtable')
install.packages('parallel')
install.packages('png')
install.packages('shinyBS')

library(shiny)
#options(shiny.maxRequestSize = 16*1024^2)
library(randomForest); library(ggplot2); library(plyr);library(reshape2); library(gridExtra); library(gtable); library(parallel);library(png)
require(Roxford);library(dplyr);library(stringr);library(shinyBS)

shinyServer(function(input, output) {
  
  tabPanelAbout <- source("source/intro.R", local=T)$value
  
  #geom_vline(xintercept = 1:5, colour="red", linetype = "longdash")
  output$tp.gettingstartedside<- renderUI({ if (is.null(input$file1)) {sidebarPanel(fileInput('file1', 'Choose image to upload',
                                                                                          multiple = TRUE,
                                                                                          accept = c(
                                                                                            'image/png', 'image/jpeg'
                                                                                          )), navlistPanel(
                                                                                            "Or log in through: ",
                                                                                            tabPanel("Facebook",icon=icon('facebook-official')),
                                                                                            tabPanel("Google Plus",icon=icon('google-plus')),
                                                                                            tabPanel("Instagram",icon=icon('instagram'))
                                                                                         ,widths = c(12, 8) ),width = 12)
    } else{
      sidebarPanel(  verbatimTextOutput("info"),hr(), navlistPanel(
        "Or log in through: ",
        tabPanel("Facebook",icon=icon('facebook-official')),
        tabPanel("Google Plus",icon=icon('google-plus')),
        tabPanel("Instagram",icon=icon('instagram'))
      ,widths = c(12, 8)),width = 12)
    }
    
    
  })
  
  filteredframe<-reactive({
    
    filteredframe<-searchframe
    
    if (length(input$filter)>0)
    {
      filteredframe<-filteredframe[filteredframe[[input$domain]] %in% input$filter,]
    }
    
    filteredframe
  })
  
  output$tp.facematchside<- renderUI({sidebarPanel( h4("Here is the test result"),tags$ul(
    tags$li(tags$b("Face ID:"),tags$div(testresult()$faceId)), 
    tags$li(tags$b("Gender:"),tags$div(testresult()$faceAttributes.gender)), 
    tags$li(tags$b("Estimated age:"),tags$div(testresult()$faceAttributes.age)),
    tags$li(tags$b("Smile:"),tags$div(testresult()$faceAttributes.smile))
  ), tagList(
    bsModal("modal", "Feature description", trigger = "detail", tags$ul(
      tags$li(tags$b("Face ID:"),tags$div(testresult()$faceId)),
      tags$li(tags$b("Face Length:"),tags$div(testresult()$faceRectangle.width)),
      tags$li(tags$b("Face Height:"),tags$div(testresult()$faceRectangle.height)),
      tags$li(tags$b("Gender:"),tags$div(testresult()$faceAttributes.gender)),
      tags$li(tags$b("Age:"),tags$div(testresult()$faceAttributes.age)),
      tags$li(tags$b("Happiness:"),tags$div(testresult()$scores.happiness)),
      tags$li(tags$b("Sadness:"),tags$div(testresult()$scores.sadness)),
      tags$li(tags$b("Fearness:"),tags$div(testresult()$scores.fear)),
      tags$li(tags$b("Angriness:"),tags$div(testresult()$scores.anger))
      )),
    actionButton("detail", "Show detail")
  ),width = 12)})
  
  output$tp.featureanalysisside<- renderUI({column(12,wellPanel(radioButtons("radio", label = h3("Select features"),
                                                         choices = list("Face area" = 1, "Gender" = 2, "Age" = 3, "Facial hair"=4,"Emotions"=5), 
                                                         selected = 1)),hr(),wellPanel(checkboxGroupInput("emotioncheck", "Emotions:",
                                                                                                          c("Smile" = "smile",
                                                                                                            "Angry" = "angry",
                                                                                                            "Fear" = "fear",
                                                                                                            "Sad"="sad")))
                                                   ,wellPanel(sliderInput("scale", "Integer:", min=0, max=100, value=50)))})
  
  output$tp.imageanalysisside<- renderUI({column(12,wellPanel(selectInput("domain","Select a classification feature:",c("Gender" = "faceAttributes.gender",
                                                                                                         "Moustache" = "faceAttributes.facialHair.moustache",
                                                                                                         "Beard"="faceAttributes.facialHair.beard",
                                                                                                         "Sideburns" = "faceAttributes.facialHair.sideburns"))
                                                    ,uiOutput("subfilter")),hr(),
                                                 wellPanel(selectizeInput("xaxis","Select the x arguments",wholelist,options = list(
                                                   placeholder = 'Please select x',
                                                   onInitialize = I('function() { this.setValue(""); }')
                                                 ),multiple=F),
                                                           uiOutput("yaxisui"),uiOutput("zaxisui")))})
  
  output$yaxisui<-renderUI({
    
    selectizeInput("yaxis","Select the y arguments",wholelist[!wholelist %in% input$xaxis],options = list(
      placeholder = 'Please select y',
      onInitialize = I('function() { this.setValue(""); }')
    ),multiple=F)
    
  })
  
  output$zaxisui<-renderUI({
    
    selectizeInput("zaxis","Select the z arguments",wholelist[!wholelist %in% c(input$xaxis,input$yaxis)],options = list(
      placeholder = 'Please select z',
      onInitialize = I('function() { this.setValue(""); }')
    ),multiple=F)
    
  })
  
  output$tp.findfaceside<- renderUI({
    
    column(12,wellPanel(
      selectizeInput(
        'column', 'Please select features:', choices = wholelist, multiple = TRUE,options = list(maxItems = 4)
      )),
      wellPanel(textInput("findid", label = h3("Type the faceID:"), value = "")),
      wellPanel(sliderInput("agerange", "Age Range:",
                  min = 10, max = 80, value = c(30,50))),
      wellPanel(checkboxGroupInput("choosegender", "Choose gender:",
                         c("Male" = "male",
                           "Female" = "female"),inline=TRUE))
      
    )
  })
  
  
  
  
  output$subfilter<-renderUI({
    selectInput("filter","Select a filter in this domain:",filtervalue(),selected=NULL,multiple=T)
  })
  
  
  filtervalue<-reactive({
    if (!is.null(input$domain))
    {
      print (input$domain)
    if ("faceAttributes.gender" %in% input$domian)
    {
      c("male","female")
    }
    else
    {
      levels(searchframe[[input$domain]])
    }
    }
  })
  
  output$tp.facematch<- renderUI({tabsetPanel(
    tabPanel("All",column(12,img(src=allsrc()$source),wellPanel(paste("Similarity:",allsrc()$confidence)))),
    tabPanel("Gender",column(6,wellPanel("male"),img(src = malesrc()$source),wellPanel(paste("Similarity:",malesrc()$confidence))),column(6,wellPanel("female")),img(src=femalesrc()$source),wellPanel(paste("Similarity:",femalesrc()$confidence))),
    tabPanel("Age",column(4,wellPanel("age 20s"),img(src=twentysrc()$source),wellPanel(paste("Similarity:",twentysrc()$confidence))),column(4,wellPanel("age 30s"),img(src=thirtysrc()$source),wellPanel(paste("Similarity:",thirtysrc()$confidence))),column(4,wellPanel("above 40s"),img(src=fortysrc()$source),wellPanel(paste("Similarity:",fortysrc()$confidence)))),
    id="tabpanel"
  )})
  
  twentysrc<-reactive({
    
    faceFINDSIMILAR = POST(
      url = 'https://api.projectoxford.ai/face/v1.0/findsimilars',
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
      body = list(faceId=testresult()$faceId,faceListID = 'list_age_2',maxNumOfCandidatesReturned=2,mode='matchFace'),
      encode = 'json'
    )
    
    pid=content(faceFINDSIMILAR)[[1]]$persistedFaceId
    tempframe=dataframe_list[[5]]
    result=tempframe[tempframe['perid']==pid,]$link[[1]][1]
    download.file(result,destfile=paste0('www/',testresult()$faceId,'twenty.jpg'))
    list(source=paste0(testresult()$faceId,'twenty.jpg'),confidence=content(faceFINDSIMILAR)[[1]]$confidence)
    
  })
  
  thirtysrc<-reactive({
    
    faceFINDSIMILAR = POST(
      url = 'https://api.projectoxford.ai/face/v1.0/findsimilars',
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
      body = list(faceId=testresult()$faceId,faceListID = 'list_age_3',maxNumOfCandidatesReturned=2,mode='matchFace'),
      encode = 'json'
    )
    
    pid=content(faceFINDSIMILAR)[[1]]$persistedFaceId
    tempframe=dataframe_list[[6]]
    result=tempframe[tempframe['perid']==pid,]$link[[1]][1]
    download.file(result,destfile=paste0('www/',testresult()$faceId,'thirty.jpg'))
    list(source=paste0(testresult()$faceId,'thirty.jpg'),confidence=content(faceFINDSIMILAR)[[1]]$confidence)
    
  })
  
  fortysrc<-reactive({
    
    faceFINDSIMILAR = POST(
      url = 'https://api.projectoxford.ai/face/v1.0/findsimilars',
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
      body = list(faceId=testresult()$faceId,faceListID = 'list_age_4',maxNumOfCandidatesReturned=2,mode='matchFace'),
      encode = 'json'
    )
    
    pid=content(faceFINDSIMILAR)[[1]]$persistedFaceId
    tempframe=dataframe_list[[7]]
    result=tempframe[tempframe['perid']==pid,]$link[[1]][1]
    download.file(result,destfile=paste0('www/',testresult()$faceId,'forty.jpg'))
    list(source=paste0(testresult()$faceId,'forty.jpg'),confidence=content(faceFINDSIMILAR)[[1]]$confidence)
    
  })
  allsrc<-reactive({
    faceFINDSIMILARmale = POST(
      url = 'https://api.projectoxford.ai/face/v1.0/findsimilars',
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
      body = list(faceId=testresult()$faceId,faceListID = 'list_gender_male',maxNumOfCandidatesReturned=2,mode='matchFace'),
      encode = 'json'
    )
    
    
    
    faceFINDSIMILARfemale = POST(
      url = 'https://api.projectoxford.ai/face/v1.0/findsimilars',
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
      body = list(faceId=testresult()$faceId,faceListID = 'list_gender_female',maxNumOfCandidatesReturned=2,mode='matchFace'),
      encode = 'json'
    )
    final<-NULL
    i<-0
    if (content(faceFINDSIMILARmale)[[1]]$confidence>content(faceFINDSIMILARfemale)[[1]]$confidence)
    {
      final<-content(faceFINDSIMILARmale)
      i<-1
    }
    else
    {
      final<-content(faceFINDSIMILARfemale)
      i<-2
    }
    
    pid=final[[1]]$persistedFaceId
    tempframe=dataframe_list[[i]]
    result=tempframe[tempframe['perid']==pid,]$link[[1]][1]
    download.file(result,destfile=paste0('www/',testresult()$faceId,'all.jpg'))
    list(source=paste0(testresult()$faceId,'all.jpg'),confidence=final[[1]]$confidence)
  })
  femalesrc<-reactive({
    faceFINDSIMILAR = POST(
      url = 'https://api.projectoxford.ai/face/v1.0/findsimilars',
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
      body = list(faceId=testresult()$faceId,faceListID = 'list_gender_female',maxNumOfCandidatesReturned=2,mode='matchFace'),
      encode = 'json'
    )
    
    pid=content(faceFINDSIMILAR)[[1]]$persistedFaceId
    tempframe=dataframe_list[[2]]
    result=tempframe[tempframe['perid']==pid,]$link[[1]][1]
    download.file(result,destfile=paste0('www/',testresult()$faceId,'female.jpg'))
    list(source=paste0(testresult()$faceId,'female.jpg'),confidence=content(faceFINDSIMILAR)[[1]]$confidence)
  })
  
  malesrc<-reactive({
    
    faceFINDSIMILAR = POST(
      url = 'https://api.projectoxford.ai/face/v1.0/findsimilars',
      content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
      body = list(faceId=testresult()$faceId,faceListID = 'list_gender_male',maxNumOfCandidatesReturned=2,mode='matchFace'),
      encode = 'json'
    )
    
    pid=content(faceFINDSIMILAR)[[1]]$persistedFaceId
    tempframe=dataframe_list[[1]]
    result=tempframe[tempframe['perid']==pid,]$link[[1]][1]

    download.file(result,destfile=paste0('www/',testresult()$faceId,'male.jpg'))
    list(source=paste0(testresult()$faceId,'male.jpg'),confidence=content(faceFINDSIMILAR)[[1]]$confidence)
  })
  
  output$tp.featureanalysis<- renderUI({
    column(12,plotOutput("distributionplot"),hr(),tableOutput("featuretext"))
     })
  
  output$tp.imageanalysis<- renderUI({
    column(12,plotOutput("analysis"),hr(),tableOutput("imagetext"))
  })
  
  output$imagetext<-renderTable({ 
    if (input$xaxis!="")
    {
    feature_list=input$xaxis
    if (input$yaxis!="")
    {
      feature_list=c(feature_list,input$yaxis)
    }
    if (input$zaxis!="")
    {
      feature_list=c(feature_list,input$zaxis)
    }
    summary(filteredframe()[feature_list])
    }
  })
    
    
  output$analysis<-renderPlot({
    if (input$xaxis!="")
    {
      if (input$yaxis!="")
      {
   
        if (input$zaxis!="")
        {
          dis_list=c()
          con_list=c()
          if (input$xaxis %in% discretelist)
          {
            dis_list=c(dis_list,input$xaxis)
          }
          else
          {
            con_list=c(con_list,input$xaxis)
          }
          
          if (input$yaxis %in% discretelist)
          {
            dis_list=c(dis_list,input$yaxis)
          }
          else
          {
            con_list=c(con_list,input$yaxis)
          }
          
          if (input$zaxis %in% discretelist)
          {
            dis_list=c(dis_list,input$zaxis)
          }
          else
          {
            con_list=c(con_list,input$zaxis)
          }
          
          len=length(dis_list)
          
          if (len==0)
          {
            g<-ggplot(filteredframe(),aes_string(x=con_list[1],y=con_list[2],colour=con_list[3]))+ geom_point()+scale_colour_gradient(trans = "log")
            print(g)
          }
          
          else if (len==1)
          {
            g<-ggplot(filteredframe(), aes_string(x=con_list[1], y=con_list[2],colour=dis_list[1]))+geom_point()
            print (g)
          }
          else if (len==2)
          {
            g<-ggplot(filteredframe(), aes_string(x=dis_list[1],y= con_list[1],fill=dis_list[2])) + geom_bar(position="dodge")
            print (g)
          }
          
          else
          {
            g<-ggplot(filteredframe(), aes_string(x=dis_list[1], fill=dis_list[2])) + geom_bar() +facet_wrap(as.formula(paste("~", dis_list[3])))
            print (g)
          }
          
          print (input$zaxis)
          ####
        }
        
        else if (input$xaxis %in% discretelist)
        {
          if (input$yaxis %in% discretelist)
          {
            g<-ggplot(filteredframe(), aes_string(x=input$xaxis,fill=input$yaxis))+geom_bar()
            print (g)
            #bar
            
          }
          
          else
          {
            g<-ggplot(filteredframe(), aes_string(x=input$xaxis,y=input$yaxis))+geom_boxplot(outlier.colour = "red", outlier.shape = 1,notch = TRUE)
            print (g)
            #box
          }
        }
        else
        {
          if (input$yaxis %in% discretelist)
          {
            g<-ggplot(filteredframe(), aes_string(x=input$yaxis,y=input$xaxis))+geom_boxplot(outlier.colour = "red", outlier.shape = 1,notch = TRUE)+coord_flip()
            print (g)
            #histo y=fill
          }
          else
          {
            g<-ggplot(filteredframe(), aes_string(x=input$xaxis, y=input$yaxis))+geom_point()+geom_density2d()
            print (g)
            #point
          }
        }
      }
      
       else if (input$xaxis %in% discretelist)
      {
        g<-ggplot(filteredframe(),aes_string(x=input$xaxis,fill=input$xaxis))+geom_bar(width = 1)+coord_polar()
        print (g)
      }
      else 
      {
        g<-ggplot(filteredframe(),aes_string(x=input$xaxis))+geom_histogram()+scale_colour_gradientn(colours = terrain.colors(10))
        print(g)
      }
    }
  })
  
  output$tp.findface<- renderUI({
    dataTableOutput('mytable')
  })
  
  output$mytable = renderDataTable({
    if (length(input$column)>0)
    {
      
      result_frame=searchframe
      
      if (input$findid!="")
      {
        if (nchar(input$findid)<=charlen)
       {
       result_frame<-result_frame[substr(result_frame$faceId,1,nchar(input$findid))==input$findid,]
       }
      }
      
      result_frame=result_frame[(result_frame['faceAttributes.age']>=input$agerange[1]) & (result_frame['faceAttributes.age']<=input$agerange[2]),]
      
 
      if (length(input$choosegender)>0)
      {
       result_frame=result_frame[result_frame[['faceAttributes.gender']] %in% input$choosegender,]
      }
      print (input$choosegender)
      result_frame[c("faceId",input$column)]
    }
  })
  
  output$tp.gettingstarted<-renderUI({
    myimage<-input$file1
    if (is.null(myimage))
      return (h4("please upload"))
    else
    {
      #return (h4(myimage$datapath))
      column(12,uiOutput('images'))
    }
  })
  
  
  
  myimage <- reactive({
    myimage <- input$file1
    myimage$datapath <- gsub("\\\\", "/", myimage$datapath)
    #print (myimage)
    myimage
    #searchpa<-input$file1
  
  })
  
  testresult<-reactive({ 
    searchpa<-input$file1
    tempface<-getFaceResponse(toString(searchpa$name), facekey)
    print (tempface)
    if (length(tempface)!=14)
    {
      return (NULL)
    }
    tempemotion<-getEmotionResponse(toString(searchpa$name), emotionkey)
    if (length(tempemotion)!=12)
    {
      return (NULL)
    }
    
    tempemotion=data.frame(tempemotion)
    tempface=data.frame(tempface)
    tempface['link']=searchpa
    tempemotion=tempemotion[grep('scores',names(tempemotion))]
    cbind(tempface,tempemotion)
  })
  
  output$images <- renderUI({
    if(is.null(input$file1)) return(NULL)
    image_output_list <- 
      lapply(1:nrow(myimage()),
             function(i)
             {
               imagename = paste0("image", i)
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
  
  observe({
    if(is.null(input$file1)) return(NULL)
    for (i in 1:nrow(myimage()))
    {
      print(i)
      local({
        my_i <- i
        imagename = paste0("image", my_i)
        print(imagename)
        output[[imagename]] <- 
          renderImage({
            list(src = myimage()$datapath[my_i],
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
  
  
  output$info <- renderText({paste0(
    
    "file name: ", toString(myimage()$name),"\n","\n",
    "image size: ", toString(myimage()$size),"\n","\n",
    "image type: ", toString(myimage()$type),"\n","\n",
    "datapath: ", toString(myimage()$datapath)
  )})
  
  output$distributionplot <- renderPlot({
    
    if (input$radio=="1")
    {
      widt=(as.numeric(as.vector(testresult()$faceRectangle.width)))
      heig=(as.numeric(as.vector(testresult()$faceRectangle.height)))
      g<-qplot(faceRectangle.width*faceRectangle.height, data=searchframe, geom="histogram", fill=..count..)+geom_vline(xintercept = widt*heig, colour="red", linetype = "longdash")
      g<-g+annotate("text", x = widt*heig, y = 20, label = "You are here")
      #g<-ggplot(searchframe,aes(faceRectangle.width*faceRectangle.height))+geom_histogram()+scale_colour_gradientn(colours = terrain.colors(10))
      print(g)
    }
    else if (input$radio=="2")
    {
      g<-ggplot(searchframe,aes(x=faceAttributes.gender,fill=faceAttributes.gender))+geom_bar(width = 1)+coord_polar()
      print (g)
    }
    else if (input$radio=="3")
    {
      myage=(as.numeric(as.vector(testresult()$faceAttributes.age)))
      g<-qplot(faceAttributes.age, data=searchframe, geom="histogram", fill=..count..)+geom_vline(xintercept = myage, colour="green", linetype = "longdash")
      
      g<-g + scale_fill_continuous(low="black", high="pink")
      g<-g+annotate("text", x = myage, y = 100, label = "You are here")
      print(g)
    }
    else if (input$radio=="4")
    {
     # print(levels(testresult()$faceAttributes.facialHair.beard))
      g<-ggplot(searchframe, aes(faceAttributes.facialHair.beard,fill=faceAttributes.facialHair.beard))+geom_bar()+geom_bar(data=searchframe[(searchframe$faceAttributes.facialHair.beard==levels(testresult()$faceAttributes.facialHair.beard)[1]),],aes(faceAttributes.facialHair.beard), alpha=0, size=1, color="black")
      print (g)
    }
    else if (input$radio=="5")
    {
      tempframe<-melt(searchframe[c('scores.happiness','scores.anger','scores.fear','scores.sadness')])
      checklist<-input$emotioncheck
      if (length(checklist)>0)
      {
      resultframe<-data.frame()
     
      if ("smile" %in% checklist)
      {
        #red
        smile=as.numeric(testresult()$scores.happiness[1])
        resultframe<-rbind(resultframe,filter(tempframe,variable=='scores.happiness'))
        q1<-geom_vline(xintercept = smile, colour="red", linetype = "longdash")
      }
      
      if ("angry" %in% checklist)
      {
        #green
        angry=as.numeric(testresult()$scores.anger[1])
        resultframe<-rbind(resultframe,filter(tempframe,variable=='scores.anger'))
        q2<-geom_vline(xintercept = angry, colour="green", linetype = "longdash")
      } 
      
      if ("fear" %in% checklist)
      {
        #blue
        fear=as.numeric(testresult()$scores.fear[1])
        resultframe<-rbind(resultframe,filter(tempframe,variable=='scores.fear'))
        q3<-geom_vline(xintercept = fear, colour="blue", linetype = "longdash")
      }
      
      if ("sad" %in% checklist)
      {
        #pupple
        sad=as.numeric(testresult()$scores.sadness[1])
        resultframe<-rbind(resultframe,filter(tempframe,variable=='scores.sadness'))
        q4<-geom_vline(xintercept = sad, colour="purple", linetype = "longdash")
      }
      
      g<-ggplot(resultframe,aes(x=value,colour=variable))+geom_freqpoly()
      if ("smile" %in% checklist)
      {
        g<-g+q1
      }
      if ("angry" %in% checklist)
      {
        g<-g+q2
      }
      if ("fear" %in% checklist)
      {
        g<-g+q3
      }
      if ("sad" %in% checklist)
      {
        g<-g+q4
      }
      
      print(g)
      }
     
    }
   })
  
   output$featuretext<-renderTable({ 
     
     if (input$radio=="1")
     {
       summary(searchframe[c('faceRectangle.width','faceRectangle.height')])
     }
     else if (input$radio=="2")
     {
       summary(searchframe['faceAttributes.gender'])
     }
     else if (input$radio=="3")
     {
       summary(searchframe['faceAttributes.age'])
     }
     else if (input$radio=="4")
     {
       summary(searchframe['faceAttributes.facialHair.beard'])
     }
     else if (input$radio=="5")
     {
       checklist<-input$emotioncheck
       if (length(checklist)>0)
       {
         resultlist=c()
       if ("smile" %in% checklist)
       {
         resultlist=c(resultlist,'scores.happiness')
       }
       if ("angry" %in% checklist)
       {
         resultlist=c(resultlist,'scores.anger')
       }
       if ("fear" %in% checklist)
       {
         resultlist=c(resultlist,'scores.fear')
       }
       if ("sad" %in% checklist)
       {
         resultlist=c(resultlist,'scores.sadness')
       }
         summary(searchframe[resultlist])
       }
     }
     })
  
  #face area #hist smile angry fear sad
  #faceAttributes.gender #pie
  #faceAttributes.age #hist
  #faceAttributes.facialHair.beard,  #bar
  #faceAttributes.facialHair.sideburns  #bar
  #emotion density
  
})
