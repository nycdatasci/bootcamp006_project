shinyServer(function(input, output, session){
    values<-reactiveValues(starting=T)
    session$onFlushed(function() {  values$starting <- F})

    # observe session
    
    observe({
      if (input$selected %in% noSectors)
      updateSelectizeInput(session,'sector',selected='all')
    })
    
    observe({
      Sys.sleep(0.1)
      selected <- input$selected
      updateSliderInput(session,'slider1',value=startYear[[selected]],min=startYear[[selected]],max=endYear[[selected]])
    })
    
    observe({
      nonCorr<-input$heatX != input$heatY
      Sys.sleep(0.1)
      if (grepl('growth',input$selected) & nonCorr)
          updateSelectizeInput(session,inputId='heatR',choices=c('absolute'))
      else if (!grepl('growth',input$selected) & nonCorr) {updateSelectizeInput(session,inputId='heatR',choices=heatRelative)}
      else {updateSelectizeInput(session,inputId='heatR',choices=c('time series'))}
    })
    
    observe({
      Sys.sleep(0.1)
      isInside <- input$heatY %in% c(heatYChoices,input$heatX)
      if (input$heatX!='sectors' & isInside)
          { updateSelectizeInput(session,'heatY',choices=unique(c(heatYChoices,input$heatX)),selected=input$heatY) }
      else if (input$heatX != 'sectors' & !isInside) {
        updateSelectizeInput(session,'heatY',choices=unique(c(heatYChoices,input$heatX)))}
      else { updateSelectizeInput(session,'heatY',choices=c('sectors')) }
    })

    observe({
      Sys.sleep(0.1)
      if (input$heatX==input$heatY)
        updateSelectizeInput(session,'heatR',label=ts_cs,choices=c('time series'))#,'cross sectional'))
      else if (!grepl('growth',input$selected)) {
        originalOrElse<-ifelse(input$heatR%in%heatRelative,input$heatR,heatRelative[1])
        updateSelectizeInput(session,'heatR',label=rela_abso,choices=heatRelative,selected=originalOrElse)}
      
    })
    
    observe({ updateSelectizeInput(session,'coordY',label='Y coordinate',setdiff(sectorsAbbreviations,input$coordX),selected='Finance') })
    observe({ updateSelectizeInput(session,'stateB',label='second state',setdiff(states,input$stateA),selected='New York') })
    
    observe({
      Sys.sleep(0.1)
      if (input$heatX==input$heatY) 
      {output$title4<-renderText(paste(input$heatX,'vs',input$heatY, "Correlation"))}  
      else if (!('years' %in% c(input$heatX,input$heatY)) & input$heatR!='time series')
      { output$title4<-renderText(paste0("Now it is year ",input$slider1)) }
      else { output$title4<-renderText(paste(input$heatX,'vs Years 2D HEAT MAP'))}
    })
    
    # reactive 
    
    getYear<-reactive({ 
      if (values$starting) return('X2015')
      year<-as.numeric(input$slider1)
      if (startYear[[input$selected]]>year) {year<-startYear[[input$selected]]}
      if (endYear[[input$selected]]<year) {year<-endYear[[input$selected]]}
      return(paste0('X',year))}
      )
    
    getDataStates<-reactive({
      if (!(input$selected %in% names(data))) return(NULL)
      return(data[[input$selected]])
    })

    getDataRegions<-reactive({
      if (!(input$selected %in% names(GDP_regions))) return(NULL)
      return(GDP_regions[[input$selected]])
      
    })
    
    getSectorRGDPSectorDataStates<-reactive({
      if (!(input$sector %in% names(rGDP_sector_states))) return(NULL)
      return(list(original=rGDP_sector_states[[input$sector]],
                  growth=rGDP_sector_states_growth[[input$sector]]))
    })
    
    getSectorNGDPSectorDataStates<-reactive({
      if (!(input$sector %in% names(nGDP_sector_states))) return(NULL)
      return(list(original=nGDP_sector_states[[input$sector]],
                  growth=nGDP_sector_states_growth[[input$sector]]))
    })
    
    getSectorRGDPSectorDataRegions<-reactive({
      if (!(input$sector %in% names(rGDP_sector_regions))) return(NULL)
      return(list(original=rGDP_sector_regions[[input$sector]],
                  growth=rGDP_sector_regions_growth[[input$sector]]))
      
    })
        
    getSectorNGDPSectorDataRegions<-reactive({
      if (!(input$sector %in% names(nGDP_sector_regions))) return(NULL)
      return(list(original=nGDP_sector_regions[[input$sector]],
                  growth=nGDP_sector_regions_growth[[input$sector]]))
      
    })
    
    getDataFrame<-reactive({
      
      if (input$selected %in% noRegions | input$selected %in% noSectors | input$sector=='all')
        DF<-getDataStates()
      else { if (grepl('nominal',input$selected)) 
        myList<-getSectorNGDPSectorDataStates()
      else if (grepl('real',input$selected))
        myList<-getSectorRGDPSectorDataStates()
      
      if (is.null(myList)) return(NULL)
      if (input$sector!='all' & grepl('growth',input$selected))
        DF<-myList[['growth']]
      else DF<-myList[['original']] }
      DF
    })
    
    ########## Real Codes begin here
    
    output$title1<-renderText(US_GDP_Visual)
    output$title2<-renderText('US GDP States/Regions Time Series Visualization')

    output$projection<-renderText("<font size = '4' color = #FFFFFF>Which sectors to project to:</font>")
    output$whichStates<-renderText("<font size = '4' color= #FFFFFF>Which States to use?</font>")
    
    
    output$linePlot <- renderGvis({
      if (values$starting) return(NULL)
      Sys.sleep(0.3)
      output$warning <- renderText(paste('No data to display for',input$selected,'with the current state/region choices'))
      selected <- input$selected
      if (input$state1=='None' & input$region1 == 'None') return(NULL)
      myStates  = c()
      myRegions = c()
      selected <- input$selected
      if (input$sector=='all') {
          if (input$state1=='None') {myData<-getDataRegions()}
          else {myData<-getDataStates()}
      } else 
      {
        if (input$state1=='None') {
           if (input$region1=='None') return(NULL)
           if (grepl('real',selected)) myList<-getSectorRGDPSectorDataRegions()
           else myList<-getSectorNGDPSectorDataRegions() }
        else {
          if (grepl('real',selected)) myList<-getSectorRGDPSectorDataStates()
          else myList<-getSectorNGDPSectorDataStates()
        }
        growToken <- ifelse(grepl('growth',selected),'growth','original')
        myData <- myList[[growToken]]
      }  

      if (is.null(myData)) return(NULL)
      year_e   <- endYear[[selected]]
      Xyear    <- getYear()
      year_s   <- max(c(strtoi(sub('X','',Xyear)),startYear[[selected]]))
      
      DF<-myData[,c('GeoName',paste0('X',seq(year_s,year_e)))]
  
      if (input$state1 != 'None' & !is.null(input$state2))  {myStates=unique(c(input$state1,input$state2))}
      else if (input$state1 != 'None') {myStates=input$state1}
      else if (input$region1 != 'None' & !is.null(input$region2)) {myRegions=unique(c(input$region1,input$region2))}
      else if (input$region1 != 'None') {myRegions=input$region1}
      else return(NULL)
      if(length(myStates)>0){myColumns=myStates}
      else {myColumns=myRegions}
      
      P<-DF %>% filter(GeoName %in% myColumns)
      myColumns <- P[,'GeoName']
      Y<-data.frame(t(P[,-1]))
      if (length(myStates)>0) names(Y) <- FindStateAbbreviation(myColumns)
      else names(Y)<-FindRegionAbbreviation(myColumns)
      
      Y['year']<-as.integer(seq(year_s,year_e))
      output$warning<-renderText(paste(myColumns,collapge='',input$selected,'Time Series:'))
      yTitle <- ifelse(grepl('growth',selected),paste(selected,'percentage'),selected)
      if (grepl('growth',selected)) myOption <- list(vAxis="{format:'#,###%'}")
      else myOption <- list()
      gvisLineChart(Y,xvar='year',yvar=names(Y[-ncol(Y)]),options=myOption)
    })   
    
    output$map <- renderGvis({
      if (values$starting) return(NULL)
      Sys.sleep(0.3)
 
      DF<-getDataFrame()
      DF1<-DF[,getYear()]
      DF1<-TreatAsMissing(DF1,bound=ifelse(grepl('growth',input$selected),5,1e16))
      maxV<-max(DF1)
      minV<-min(DF1)
      medV<-median(DF1)
      colorStr=paste("{values:[",minV,",",medV,",",maxV,"],")
      gvisGeoChart(DF, "GeoName", getYear(),
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces", width='1200px',height='700px',
                                backgroundColor='#F6E3CE',colorAxis=paste0(colorStr,"colors:['red','#FBEFEF','#0404B4']}")))
    })   
    
     output$table1 <- renderGvis({
       if (values$starting) return(NULL)
      Sys.sleep(0.3)
      Xyear <- getYear()
      selected <- input$selected
      if (selected %in% noRegions) {
        DF<-getDataStates()
        if (is.null(DF)) return(NULL)
        DF<-DF[,c('GeoName',Xyear)]
        DF<-DF[order(DF[[Xyear]],decreasing=T),]
        DF<-head(DF,10)
        hTitle = paste0("[{title:'top ",selected,"'}]")
        output$title1<-renderText(US_GDP_Visual)
      }
      else if (grepl('growth$',selected)) {
        DF<-getDataRegions()
        if (is.null(DF)) return(NULL)
        DF<-DF[,c('GeoName',Xyear)]
        hTitle = "[{title:'percentage of U.S. GDP growth'}]"
        US_GDP<-ConvertNum(DF[1,2])
        output$title1<-renderText(paste(US_GDP_Visual,'----US',input$selected,US_GDP))
      }
      else if (selected != 'per capita real GDP') {
        DF<-getDataRegions()
        if (is.null(DF)) return(NULL)
        US_GDP<-ConvertNum(DF[1,Xyear])
        DF<-DF[,c('GeoName',Xyear)]
        DF[,Xyear] <- DF[,Xyear]/DF[1,Xyear]
        DF<-DF[order(DF[Xyear],decreasing=T),]
        hTitle     = "[{title:'percentage contributing to U.S. GDP'}]"
        DF<-DF[-1,]
        output$title1<-renderText(paste(US_GDP_Visual,'----US',input$selected,US_GDP))
      }
      else {
        DF<-getDataRegions()
        if (is.null(DF)) return(NULL)
        US_GDP<-ConvertNum(DF[1,Xyear])
        DF<-DF[,c('GeoName',Xyear)]
        DF<-DF[order(DF[Xyear],decreasing=T),]
        hTitle     = "[{title:'percentage contributing to U.S. GDP'}]"

        output$title1<-renderText(paste(US_GDP_Visual,'----US',input$selected,US_GDP))
      }

      DF[,Xyear]<-ConvertNum(DF[,Xyear])
      names(DF) <- c('GeoName',sub('X','',Xyear))
      gvisTable(DF,options=list(title=hTitle,page='enable',height=305,width=200))
    })

    output$barPlot <- renderGvis({
      if (values$starting) return(NULL)
      Sys.sleep(0.3)
      Xyear <- getYear()
      selected <- input$selected
      if (selected %in% noRegions) {
        DF<-data[[selected]][,c('GeoName',Xyear)]
        DF<-DF[order(DF[[Xyear]],decreasing=T),]
        DF<-head(DF,10)
        DF$GeoName <- FindStateAbbreviation(DF$GeoName)
        hTitle = paste0("[{title:'top ",selected,"'}]")
      }
      else if (grepl('growth$',selected)) {
        DF<-GDP_regions[[selected]][,c('GeoName',Xyear)]
        DF[Xyear]<-DF[Xyear]*100.0
        hTitle = "[{title:'U.S. GDP growth percentage'}]"
      }
      else if (selected != 'per capita real GDP') {
        DF<-GDP_regions[[selected]][,c('GeoName',Xyear)]
        DF[,Xyear] = DF[,Xyear]/DF[1,Xyear]*100.0
        hTitle     = "[{title:'percentage contributing to U.S. GDP'}]"
        DF<-DF[-1,]
      } else {
        DF<-GDP_regions[[selected]][,c('GeoName',Xyear)]
        hTitle     = "[{title:'U.S. per capita real GDP'}]"
        DF<-DF[-1,]
      }

      names(DF) <- c('GeoName','percentage')
      DF <- arrange(DF,desc(percentage))
      gvisBarChart(DF,xvar='GeoName',yvar='percentage',options=list(hAxes=hTitle,height=305))
    })

    # show statistics using infoBox
    output$maxBox <- renderInfoBox({
        if (values$starting) { Xyear<-'X2007'}
        else { Xyear = getYear() }
        DF<-getDataFrame()
        DF<-TreatAsMissing(DF,bound=ifelse(grepl('growth',input$selected),5,1e16))
        outData <- DF[,Xyear]
        
        max_value <- max(outData,na.rm=T)
        max_state <- FindStateAbbreviation(DF$GeoName[outData == max_value])
        infoBox(max_state, ConvertNum(max_value), icon = icon("chevron-up"),color='blue')
    })
    output$minBox <- renderInfoBox({
        if (values$starting) { Xyear='X2007'}
        else { Xyear = getYear() }
        DF<-getDataFrame()
        DF<-TreatAsMissing(DF,bound=ifelse(grepl('growth',input$selected),5,1e16))
        outData <- DF[,Xyear]
        min_value <- min(outData,na.rm=T)
        min_state <- FindStateAbbreviation(DF$GeoName[outData == min_value])
        infoBox(min_state, ConvertNum(min_value), icon = icon("chevron-down"),color='red')
    })
    output$medBox <- renderInfoBox({
      
        if (values$starting) { Xyear='X2007'}
        else { Xyear = getYear() }
        DF<-getDataFrame()
        DF<-TreatAsMissing(DF,bound=ifelse(grepl('growth',input$selected),5,1e16))
        outData <- DF[,Xyear]
        infoBox(paste("MEDIAN", input$selected),
                ConvertNum(median(outData,na.rm=T)),
                icon = icon("calculator"),color='green')
    })
    
    
    
    output$bubble <- renderBubbles({
      
      if (!(input$selected %in% names(data))) return(NULL)
    
      Xyear <- getYear()
      DF<-getDataFrame()
      DF <-DF[,c('GeoName',Xyear)]
      if (!is.numeric(DF[[Xyear]])) DF[[Xyear]]<-type.convert(DF[[Xyear]])
      DF[is.na(DF)]<-0.0
      DF<-DF[order(DF[[Xyear]],decreasing=T),]
      DF<-head(DF,input$slider2)
      minValue <- min(DF[[Xyear]],na.rm=T)
      DF[is.na(DF)]<-minValue-0.1
      if (minValue<0) DF[[Xyear]] <- DF[[Xyear]] - 1.2*min(DF[[Xyear]])
      colors1<-two.colors(input$slider2,start='#0B2161',end='#F5A9F2',middle='#01A9DB',alpha=1.0)
      output$title3<-renderText(paste("Now it is year ",sub("X",'',Xyear)))
      bubbles(sqrt(DF[[Xyear]]), DF$GeoName, key = DF$GeoName, color=colors1,textColor='#FFFFFF')
    })
    
    output$heat<-renderPlot({
      if (values$starting) return(NULL)
      Sys.sleep(0.6)
      heatX<-input$heatX
      if (heatX=='None') {
        output$title4<-renderText("Heat Map X variable cannot be None!")
        return(NULL)
      } else if (heatX=='sectors' & input$heatY!='sectors') {return(NULL)}
        
      heatY<-input$heatY
      correlational<-(heatX==heatY)
      selected<-input$selected
      isReal<-grepl('real',selected)
      isGrowth<-grepl('growth',selected)
      isTimeSeries<-grepl('time',input$heatR)
      
      if (!(selected %in% support_byYear)) {
        output$title4<-renderText("Only support (real or nominal) GDP (.|growth)")
        return(NULL)}
      if (correlational & !isGrowth) {
        output$title4<-renderText("Only support (real or nominal) GDP growth")
        return(NULL)
      }
      # choose the right data to use
      T1<-ifelse(isReal,'rGDP','nGDP')
      T2<-ifelse(grepl('states',heatX),'_states','_regions')
      T3<-ifelse(isGrowth,'_growth','')
      T4<-'_byYear'
      
      if (heatY=='sectors' & !correlational) {
          varName <- paste0(T1,T2,T3,T4)
          DF<-(get(varName)[[getYear()]])
      }
      else if (input$sector=='all' & !correlational) {
          if (heatX=='states') DF <- getDataStates()
          else { DF <- getDataRegions() }
        
      } else if (input$sector!='all' & !correlational) {
          if (heatX=='states' & grepl('real',selected)) {myList<-getSectorRGDPSectorDataStates()}
          else if (heatX=='states' & !isReal) {myList<-getSectorNGDPSectorDataStates()}
          else if (heatX=='regions' & isReal) {myList<-getSectorRGDPSectorDataRegions()}
          else {myList<-getSectorNGDPSectorDataRegions()}
          if (!isGrowth) {DF<-myList[['original']]}
          else { DF<-myList[['growth']]}
      }
      
      #if (!('years' %in% c(heatX,heatY)) & input$heatR!='time series')
      #output$title4<-renderText(paste0("Now it is year ",input$slider1))
      #else { output$title4<-renderText(paste(heatX,'vs Years 2D HEAT MAP'))}
      
      if (correlational) {
        if (isTimeSeries) {
          if (heatX=='states'){
            if (input$sector=='all') { DF<-getDataStates() }
            else {if(isReal) {DF<-getSectorRGDPSectorDataStates()[['growth']]}
                  else {DF<-getSectorNGDPSectorDataStates()[['growth']]}
                 }
          } else if (heatX == 'US regions') {
            if (input$sector=='all') { DF<-getDataRegions()
            } else  { 
                    if (isReal) { DF<-getSectorRGDPSectorDataRegions()[['growth']] }
                    else { DF<-getSectorNGDPSectorDataRegions()[['growth']] }
                    DF<-filter(DF,!grepl('United States',GeoName)) }
            } else if (heatX=='sectors') {
               if (isReal) { X<-rGDP_sector_regions_growth} 
               else { X<- nGDP_sector_regions_growth }

               for (key in names(X)) { X[[key]]<-filter(X[[key]],GeoName=='United States')}
               DF<-StackDFs(X,'Sectors')
            } else {return(NULL)}
        } else {
           # not implemented yet for cross sessional correlations
          if (heatX=='states'){
            
          } else if (heatX=='regions') {
            
          } else if (heatX=='sectors') {
            
            
          }          
        }
         if (!exists('DF')) { #print("DF doesn't exist")
           return(NULL)}
         myScale = 1.0
         if (heatX=='states') { 
           rownames(DF)<-FindStateAbbreviation(DF[,1])
           myScale = 0.7
         }
         else if (heatX =='US regions') {rownames(DF)<-FindRegionAbbreviation((DF[,1]))}
         else if (heatX == 'sectors')  {rownames(DF)<-sectorsAbbreviations}
         DF<-DF[,-1]
         DF<-t(DF)
         corMatrix<-cov2cor(var(DF,na.rm=T))
         return(corrplot(corMatrix,method='circle',tl.cex=myScale,type='lower'))
      }
      
      DF<-TreatAsMissing(DF)
      
      if (input$heatR=='relative') {
           if (input$heatY=='sectors') DF<-NormalizeDFAlongRows(DF)
           else DF<-NormalizeDFAlongColumns(DF)
        }

      DF<-melt(DF,id.var='GeoName')

      if (heatY=='sectors') DF<-transmute(DF,GeoName=factor(GeoName),Sector=factor(variable),value)
      else DF<-transmute(DF,GeoName=factor(GeoName),years=factor(variable),value)
      if (heatX=='US regions') DF<-filter(DF,GeoName!='United States')
      
      if (heatY=='sectors') g <- ggplot(DF, aes(x=GeoName, y=SectorAbbre(Sector),fill=value*100))
      else g <- ggplot(DF,aes(x=GeoName,y=sub('X','',years),fill=value*100))
      g<- g+ geom_tile() + scale_fill_gradientn(colors=c('black','dark red','red','orange','yellow','white')) 
      g<- g + xlab(switch(paste0(heatX,'W'),statesW='States',regionsW='US regions',sectorsW='Sectors'))
      g<- g + ylab(switch(paste0(heatY,'W'),statesW='States',yearsW='Years',regionsW='US regions',sectorsW='Sectors'))
      g<- g+ theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) +labs(fill='GDPGrowth %')
      return(g)                                                                                                                                                                      
      
    })

    output$bubble2<-renderGvis({
      if (values$starting) return(NULL)
      Sys.sleep(0.3)
      
      selected<-input$selected
      isStates<-T
      isReal  <- grepl('real',selected)
      if (!grepl('growth',selected)) {
        output$titleB<-renderText('Support real or nominal GDP growth only')
        return(NULL)}
      else {output$titleB<-renderText(paste0('Motion Chart of ',input$selected))}
      
      decisionToken = ''
      if (isReal  & isStates)  decisionToken <- 'rs'
      if (isReal  & !isStates) decisionToken <- 'ru'
      if (!isReal & isStates)  decisionToken <- 'ns'
      if (!isReal & !isStates) decisionToken <- 'nu'
      x<-c('r','n')
      names(x)<-x
      y<-c('states','regions')
      names(y)<-c('s','u')
      decisionVec <-strsplit(decisionToken,split='')[[1]]
      dataName <- paste0(x[decisionVec[1]],'GDP_sector_',y[decisionVec[2]],'_growth')
      
      motion_data <- get(dataName,inherits=T)
      coordX<-sectors[input$coordX]
      coordY<-sectors[input$coordY]
      X<-motion_data[[coordX]]
      Y<-motion_data[[coordY]]
      
      Aloc<-input$stateA
      Bloc<-input$stateB
      locPair<-c(Aloc,Bloc)
      if(length(unique(locPair))!=2) return(NULL)
      X<-filter(X,GeoName %in% locPair)
      Y<-filter(Y,GeoName %in% locPair)
      X1<-melt(X,id.var='GeoName')
      Y1<-melt(Y,id.var='GeoName')
      names(X1)<-c('GeoName','Year',input$coordX)
      names(Y1)<-c('GeoName','Year',input$coordY)
      Z<-inner_join(X1,Y1,by=c('GeoName','Year'))
      Z$Year<-type.convert(sub("X",'',Z$Year))
      Z$Color<-factor(Z$GeoName)
      Z$GeoName<-factor(Z$GeoName)
      
      return(gvisMotionChart(Z,idvar='GeoName',timevar='Year', colorvar='Color',
                             xvar=input$coordX,yvar=input$coordY,options=list(width='760px',height='300px')))
      
    })
    
})
