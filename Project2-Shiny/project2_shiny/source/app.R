{
tabPanelAbout <- source("source/intro.R", local=T)$value

output$tp.gettingstartedside<- renderUI({ if (is.null(myimage)) {sidebarPanel(fileInput('file1', 'Choose image to upload',
                                                    accept = c(
                                                      '.png',
                                                      '.jpg',
                                                      '.gif'
                                                    )))} else{
                                                      h4("Congratulation!")
                                                    }
  })

output$tp.facematchside<- renderUI({})

output$tp.featureanalysisside<- renderUI({})

output$tp.imageanalysisside<- renderUI({})

output$tp.findfaceside<- renderUI({})

}