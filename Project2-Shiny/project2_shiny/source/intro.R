fluidRow(
  column(10,tabPanel("intro",
           HTML('<p style="text-align:justify">This is a simplified shiny project for facial features analysis. Users can upload their own face pictures or log in through other SNS account
                 to get connected to the website database and get the feedback data from server.</p>
                
                <p style="text-align:justify">There are multiple analytic functions for the specific picture just uploaded. The face match column is used for matching your face picture with 
                pictures in database and pick out the very similar faces for you grouped by features like ethnics, locations and genders.The feature analysis part is for the analysis of your
                facial features, like the size of your eyes, the color of your skins and color of hairs. The third column "Image Analysis" aimed to give a deeper vision of the characteristics
                of the picture, the more phisical ones compared to the previous biological ones. The last one would use a slight deep learning technique to search for specific pictures defined 
                by the words the users type. </p>
                
                <p style="text-align:justify">This project may have multiple weaknesses and drawbacks and may not perform as perfectly as expected. But the basic functions should work correctly
                and efficiently. I hope I can add more features the enhance the web page in the future. </p>
                
                '),
           
           
           
           fluidRow(
             column(6,
                    strong('Referenced websites'),
                    p(HTML('<ul>'),
                      HTML('<li>'),a("Random variables: App tutorial part 1", href="http://shiny.rstudio.com/", target="_blank"),HTML('</li>'),
                      HTML('<li>'),a("Random variables: App tutorial part 2", href="http://stackoverflow.com/", target="_blank"),HTML('</li>'),
                      HTML('</ul>')),
                    strong('Code'),
                    p('Source code would be available soon at',
                      a('GitHub', href="https://github.com/sueyhan", target="_blank")),
                    br()
             ),
             column(6,
                    strong('Referenced packages'),
                    p(HTML('<ul>'),
                      HTML('<li>'),a("Random variables: App tutorial part 1", href="https://cran.r-project.org/web/packages/shiny/index.html", target="_blank"),HTML('</li>'),
                      HTML('<li>'),a("Random variables: App tutorial part 2", href="http://www.showmeshiny.com/category/visualization/ggplot2/", target="_blank"),HTML('</li>'),
                      HTML('</ul>')),
                    br()
             )
             
           ),
           
           
           value="intro"
  )),
  
  column(2," ")
)
