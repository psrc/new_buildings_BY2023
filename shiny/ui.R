navbarPage(theme = shinytheme("simplex"),
           "Development Projects",
           tabPanel("View by Time",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                      fluidRow(
                        column(width = 3,
                               uiOutput("select_run"), # dynamic, lists runs on modelsrv8
                               fluidRow(
                                 column(width = 7,
                                        checkboxInput("cummulate", "Cummulative", TRUE)),
                                 column(width = 2,
                                        actionButton("clear", "Clear")
                                 )
                               ),
                               sliderInput(inputId = "year",
                                           label = "Year",
                                           min = 2014,
                                           max = 2040,
                                           value = 2014,
                                           step = 1,
                                           sep = "", 
                                           animate = animationOptions(interval=3000))

                        ),
                        column(width = 9,
                               leafletOutput("map", height = "725px")
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ) # end tabPanel
) # end navbarPage