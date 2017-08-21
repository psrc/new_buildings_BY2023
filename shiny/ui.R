navbarPage(theme = shinytheme("simplex"),
           "Development Projects",
           tabPanel("View by Time",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                      fluidRow(
                        column(width = 3,
                               uiOutput("select_run"), # dynamic, lists runs on modelsrv8
                               sliderInput(inputId = "year",
                                           label = "Year",
                                           min = 2014,
                                           max = 2040,
                                           value = 2014,
                                           step = 1,
                                           sep = "", 
                                           animate = animationOptions(interval=3000)),
                               hr(),
                               h5("Filters"),
                               #fluidRow(
                                 #column(width = 9,
                                        #checkboxInput("cummulate", "Cummulative", TRUE)),
                                        selectInput("timefilter", "Time", 
                                                    c("Single year"="single", "Add by year"="cummulative", 
                                                      "All <= year"="all"),
                                                    selected = "single"),
                                        selectInput("BTfilter", "Building Type", 
                                                    structure(building_types_selection[,1],
                                                              names=rownames(building_types_selection)),
                                                    multiple = TRUE, selected = building_types_selection[,1]),
                               hr(),
                               selectInput("color", "Color By", 
                                           c("Year"="year", "Building type"="bt", 
                                             "Size residential"="sizeres", 
                                             "Size non-residential"="sizenonres"),
                                           selected = "year"),
                               #),
                                 #column(width = 2,
                                        actionButton("clear", "Clear Map")
                                 #)
                               #),
 
                               

                        ),
                        column(width = 9,
                               leafletOutput("map", height = "800px")
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ) # end tabPanel
) # end navbarPage