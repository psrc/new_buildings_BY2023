navbarPage(theme = shinytheme("simplex"),
           "Development Projects",
           tabPanel("View by Time",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                      shinyjs::useShinyjs(),
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
                               selectInput("timefilter", "Time", 
                                                    c("Single year"="single", 
                                                      "All <= year"="all", "Add by year"="cummulative"),
                                                    selected = "single"),
                                selectInput("BTfilter", "Building Type", 
                                                    structure(building_types_selection[,1],
                                                              names=rownames(building_types_selection)),
                                                    multiple = TRUE, selected = building_types_selection[,1]),
                               hr(),
                               h5("Style"),
                               selectInput("color", "Color By", 
                                           c("Year"="year", "Building type"="bt", 
                                             "Size residential"="sizeres", 
                                             "Size non-residential"="sizenonres"),
                                           selected = "year"),
                               checkboxInput("cluster", "Show in clusters", FALSE),
                               actionButton("clear", "Clear Map"),
                               hr()
                        ),
                        column(width = 9,
                               leafletOutput("map", height = "800px")
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ) # end tabPanel
) # end navbarPage