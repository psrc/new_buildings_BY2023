mu.select.indicators <- c("Non-residential Share"="share",
                          "Maximum DU/Acre"="dua",
                          "Maximum FAR"="far",
                          "Residential sqft"="ressqft",
                          "Non-residential sqft"="nonressqft",
                          "Unit price"="price"
                            )

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
                               checkboxInput("mixuse.chb", "Mix-use parcels", FALSE),
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
           ), # end tabPanel
           tabPanel("View Mix-Use",
                fluidPage(
                    fluidRow(
                        column(width = 3,
                            uiOutput("select_run_mixuse"),
                            selectInput("MUindicator", "Indicator", 
                                        mu.select.indicators,
                                        selected = "share"),
                            "Exclude where not allowed:",
                            checkboxInput("office", "Office"),
                            checkboxInput("comm", "Commercial"),
                            checkboxInput("mfr", "Multi-family"),
                            "Dots show new development in mix-use parcels. They are color-scaled by the chosen indicator, with blue being the smallest value and red being the largest value."
                        ), # end column
                        column(width = 9,
                          tabsetPanel(
                            tabPanel("Map",
                               leafletOutput("map_mixuse", height = "800px")
                               ),
                            tabPanel("Plots",
                                column(width = 6,
                                selectInput("MUindicatorX", "X axis", 
                                            mu.select.indicators,
                                            selected = "far")
                                ),
                                column(width = 6,
                                selectInput("MUindicatorY", "Y axis", 
                                            mu.select.indicators,
                                            selected = "share")
                                ),
                                plotOutput("MUplot")
                                #)
                            ))
                        ) # end column
                    ) # end fluidRow
                ) # end fluidPage
           ) # end tabPanel
) # end navbarPage