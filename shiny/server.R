function(input, output, session) {

  # functions ---------------------------------------------------------------
  
  # Color palette
  palette.year <- colorNumeric(c("black", "blue", "green", "yellow"), 2015:2040)
  palette.bt <- colorFactor(rainbow(nrow(building_types_selection)), 
                              levels=building_types_selection[,1])

  # enable/disable color selection depending on clustering
  observeEvent(input$cluster, {
    shinyjs::toggleState("color", input$cluster == FALSE)
  })
  
  # reset/default map
  leaflet.blank <- function() {
    leaflet() %>%
      #addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addLayersControl(
        baseGroups = c("Street Map", "Imagery")
      ) %>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9) %>%
      addEasyButton(
        easyButton(
          icon="fa-globe", 
          title="Zoom to Region",
          onClick=JS("function(btn, map){ 
                     map.setView([47.549390, -122.008546],9);}"))
          )
  }
  
  # show leaflet results
  leaflet.results <- function(proxy, selected.data, popup, add=FALSE, cluster = FALSE) {
    if(!add) proxy <- proxy %>% clearMarkers() %>% clearMarkerClusters()
    cluster.options <- NULL
    if(cluster) cluster.options <- markerClusterOptions()
    proxy %>% 
      addCircleMarkers(data = selected.data,
                 ~long,
                 ~lat,
                 radius = 3,
                 popup = popup,
                fillOpacity=0.4,
                color = ~color, 
                clusterOptions = cluster.options
      ) 
  }  
  
  # display initial map
  output$map <- renderLeaflet({
    leaflet.blank()
  })

  # select which run to use
  output$select_run <- renderUI({
    select.run <- list.dirs(base.ind.dir, full.names = FALSE, recursive = FALSE)
    selectInput(inputId = "run",
                label = "Run",
                choices = select.run,
                width = "100%")
  })

  # read buildings indicator data 
  bldg.data <- reactive({
    dir <- file.path(base.ind.dir, input$run, "indicators")
    file <- list.files(dir, "building__dataset_table__new_buildings__2040.tab", full.names = TRUE)
    if(length(file) == 0) return(NULL)
    df <- fread(file, sep="\t", header = TRUE)
    df %>% left_join(parcels.attr, by = "parcel_id") %>% left_join(building_types, by="building_type_id")
  })
  
  subset.data <- reactive({
    data <- bldg.data()
    if (is.null(data)) return()
    subdata <- if(input$timefilter == "all") subset(data, year_built <= as.integer(input$year)) else 
                  subset(data, year_built == input$year)
    subdata <- subset(subdata, building_type_id %in% as.integer(input$BTfilter))
    if (is.null(subdata)) return()
    
    if(input$color %in% c("sizeres", "sizenonres")) {
      values <- log(subdata[[color.attributes[input$color]]]+1)
      palette.size <- colorQuantile("YlOrRd", range(values), n=9)
      palette.name <- "palette.size"
    } else {
      palette.name <- paste0("palette.", input$color)
      values <- data[[color.attributes[input$color]]]
    }
    subdata$color <-rep(NA, nrow(subdata))
    if(nrow(subdata) > 0) subdata$color[] <- do.call(palette.name, list(values))
    subdata
  })
  
  subset.data.deb <- subset.data %>% debounce(1000) # causes some delay for collecting inputs
  
  # display markers
  observe({
    data <- subset.data.deb()
    if (is.null(data)) return()
    marker.popup <- ~paste0("Parcel ID:  ", parcel_id, 
                            "<br>Bld ID:     ", as.integer(building_id), 
                            "<br>Year built: ", as.integer(year_built),
                            "<br>Bld type:   ", building_type_name,
                            "<br>DU:         ", as.integer(residential_units.x),
                            "<br>DU pcl base: ", as.integer(residential_units.y),
                            "<br>Non-res sf: ", as.integer(non_residential_sqft),
                            "<br>NR pcl base: ", as.integer(nonres_building_sqft),
                            "<br>Unit price: ", round(unit_price, 2))
    leaflet.results(leafletProxy("map"), data, marker.popup, 
                    add = input$timefilter == "cummulative" && !input$cluster,
                    cluster = input$cluster)
  })
  
  # Clear map
  observeEvent(input$clear, {
    leafletProxy("map") %>% clearMarkers() %>% clearMarkerClusters()
  })
}# end server function






