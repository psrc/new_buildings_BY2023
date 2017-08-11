function(input, output, session) {

  # functions ---------------------------------------------------------------
  
  # Color palette
  palette <- colorNumeric(c("black", "blue", "green", "yellow"), 1:26)
  
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
  leaflet.results <- function(proxy, selected.data, popup, add=FALSE, col="black") {
    if(!add) proxy <- proxy %>% clearMarkers()
    proxy %>% 
      addCircleMarkers(data = selected.data,
                 ~long,
                 ~lat,
                 radius = 3,
                 popup = popup,
                fillOpacity=0.4,
                color=col
                #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE)
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
  
  # display markers
  observe({
    data <- bldg.data()
    if (is.null(data)) return()
    years <- 2015:2040
    subdata <- subset(data, year_built == input$year)
    if (is.null(subdata)) return()
    #browser()
    marker.popup <- ~paste0("Parcel ID:  ", parcel_id, 
                            "<br>Bld ID:     ", as.integer(building_id), 
                            "<br>Year built: ", as.integer(year_built),
                            "<br>Bld type:   ", building_type_name,
                            "<br>DU:         ", as.integer(residential_units.x),
                            "<br>DU pcl base: ", as.integer(residential_units.y),
                            "<br>Non-res sf: ", as.integer(non_residential_sqft),
                            "<br>NR pcl base: ", as.integer(nonres_building_sqft),
                            "<br>Unit price: ", round(unit_price, 2))
    leaflet.results(leafletProxy("map"), subdata, marker.popup, add=isolate(input$cummulate), 
                    col=palette(which(years == input$year)))
  })
  
  # Clear map
  observeEvent(input$clear, {
    leafletProxy("map") %>% clearMarkers()
  })
}# end server function






