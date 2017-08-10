function(input, output, session) {

  # functions ---------------------------------------------------------------
  
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
      ) 
  }  
  
  # format table
  format.table <- function(sSelected) {
    sSelected %>%
      select(-(OBJECTID_12:PIN), -(MAJOR:Shape_Le_1), -(Shape_Length)) %>%
      rename(county = COUNTY,
             bldg_sqft = building_sqft, 
             nonres_bldg_sqft = nonres_building_sqft,
             num_hh = number_of_households,
             num_jobs = number_of_jobs,
             num_bldgs = number_of_buildings,
             res_units = residential_units,
             gwthctr_id = growth_center_id,
             area = AREA,
             shape_area = Shape_Area
      ) %>%
      mutate(shape_area = round(shape_area, 10),
             area = round(area, 2),
             max_dua = round(max_dua, 2),
             max_far = round(max_far, 2),
             lat = round(lat, 4),
             long = round(long, 4)
      ) %>%
      select(county, parcel_id, zone_id, faz_id, gwthctr_id, city_id, area, shape_area, parcel_sqft, bldg_sqft:res_units, lat,long)
  }

 
  # display parcel_ids
  output$map <- renderLeaflet({
    leaflet.blank()
  })
  


  
  # observe({
  #   sSelected <- sSelectedcl()
  #   if (is.null(sSelected) || values.cl$ids == " ") return()
  #   marker.popup <- ~paste0("<strong>Parcel ID: </strong>", as.character(parcel_id))
  #   leaflet.results(leafletProxy("mapc"), sSelected, marker.popup)
  # })
    

  output$select_run <- renderUI({
    select.run <- list.dirs(base.ind.dir, full.names = FALSE, recursive = FALSE)
    selectInput(inputId = "run",
                label = "Run",
                choices = select.run,
                width = "100%")
  })

  bldg.data <- reactive({
    dir <- file.path(base.ind.dir, input$run, "indicators")
    file <- list.files(dir, "building__dataset_table__new_buildings__2040.tab", full.names = TRUE)
    if(length(file) == 0) return(NULL)
    df <- fread(file, sep="\t", header = TRUE)
    df %>% left_join(parcels.attr, by = "parcel_id")
  })
  
  observe({
    data <- bldg.data()
    if (is.null(data)) return()
    years <- 2015:2040
    subdata <- subset(data, year_built == input$year)
    if (is.null(subdata)) return()
    subdata <- subdata %>% left_join(building_types, by="building_type_id")
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
  
  observeEvent(input$clear, {
    leafletProxy("map") %>% clearMarkers()
  })
}# end server function






