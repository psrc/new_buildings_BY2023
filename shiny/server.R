function(input, output, session) {

  # functions ---------------------------------------------------------------
  
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
  leaflet.results <- function(proxy, selected.data, popup) {
    proxy %>% 
      clearMarkers() %>%
      addMarkers(data = selected.data,
                 ~long,
                 ~lat,
                 popup = popup
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
    files <- list.files(dir, "building__dataset_table__new_buildings__*", full.names = TRUE)
    df <- NULL
    for(f in files) {
      if(length(grep("meta", f)) > 0) next
      df <- rbind(df, read.csv(f))
    }
  })
  
  
}# end server function






