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
    setkey(df, parcel_id)
    df <- df %>% merge(parcels, all.x=TRUE) 
    setkey(df, building_type_id)
    df %>% merge(building_types, all.x=TRUE)
  })
  
  subset.data <- reactive({
    data <- bldg.data()
    if (is.null(data)) return()
    subdata <- if(input$timefilter == "all") data[year_built <= as.integer(input$year)] else 
                  data[year_built == input$year]
    subdata <- subdata[building_type_id %in% as.integer(input$BTfilter)]
    if (is.null(subdata)) return()
    
    if(input$color %in% c("sizeres", "sizenonres")) {
      values <- log(subdata[[color.attributes[input$color]]]+1)
      palette.size <- colorQuantile("YlOrRd", range(values), n=9)
      palette.name <- "palette.size"
    } else {
      palette.name <- paste0("palette.", input$color)
      values <- subdata[[color.attributes[input$color]]]
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
  
  ####
  # Code for the mix-use tab
  # select which run to use
  output$select_run_mixuse <- renderUI({
      select.run <- list.dirs(base.ind.dir, full.names = FALSE, recursive = FALSE)
      selectInput(inputId = "run_mu",
                  label = "Run",
                  choices = select.run,
                  width = "100%")
  })
  
  # display initial map
  output$map_mixuse <- renderLeaflet({
      leaflet.blank()
  })
  
  # read buildings indicator data and aggregate to parcels
  pcl.data <- reactive({
      dir <- file.path(base.ind.dir, input$run_mu, "indicators")
      file <- list.files(dir, "building__dataset_table__new_buildings__2040.tab", full.names = TRUE)
      if(length(file) == 0) return(NULL)
      df <- fread(file, sep="\t", header = TRUE)
      setkey(df, parcel_id)
      if(is.null(df$building_sqft)) df[, building_sqft:=NA]
      pclbld <- df[, .(new_res_units=sum(residential_units),
                       new_nonres_sqft=sum(non_residential_sqft),
                       new_price=median(unit_price),
                       new_building_sqft=sum(building_sqft)), 
                   by = parcel_id]
      pcl <- merge(parcels, pclbld)
      mixpcl <- pcl[N_res_con > 1 & N_nonres_con > 1]
      mixpcl[is.na(new_res_units), new_res_units:=0]
      mixpcl[is.na(new_nonres_sqft), new_nonres_sqft:=0]
      mixpcl[, nonres_share:= new_nonres_sqft/new_building_sqft]
      mixpcl[new_nonres_sqft > 0 | new_res_units > 0]
  })
  
  # display markers
  observe({
      data <- pcl.data()
      if (is.null(data)) return()
      palette.share <- colorQuantile("Spectral", c(0,1), n=9)
      data[, color := NA]
      if(nrow(data) > 0) 
          data$color[] <- do.call("palette.share", list(data[, nonres_share]))
      marker.popup <- ~paste0("Parcel ID:  ", parcel_id, 
                              "<br>Non-res percent: ", as.integer(100*nonres_share),
                              "<br>DU:            ", as.integer(new_res_units),
                              "<br>Residen sqft:  ", as.integer(new_building_sqft-new_nonres_sqft),
                              "<br>Maximum DU/A:  ", round(max_dua,2),
                              "<br>Non-res sqft:  ", as.integer(new_nonres_sqft),
                              "<br>Maximum FAR:   ", round(max_far,2),
                              "<br>Unit price:    ", round(new_price, 2))
      leaflet.results(leafletProxy("map_mixuse"), data, marker.popup, add=FALSE)
  })
}# end server function






