function(input, output, session) {

  # functions ---------------------------------------------------------------
  
  # Color palette
  palette.year <- colorNumeric(c("black", "blue", "green", "yellow"), 2015:2050)
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
    proxy %>% addCircleMarkers(data = selected.data,
                               ~lon,
                               ~lat,
                               radius = 3,
                               popup = popup,
                               fillOpacity=0.4,
                               color = ~color, 
                               clusterOptions = cluster.options
      ) %>% #### Adapted from https://redoakstrategic.com/geoshaper/
      addDrawToolbar(targetGroup='Selected',
                     polylineOptions=FALSE,
                     markerOptions = FALSE,
                     polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'red',weight = 3)),
                     rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'red',weight = 3)),
                     circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,color = 'red',weight = 3)),
                     editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())
      )
  }  

  
#### Adapted from https://redoakstrategic.com/geoshaper/ ---------------------  
  
  # store selections for tracking
  data.of.click <- reactiveValues(clickedMarker = list(), # all coordinates within boundaries (unfiltered)
                                  selected = list()) # parcels matching sub.data.deb() (filtered)

  observeEvent(input$map_draw_new_feature, {
    # Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$map_draw_new_feature,
                                    location_coordinates = coordinates,
                                    location_id_colname = "parcel_id")
    
    for(id in found_in_bounds){
      if(id %in% data.of.click$clickedMarker){
        # don't add id
      } else {
        # add id
        data.of.click$clickedMarker <- append(data.of.click$clickedMarker, id, 0)
      }
    }
   
    # look up parcels by ids found
    data.of.click$selected <- subset(subset.data.deb(), parcel_id %in% data.of.click$clickedMarker)
    proxy <- leafletProxy("map")
    proxy %>%
      addCircles(data = data.of.click$selected,
                 lat = data.of.click$selected$lat,
                 lng = data.of.click$selected$lon,
                 fillColor = "mediumseagreen",
                 fillOpacity = 1,
                 color = "mediumseagreen",
                 weight = 3,
                 stroke = T,
                 layerId = as.character(data.of.click$selected$secondLocationID),
                 highlightOptions = highlightOptions(color = "hotpink",
                                                     opacity = 1.0,
                                                     weight = 2,
                                                     bringToFront = TRUE))
  })
  
  observeEvent(input$map_draw_deleted_features,{
    # loop through list of one or more deleted features/ polygons
    for(feature in input$map_draw_deleted_features$features){
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature,
                                         location_coordinates = coordinates,
                                         location_id_colname = "secondLocationID")
      # remove second layer representing selected locations
      proxy <- leafletProxy("map")
      proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(subset.data.deb(), secondLocationID %in% bounded_layer_ids)$locationID
      data.of.click$clickedMarker <- data.of.click$clickedMarker[!data.of.click$clickedMarker
                                                                 %in% first_layer_ids]
    }
    data.of.click$selected <- NULL
    data.of.click$clickedMarker <- NULL
  })
  
  # Display summary table
  output$dt <- DT::renderDataTable({
    if (length(data.of.click$selected) == 0) return(NULL)
    data <- data.of.click$selected
    d <- data[, `:=`(du = residential_units.x, 
                     du_pcl_base = residential_units.y, 
                     non_res_sf = non_residential_sqft, 
                     non_res_pcl_base = nonres_building_sqft)
              ]
    if (input$checkbox_aggr) {
      d1<- d[, .(num_parcels = .N, 
            du = sum(du), 
            du_pcl_base = sum(du_pcl_base), 
            non_res_sf = sum(non_res_sf), 
            non_res_pcl_base = sum(non_res_pcl_base))]
    } else {
      d1 <- d[, .(parcel_id, du, du_pcl_base, non_res_sf, non_res_pcl_base)]
    }
    # if (data > 0) {
    #   data <- data.of.click$selected
    # browser()
    # }
    datatable(d1)
  })
  
#### End Adaptation from https://redoakstrategic.com/geoshaper/ --------------------- 

    
  # display initial map
  output$map <- renderLeaflet({
    leaflet.blank()
  })

  # select which run to use
  output$select_run <- renderUI({
    selectInput(inputId = "run",
                label = "Run",
                choices = allruns,
                width = "100%")
  })

  # read buildings indicator data 
  bldg.data <- reactive({
    dir <- file.path(input$run, "indicators")
    file <- list.files(dir, bld.filename, full.names = TRUE)
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
    if(input$mixuse.chb)
        subdata <- subdata[!is.na(N_res_con) & !is.na(N_nonres_con)]
    if (is.null(subdata)) return()
    if(is.null(subdata[["template_id"]])) subdata[,template_id:=NA]
    if(input$color %in% c("sizeres", "sizenonres")) {
      values <- log(subdata[[color.attributes[input$color]]]+1)
      palette.size <- colorQuantile("YlOrRd", range(values), n=9)
      palette.name <- "palette.size"
    } else {
      palette.name <- paste0("palette.", input$color)
      values <- subdata[[color.attributes[input$color]]]
    }
    subdata[, color := NA]
    if(nrow(subdata) > 0 && length(unique(values)) > 1) subdata[, color:= do.call(palette.name, list(values))]
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
                            "<br>Unit price: ", round(unit_price, 2),
                            "<br>Template ID: ", as.integer(template_id))
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
      selectInput(inputId = "run_mu",
                  label = "Run",
                  choices = allruns,
                  width = "100%")
  })
  
  # display initial map
  output$map_mixuse <- renderLeaflet({
      leaflet.blank()
  })
  
  # read buildings indicator data and aggregate to parcels
  pcl.data <- reactive({
      dir <- file.path(input$run_mu, "indicators")
      file <- list.files(dir, bld.filename, full.names = TRUE)
      if(length(file) == 0) return(data.table(a=integer()))
      df <- fread(file, sep="\t", header = TRUE)
      setkey(df, parcel_id)
      if(is.null(df$building_sqft)) df[, building_sqft:=NA]
      pclbld <- df[, .(new_res_units=sum(residential_units),
                       new_nonres_sqft=sum(non_residential_sqft),
                       new_price=median(unit_price),
                       new_building_sqft=sum(building_sqft)), 
                   by = parcel_id]
      pcl <- merge(parcels, pclbld)
      pcl <- pcl[!is.na(N_res_con) & !is.na(N_nonres_con)]
      pcl[is.na(new_res_units), new_res_units:=0]
      pcl[is.na(new_nonres_sqft), new_nonres_sqft:=0]
      pcl[, nonres_share:= new_nonres_sqft/new_building_sqft]
      pcl[, new_res_sqft := new_building_sqft-new_nonres_sqft]
      pcl[new_nonres_sqft > 0 | new_res_units > 0]
  })
  
  get.mu.indicator <- function(ind)
      switch(ind,
             share="nonres_share",
             dua="max_dua",
             far="max_far",
             ressqft="new_res_sqft",
             nonressqft="new_nonres_sqft",
             price="new_price"
      )

  mu.indicator <- reactive({
      get.mu.indicator(input$MUindicator)
  })
  
  # display markers
  observe({
      data <- pcl.data()
      if (is.null(data)) return()
      if(nrow(data) == 0) {
          clear.mixuse()
          return()
      }
      if(input$office) data <- data[allow_off == TRUE]
      if(input$mfr) data <- data[allow_mfr == TRUE]
      if(input$comm) data <- data[allow_com == TRUE]
      data[, color := NA]
      ind <- mu.indicator()
      d <- data[[ind]]
      if(ind %in% c("new_nonres_sqft", "new_res_sqft", "new_price"))
              d <- log(d+1)
      palette.share <- colorQuantile("Spectral", range(d), n=9, reverse=TRUE)
      data$color[] <- do.call("palette.share", list(d))
     
      marker.popup <- ~paste0("Parcel ID:  ", parcel_id, 
                              "<br>Non-res percent: ", as.integer(100*nonres_share),
                              "<br>DU:            ", as.integer(new_res_units),
                              "<br>Residen sqft:  ", as.integer(new_res_sqft),
                              "<br>Maximum DU/A:  ", round(max_dua,2),
                              "<br>Non-res sqft:  ", as.integer(new_nonres_sqft),
                              "<br>Maximum FAR:   ", round(max_far,2),
                              "<br>Unit price:    ", round(new_price, 2),
                              "<br>DU base:       ", as.integer(residential_units),
                              "<br>NonRes sf base:", as.integer(nonres_building_sqft)
                              )
      leaflet.results(leafletProxy("map_mixuse"), data, marker.popup, add=FALSE)
  })
  # Clear map
  clear.mixuse <- function()
      leafletProxy("map_mixuse") %>% clearMarkers()
  
  # Scatter plot
  output$MUplot <- renderPlot({
      data <- pcl.data()
      if (is.null(data)) return()
      if(nrow(data) == 0) return(plot(0,type='n',axes=FALSE,ann=FALSE))
      if(input$office) data <- data[allow_off == TRUE]
      if(input$mfr) data <- data[allow_mfr == TRUE]
      if(input$comm) data <- data[allow_com == TRUE]
      xind <- get.mu.indicator(input$MUindicatorX)
      yind <- get.mu.indicator(input$MUindicatorY)
      xlim <- ylim <- NULL
      if(xind == "nonres_share") xlim <- c(-0.1, 1.1)
      if(yind == "nonres_share") ylim <- c(-0.1, 1.1)
      plot(data[[xind]], data[[yind]], xlab=input$MUindicatorX, 
           ylab=input$MUindicatorY, xlim=xlim, ylim=ylim)
      if("nonres_share" %in% c(xind, yind)) {
          bpind <- c(xind, yind)[c(xind, yind) != "nonres_share"]
          if(length(bpind)>0) {
            boxplot(data[nonres_share == 0, bpind, with=FALSE], 
                  horizontal=yind == "nonres_share", boxwex = 0.25, add=TRUE, 
                  at=0, xaxt="n", yaxt="n", outline=FALSE)
            boxplot(data[nonres_share == 1, bpind, with=FALSE], 
                  horizontal=yind == "nonres_share", boxwex = 0.25, add=TRUE, 
                  at=1, xaxt="n", yaxt="n", outline=FALSE)
          }
      }
  })
}# end server function






