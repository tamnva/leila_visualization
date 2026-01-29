library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spsComps)


function(input, output, session) {

  # Stop the app when user close the browser
  session$onSessionEnded(function(){
    shiny::stopApp()
  })
  
  #----------------------------------------------------------------------------#
  #               Background + default maps/tables                             #
  #----------------------------------------------------------------------------#
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addScaleBar(position = "bottomright") %>%
      addRasterImage(huek, opacity = 0.7, group = "Hydrogeologie") %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       group = "CartoDBPositronNolabel") %>%
      addProviderTiles(providers$CartoDB.Positron,
                       group = "CartoDBPositron") %>%
      addProviderTiles(providers$OpenTopoMap,
                       group = "OpenTopoMap") %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "WorldImagery") %>%
      addCircleMarkers(data = stations,
                       radius = 3,
                       group = "Alle Einzugsgebiete",
                       fillColor = "#785EF0",
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       popup = ~ showPopup(gauge_id),
                       layerId = ~ gauge_id
      ) %>%
      addLayersControl(
        baseGroups = c("CartoDBPositron", "CartoDBPositronNolabel", 
                       "OpenStreetMap", "OpenTopoMap", "WorldImagery"),
        overlayGroups = c("Alle Einzugsgebiete",
                          "Hydrogeologie"),
        options = layersControlOptions(position = "bottomleft")
      )  %>%
      hideGroup("Hydrogeologie") %>%
      setView(lng = 9, lat = 50, zoom = 5)
  })
  
  
  output$catchment_attributes <- DT::renderDataTable({
    showDataFrame(attributes, session, NULL)
  })
  
  #----------------------------------------------------------------------------#
  #    Select catchment based on streamflow data availability (Data)           #
  #----------------------------------------------------------------------------#
  observeEvent(input$dataSubset, {

    # Read streamflow data from CAMELS-DE
    streamflow_statistic <<- getStreamflowStatistics(
      timeseries_camels_combine = timeseries_camels_combine_file,
      variable_name = c("discharge_spec_obs", "precipitation_mean"),
      start_date = input$selectPeriod[1],
      end_date = input$selectPeriod[2],
      max_missing = input$maxQmissing)
    
    hydrologische_indikatoren <<-  attributes %>% 
      filter(gauge_id %in% streamflow_statistic$gauge_id) %>%
      select(lat, long, gauge_id) %>% 
      left_join(streamflow_statistic, by = "gauge_id")
    
    # Display hydrological indicators
    output$hydrologische_indikatoren <- DT::renderDataTable({
      df <- hydrologische_indikatoren %>% 
        mutate_if(is.numeric, round, digits = 3) %>%
        mutate(Show = paste('<a class="go-map" href="" data-lat="', 
                            lat, '" data-long="', 
                            long, '" data-zip="', 
                            gauge_id, '"><i class="fa fa-crosshairs"></i></a>', 
                            sep="")) %>% 
        select(last_col(), everything()) %>%
        select(!c(lat, long))
      
      action <- DT::dataTableAjax(session, df, 
                                  outputId = "hydrologische_indikatoren")
      
      DT::datatable(df, options = list(ajax = list(url = action)), 
                    escape = FALSE)
    })
    
    # Display catchment attributes
    output$catchment_attributes <- DT::renderDataTable({
      showDataFrame(attributes, session, streamflow_statistic$gauge_id)
    })
    
    # Update map
    leafletProxy("map") %>%
      clearGroup("Alle Einzugsgebiete") %>%
      addCircleMarkers(data = stations %>% 
                         dplyr::filter(
                           gauge_id %in% streamflow_statistic$gauge_id),
                       radius = 3,
                       group = "Alle Einzugsgebiete",
                       fillColor = "#785EF0",
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       layerId = ~ gauge_id
      ) %>%
      clearControls()
    
  })
  
  

  #----------------------------------------------------------------------------#
  #                 Show catchment when click on table                         #
  #----------------------------------------------------------------------------#
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.01
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  #----------------------------------------------------------------------------#
  #                 Add catchment shape file when click on gauge               #
  #----------------------------------------------------------------------------#
  observeEvent(input$map_marker_click, {
    
    if (!is.null(input$map_marker_click$id)){
      leafletProxy("map") %>%
        clearGroup("Gewägktes Einzugsgebiet") %>%
        addPolygons(
          data = subset(catchments, gauge_id == input$map_marker_click$id),
          stroke = TRUE,
          weight = 2,
          popup = ~ showPopup(gauge_id),
          group = "Gewägktes Einzugsgebiet",
          layerId = ~ gauge_id)}
  })
  
  #----------------------------------------------------------------------------#
  #                           Boxplot for selected catchment                   #
  #----------------------------------------------------------------------------#

}
