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
  #                                Background map                              #
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
      addLayersControl(
        baseGroups = c("CartoDBPositron", "CartoDBPositronNolabel", 
                       "OpenStreetMap", "OpenTopoMap", "WorldImagery"),
        overlayGroups = c("Naturnahes Einzugsgebiet", 
                          "Unnatürliches Einzugsgebiet",
                          "Hydrogeologie"),
        options = layersControlOptions(position = "bottomleft")
      )  %>%
      setView(lng = 9, lat = 50, zoom = 5)
  })

  #----------------------------------------------------------------------------#
  #                                Add pop up                                  #
  #----------------------------------------------------------------------------#
  # When map is clicked, show a popup with catchment info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
  
  })
  

  #----------------------------------------------------------------------------#
  #                 Near-natural catchment selection                           #
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

  output$catchment_attributes <- DT::renderDataTable({
    df <- attributes %>%
      filter(
        dams_num <= input$nr_dam,
        artificial_surfaces_perc <= input$urban_land,
        agricultural_areas_perc <= input$agri_land
      ) %>%
      mutate(Show = paste('<a class="go-map" href="" data-lat="', 
                            Lat, '" data-long="', 
                            Long, '" data-zip="', 
                            gauge_id, '"><i class="fa fa-crosshairs"></i></a>', 
                            sep="")) %>% 
      select(last_col(), everything())
    
    action <- DT::dataTableAjax(session, df, outputId = "catchment_attributes")

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  output$hydrologische_indikatoren <- DT::renderDataTable({
    df <- hydrologische_indikatoren %>% 
      mutate_if(is.numeric, round, digits = 3) %>%
      mutate(Show = paste('<a class="go-map" href="" data-lat="', 
                          Lat, '" data-long="', 
                          Long, '" data-zip="', 
                          gauge_id, '"><i class="fa fa-crosshairs"></i></a>', 
                          sep="")) %>% 
      select(last_col(), everything()) %>%
      select(!c(Lat, Long))
    
    action <- DT::dataTableAjax(session, df, outputId = "hydrologische_indikatoren")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  #----------------------------------------------------------------------------#
  #                 Show selected gauge on map                                 #
  #----------------------------------------------------------------------------#
  observeEvent(input$map_marker_click, {
    
    if (!is.null(input$map_marker_click$id)){
      leafletProxy("map") %>%
        clearGroup("click_catchment") %>%
        addPolygons(
          data = subset(catchments, gauge_id == input$map_marker_click$id),
          stroke = TRUE,
          weight = 2,
          group = "click_catchment",
          layerId = ~ gauge_id)
    } 
    
    
  })
  
  #----------------------------------------------------------------------------#
  #                 Display selected  when click gauge                #
  #----------------------------------------------------------------------------#
  observeEvent(c(input$nr_dam, input$agri_land, input$urban_land), {
  
    selected_catchments <- attributes %>%
      dplyr::filter(dams_num <= input$nr_dam,
                    artificial_surfaces_perc <= input$urban_land,
                    agricultural_areas_perc <= input$agri_land)
    
    leafletProxy("map") %>%
      clearGroup("Naturnahes Einzugsgebiet") %>%
      addCircleMarkers(data = stations %>% 
                         dplyr::filter(
                           gauge_id %in% selected_catchments$gauge_id),
                       radius = 3,
                       group = "Naturnahes Einzugsgebiet",
                       fillColor = "#1A85FF",
                       fillOpacity = 1,
                       stroke = FALSE,
                       layerId = ~ gauge_id
      ) %>% 
      addCircleMarkers(data = stations %>% 
                         dplyr::filter(
                           !gauge_id %in% selected_catchments$gauge_id),
                       radius = 3,
                       group = "Unnatürliches Einzugsgebiet",
                       fillColor = "#FFC107",
                       fillOpacity = 1,
                       stroke = FALSE,
                       layerId = ~ gauge_id
      ) %>%
      addPolygons(
        data = catchments %>% 
          dplyr::filter(gauge_id %in% selected_catchments$gauge_id),
        stroke = TRUE,
        group = "Naturnahes Einzugsgebiet",
        fillColor = "#ffffff00",
        color = "#1A85FF",
        weight = 2,
        layerId = ~ gauge_id) %>% 
      clearControls()
  })
  
  #----------------------------------------------------------------------------#
  #                           Boxplot for selected catchment                   #
  #----------------------------------------------------------------------------#
  observeEvent(
    c(input$nr_dam, input$agri_land, input$urban_land, input$select_attr), {
      
      spsComps::shinyCatch(
        selected_catchments <- attributes %>%
          dplyr::mutate(catchment_group = case_when(
            dams_num <= input$nr_dam & 
              artificial_surfaces_perc <= input$urban_land &
              agricultural_areas_perc <= input$agri_land ~ "near-nat.",
            TRUE ~ "non-nat.")) %>%
          dplyr::select(c(catchment_group, input$select_attr)) %>%
          tidyr::pivot_longer(cols = -1, names_to = "att", values_to = "value"),
        
        blocking_level = "error"
      )
      
      
      spsComps::shinyCatch(
        output$boxplot_attr <- renderPlot(
          ggplot(selected_catchments, 
                 aes(x = catchment_group, y = value, fill = catchment_group)) +
            geom_boxplot() +
            facet_wrap(~att, ncol = 2, scales = "free") +
            labs(y ="", x = "") +
            scale_fill_manual(values=c("#1A85FF", "#FFC107")) +
            guides(fill = "none") +
            theme_bw() + 
            theme(text = element_text(size = 16),
                  axis.text.x = element_blank())),
        
        blocking_level = "error"
      )
  })
}
