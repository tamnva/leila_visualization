library(leaflet)

navbarPage(
  "", id="nav",
  
  tabPanel(
    "Home",
    
    conditionalPanel("false", icon("bullseye")),
    
    shinybusy::add_busy_spinner(spin = "radar", position = c("bottom-right"),
                                margins = c(100, 100)),
    
    div(class="outer",
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),
        
        leafletOutput("map", width="100%", height="100%"),
        
        # Panel to display plots.
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE, 
          draggable = TRUE, top = 60, left = "auto", right = 20, 
          bottom = "auto", width = 450, height = "auto",
          
          bslib::navset_card_underline(
            id = "navset",
            title = NULL,
            
            # First need to filter catchments with streamflow data
            bslib::nav_panel(
              title = "Data →", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              dateRangeInput("selectPeriod", "1. Select period for analysis",
                             start = as.Date("2001-01-01"),
                             end = as.Date("2020-12-31"),
                             min = as.Date("1980-01-01"),
                             max = as.Date("2020-12-31")),
              
              numericInput("maxQmissing", "2. Maximum allowable missing streamflow (%)",
                           min = 0, max = 99, value = 5),
              
              h5("3. Run subseting catchments"),
              actionButton("dataSubset", "Run and calculate streamflow statistics")
            ),
            
            # Now select "targeted catchment"
            bslib::nav_panel(
              title = "Targeted catchments", selected = TRUE,
              tags$hr(class = "custom-line"), h5(),
              
              h6("working")

            ),
            
          ), 
        ),
    )
  ),
  
  tabPanel(
    "Einzugsgebiet Charakteristik",
    
    # Elements for selecting near-natural catchments
    fluidRow(
      column(12,
             numericInput("nr_dam", "1. Maximum number of dams", 
                          min = 0, max = 999, value = 0),
             
             numericInput("agri_land", "2. Max fraction of agricultural land (%)", 
                          min = 0, max = 100, value = 20),
             
             numericInput("urban_land", "3. Max fraction of urban land (%)", 
                          min = 0, max = 100, value = 10),
      )
    ),
    
    hr(),
    DT::dataTableOutput("catchment_attributes")
  ),
  
  tabPanel(
    "Hydrologische Indikatoren",
    
    hr(),
    DT::dataTableOutput("hydrologische_indikatoren")
  )
)
