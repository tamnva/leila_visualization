library(leaflet)

navbarPage(
  "LEILA", id="nav",
  
  tabPanel(
    "Interaktive Karte",
    
    conditionalPanel("false", icon("bullseye")),
    
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
          bottom = "auto", width = 330, height = "auto",
          
          h4("Catchment explorer"),
          
          selectInput("select_attr", "1. Select catchment attributes", 
                      multiple = TRUE, choices = colnames(attributes[,-c(1)]), 
                      selected  = c("elev_mean","p_mean", 
                                    "q_mean", "runoff_ratio")),
                      
          plotOutput("boxplot_attr", height = 300)
          
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
