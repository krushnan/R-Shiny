# Shiny table to filter hospitals based on a variety of factors
# v1 - Map is reactive and 
# v2 - Map uses Leaflet Proxy 
# v3 - Includes option to only filter visible in data table; filterable by hospital type; adjust datatable output
  # to include horizontal scrolling and changed the default menu lengths


library(dplyr)
library(shiny)
library(readr)
library(leaflet)

# setwd("C:\\Dropbox (Leavitt Partners)\\Data Plus Programming\\Mapping\\Hospital Subset 3")

#bring in global data
# read_csv(file="Hospitals.csv") %>% 
#   filter(FIRM_TYPE=="Hospital") %>% 
#   select(HOSPITAL_NAME, HOSPITAL_ID, HQ_LATITUDE, HQ_LONGITUDE, WEBSITE, HQ_ADDRESS, HQ_CITY, HQ_STATE, HQ_ZIP_CODE, HQ_PHONE, NETWORK_NAME, NETWORK_PARENT_NAME) %>% 
#   filter(!is.na(HQ_LATITUDE)) -> hospitals

ui <- fluidPage(
  headerPanel("Filters"),
  sidebarPanel(
    selectizeInput("type", "Hospital Type", choices = NULL, multiple = TRUE),
    selectizeInput("state", "State", choices = NULL, multiple = TRUE, selected=NULL),
    selectizeInput("name", "Network Name", choices = NULL, multiple = TRUE, selected=NULL),
    selectizeInput("parentname", "Network Parent Name", choices = NULL, multiple = TRUE, selected=NULL),
    checkboxInput("cluster", "Cluster Hospitals", TRUE),
    checkboxInput("visible", "Only Show Visible Hospitals in Table", TRUE)
  ),
  mainPanel(
    leafletOutput("map"),
    dataTableOutput("hospitaldata")
    )
  
)

server <- function(input, output, session) {
  read_csv(file="data/Hospitals.csv") %>% 
    filter(FIRM_TYPE=="Hospital") %>% 
    select(HOSPITAL_NAME, HOSPITAL_ID, HQ_LATITUDE, HQ_LONGITUDE,  HOSPITAL_TYPE, WEBSITE,
           HQ_ADDRESS, HQ_CITY, HQ_STATE, HQ_ZIP_CODE, HQ_PHONE, NETWORK_NAME, NETWORK_PARENT_NAME, NUMBER_BEDS) %>% 
    filter(!is.na(HQ_LATITUDE)) -> hospitals -> h2
  
  #what goes in the popup
  hospitals$popup <- paste0("<b><a href=http://",hospitals$WEBSITE,">",hospitals$HOSPITAL_NAME,"</a></b><br/>",
                            hospitals$HQ_ADDRESS,"<br/>",
                            hospitals$HQ_CITY,", ",hospitals$HQ_STATE," ",hospitals$HQ_ZIP_CODE,"<br/>",
                            
                            "<a href='https://www.defhc.com/hospitals/",hospitals$HOSPITAL_ID,"'>","More Info</a>")
  
  #remove non-ASCII characters
  hospitals$popup2<-iconv(hospitals$popup, "latin1", "ASCII", sub="hospitals$HOSPITAL_NAME")
  
   updateSelectizeInput(session, "type", choices = sort(unique(h2$HOSPITAL_TYPE)), server = TRUE, selected="Short Term Acute Care Hospital")
   updateSelectizeInput(session, "state", choices = sort(unique(h2$HQ_STATE)), server = TRUE)
   updateSelectizeInput(session, "name", choices = sort(unique(h2$NETWORK_NAME)), server = TRUE)
   updateSelectizeInput(session, "parentname", choices = sort(unique(h2$NETWORK_PARENT_NAME)), server = TRUE)

  h2<-hospitals
  
  

  
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    # h2<-hospitals %>% filter(state %in% input$state) 
    
    h2<-hospitals
    
    # filter based on map bounds
      if (input$visible) {
        input$map_bounds -> bounds
        if (is.null(bounds)) {
          bounds$north <- 86
          bounds$south <- -86
          bounds$east <- 181
          bounds$west <- -181
        }
        filter(h2,HQ_LATITUDE<=bounds$north, HQ_LATITUDE>=bounds$south, 
               HQ_LONGITUDE>=bounds$west, HQ_LONGITUDE<=bounds$east) -> h2
      }
    
    # filter based on the filter options
    if (is.null(input$type)) {
      inputtype<-unique(h2$HOSPITAL_TYPE)
    } else {
      inputtype<-input$type 
    }
    if (is.null(input$state)) {
        inputstate<-unique(h2$HQ_STATE)
    } else {
     inputstate<-input$state 
      }
    if (is.null(input$name)) { 
        inputname<-unique(h2$NETWORK_NAME)
    } else {
      inputname<-input$name
    }
    if (is.null(input$parentname)) { 
        inputparentname<-unique(h2$NETWORK_PARENT_NAME)
    } else {
      inputparentname<-input$parentname
    }
 
    filter(h2, HOSPITAL_TYPE %in% inputtype) %>% 
      filter(HQ_STATE %in% inputstate) %>% 
      filter(NETWORK_NAME %in% inputname) %>% 
      filter(NETWORK_PARENT_NAME %in% inputparentname) %>% 
      arrange(HOSPITAL_NAME)
    
    
    
    
    # # inputstate<-input$state
    #   h2<-filter(h2,HQ_STATE==input$state)
    #   s<<-inputstate
    # 
    # # inputname<-input$name
    # if (!is.null(input$name)) { 
    #   h2<-filter(h2,NETWORK_NAME==input$name)
    # }
    # # inputparentname<-input$parentname
    # if (!is.null(input$parentname)) { 
    #   h2<-filter(h2,NETWORK_PARENT_NAME==input$parentname)


      
    # }
    # istate<<-input$state
    # iname<<-input$name
    # iparent<<-input$parentname
    # h<<-hospitals
    # h2<-filter(hospitals,HQ_STATE == inputstate, NETWORK_NAME == inputname, NETWORK_PARENT_NAME == inputparentname)  #filtered hospitals

  })
  

  # Output the data table but only use select variables
  
  output$hospitaldata <- renderDataTable(
    select(filteredData(), HOSPITAL_NAME, HOSPITAL_TYPE,  WEBSITE, HQ_ADDRESS, HQ_CITY, HQ_STATE, 
        HQ_ZIP_CODE, HQ_PHONE, NETWORK_NAME, NETWORK_PARENT_NAME, NUMBER_BEDS) ,
      options = list(pageLength=5,
                     scrollX=TRUE,
                     lengthMenu = c(5, 10, 25, 100),
                     searching=FALSE)
  )
  
  
  # Clusting options - whether or not to cluster
  cluster <- reactive({
    if (input$cluster) {
      markerClusterOptions(disableClusteringAtZoom=12,maxClusterRadius=120)
    } else {
      NULL
    }
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addTiles(options=tileOptions(minZoom = 0, maxZoom = 10)) %>%
      setView(lng = -98.35, lat = 39.5, zoom = 4) %>% 
      setMaxBounds( -180, 5, -52,  73) 
  })
  
  
  
  # Incremental changes to the map (in this case, changing which hospitals 
  # to display) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.

  
  observe({
  
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>% 
      addMarkers(data=filteredData(), ~HQ_LONGITUDE, ~HQ_LATITUDE, popup = ~popup2,
                 clusterOptions = cluster())
  })

  
  
}


shinyApp(ui = ui, server = server) 