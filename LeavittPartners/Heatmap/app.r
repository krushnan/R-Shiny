###############################################################################
# Heatmap App by Krushnan Mahalingam                                          #
# June 20, 2016                                                               #
#                                                                             #
#                                                                             #
###############################################################################

#
#V1  - Basic functionality. Slider input is dummy. Does nothing in this version.
#    - No shape file for county data. No option to group by county in this version.
#    - Selecting the variable does not work for all the options. Need to look into this.
#    - Major issue to be looked into - performance.
#V2  - County level has been included in this version.
#V3  - Slider is working in this version.
#    - Selecting all variables works in this version.

library(dplyr)
library(shiny)
library(readr)
library(leaflet)
library(rgdal)
library(data.table)

#setwd("C:/Dropbox (Leavitt Partners)/Data Plus Programming/Heat Map/Alpha/v1")
#setwd("F:/Dropbox (Leavitt Partners)/Data Plus Programming/Heat Map/Alpha/v1")
#setwd("F:\\Dropbox (Leavitt Partners)\\Data Plus Programming\\Hospital\\Hospital Subset 3")
setwd("E:/LeavittPartners/Data/Hospital Subset 3")

read_csv(file="data/Heat Map Data.csv") %>% 
  select(statename, countyname, populationestimate, medianage, pctpop65andover, 
         pctacolives, pctselfins50more_st,readmissionpenalty, percentemployeddocs, 
         percentprimarycaredocs, diabetes, chf, cancer, standrapercapcost, MApenetration, 
         totalproviders, physicians, pctphysiciansingroups, pctphysinlargegroups, pctpcpsingroups,
         privateenrollment, medicaidenrollment,medicareadvantageenrollment, medicarepdpenrollment,
         managedcarepenetration, pctselfins, econstrength2011, percentuninsured) -> heatmapData

setnames(heatmapData, old = c('statename', 'countyname', 'populationestimate', 'medianage', 'pctpop65andover', 
                    'pctacolives', 'pctselfins50more_st','readmissionpenalty', 'percentemployeddocs', 
                    'percentprimarycaredocs', 'diabetes', 'chf', 'cancer', 'standrapercapcost', 'MApenetration', 
                    'totalproviders', 'physicians', 'pctphysiciansingroups', 'pctphysinlargegroups', 'pctpcpsingroups',
                    'privateenrollment', 'medicaidenrollment','medicareadvantageenrollment', 'medicarepdpenrollment',
                    'managedcarepenetration', 'pctselfins', 'econstrength2011', 'percentuninsured'), 
         new = c('StateName','CountyName', 'PopulationEstimate','MedianAge', 'PercentPopulationAge65orHigher',
                 'EstimatedPercentOfPeopleInZipCodeThatArePartOfAnACO','PercentOfInsuredEmployeesWhoseEmployerIsSelfInsured',
                 'MedicareReadmissionPenalty', 'PercentOfPhysiciansThatAreEmployed', 'PercentOfPhysciainsThatAreInPrimaryCareSpecialties',
                 'ProportionOfPeopleThatHaveBeenDiagnosedWithDiabetes', 'ProportionOfPeopleThatHaveBeenDiagnosedWithCHF', 
                 'ProportionOfPeopleThatHaveBeenDiagnosedWithNonSkinCancer', 'StandardizedRiskAdjustedPerCapitaCosts', 
                 'MedicareAdvantagePenetration', 'TotalProviders', 'TotalPhysicians', 'ProportionOfPhysiciansInGroups','ProportionOfPhysiciansInLargeGroups',
                 'ProportionOfPrimaryCarePhysiciansInGroups', 'PrivateEnrollment', 'MedicaidEnrollment', 'MedicareAdvantageEnrollment', 'MedicarePDPEnrollment',
                 'PercentOfPrivatelyInsuredThatAreCoveredByHMO', 'PercentOfPrivatelyInsuredLivesThatAreSelfInsured', 
                 'EconomicStrength', 'PercentOfPeopleLessThan65WithoutInsurance'))

datasetLevel.group <- function(df, grp.var) {

    df1<- df %>% group_by_(grp.var) %>%
    summarise_each(funs(sum), PopulationEstimate, TotalProviders, TotalPhysicians, PrivateEnrollment, MedicaidEnrollment, 
                   MedicareAdvantageEnrollment, MedicarePDPEnrollment)
  
    df2 <- df %>% group_by_(grp.var) %>%
      summarise_each(funs(mean), MedianAge, PercentPopulationAge65orHigher,
                EstimatedPercentOfPeopleInZipCodeThatArePartOfAnACO, PercentOfInsuredEmployeesWhoseEmployerIsSelfInsured,
                MedicareReadmissionPenalty, PercentOfPhysiciansThatAreEmployed, PercentOfPhysciainsThatAreInPrimaryCareSpecialties,
                ProportionOfPeopleThatHaveBeenDiagnosedWithDiabetes, ProportionOfPeopleThatHaveBeenDiagnosedWithCHF,
                ProportionOfPeopleThatHaveBeenDiagnosedWithNonSkinCancer, StandardizedRiskAdjustedPerCapitaCosts,
                MedicareAdvantagePenetration, ProportionOfPhysiciansInGroups, ProportionOfPhysiciansInLargeGroups,
                ProportionOfPrimaryCarePhysiciansInGroups, PercentOfPrivatelyInsuredThatAreCoveredByHMO, 
                PercentOfPrivatelyInsuredLivesThatAreSelfInsured, EconomicStrength, PercentOfPeopleLessThan65WithoutInsurance)
      
    df <- inner_join(df1, df2, by = grp.var)    

  df
}

folder <- getwd()
usa.states <- readOGR(dsn = folder, 
                      layer = "States LP (PR)",
                      encoding = "latin1", #you may need to use a different encoding
                      verbose = FALSE)


usa.county <- readOGR(dsn = folder,
                      layer = "cb_2013_us_county_5m",
                      encoding = "latin1", #you may need to use a different encoding
                      verbose = FALSE)


hm_state <- select(heatmapData, -c(2))
hm_county <- select(heatmapData, -c(1))    

hm_state <- datasetLevel.group(hm_state, c("StateName"))
hm_county <- datasetLevel.group(hm_county, c("CountyName"))

hm_state[,-1] <-round(hm_state[,-1], 2)
hm_county[,-1] <-round(hm_county[,-1], 2)

usa.states@data <- left_join(usa.states@data, hm_state,by = c("STATE" = "StateName"))

proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

hm_county$CountyName = proper(hm_county$CountyName)
usa.county@data <- left_join(usa.county@data, hm_county, by= c("NAME" = "CountyName"))

heatmapdata_1 <- select(heatmapData, -c(1,2))

variable = c()
group = c()
readS = FALSE
readC = FALSE

ui <- fluidPage(
    titlePanel("Filters"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("level", "Select the Level", choices = c("State", "County"),selected = "State" ,inline = TRUE),
        selectInput("variable", "Variable Name", choices = unique(colnames(heatmapdata_1)), multiple = FALSE, selectize = TRUE),
        uiOutput("range"),
        actionButton("reset_button", "Reset view")
      ),
      mainPanel(
        leafletOutput("map"),
        dataTableOutput("heatmapdata")
      )
    ),
    # Add a little CSS to make the map background pure white
    tags$head(tags$style("
                         .leaflet-container { background-color: white !important; }
                         "))
    
  )


server <- function(input, output, session) {
  
  dataset.Filter <- function(dt, fil.var){
    if (fil.var == ""){
      dt
    } else {
      dt <- dt[ ((dt[[fil.var]] > input$slider[1]) & (dt[[fil.var]] < input$slider[2])) , ]
      dt  
    }
    
  }
  
  datasetLevel <- reactive({
    #inputvariable <- c("populationestimate")
    if (input$variable != "") {
      inputvariable<- c("PopulationEstimate")
    } else {
      inputvariable<-input$variable
    }
  
    if (input$level == "State") {
      group = c("StateName")
      l_hd <- list(hm_state, group, input$variable)
    }
      
    if (input$level == "County") {
      group = c("CountyName")
      l_hd <- list(hm_county, group, input$variable)
    }
    
    l_hd
  
  }) 
  
  output$range <- renderUI({
    sliderInput("slider", "Slider", min= round(min(datasetLevel()[[1]][[input$variable]], na.rm = TRUE), 2),
                                      max= round(max(datasetLevel()[[1]][[input$variable]], na.rm = TRUE), 2),
                                      value = c( round(min(datasetLevel()[[1]][[input$variable]], na.rm = TRUE), 2)
                                                ,round(max(datasetLevel()[[1]][[input$variable]], na.rm = TRUE), 2)))
  })
  
  
  
  lat <- 39.5
  lng <- -98.35
  zoom <- 4
  
  output$map <- renderLeaflet(
    {
      leaflet() %>%
        setView(lng = lng, lat = lat, zoom = zoom) %>% 
        setMaxBounds( -180, 5, -52,  73) 
    }
  )

  observeEvent( input$slider, ({

        variable <- datasetLevel()[[3]]
      
        pal_grid <- colorBin(palette = "YlGn", c(input$slider[1], input$slider[2]))
        
        if (input$level == "State") {
          
          #usa.states@data <- dataset.Filter(usa.states@data, datasetLevel()[[3]])
  
          names(usa.states@data)[names(usa.states@data)== variable] <- "variable"
          usa.states$variable <- round(usa.states$variable, 2)
          
          popup <- paste0("<strong>State: </strong>",
                          usa.states@data$STATE,
                          "<br><strong>Count: </strong>",
                          usa.states$variable)
          
          leafletProxy("map", data = usa.states@polygons) %>%
            clearShapes() %>% 
            clearControls() %>% 
            addPolygons(data = usa.states,  fillOpacity = 0.8, 
                        color = ~pal_grid(usa.states$variable),
                        weight = 2, popup = popup)
        }
        
        if (input$level == "County") {
          usa.county@data <- dataset.Filter(usa.county@data, datasetLevel()[[3]])
          names(usa.county@data)[names(usa.county@data)== variable] <- "variable"
          usa.county$variable <- round(usa.county$variable, 2)
          
          popup <- paste0("<strong>State: </strong>",
                          usa.county$NAME,
                          "<br><strong>Count: </strong>",
                          usa.county$variable)
          
          leafletProxy("map", data = usa.county@polygons) %>%
            clearShapes() %>% 
            clearControls() %>% 
            addPolygons(data = usa.county,  fillOpacity = 0.8, 
                        color = ~pal_grid(usa.county$variable),
                        weight = 2, popup = popup)
        }
    })
  )
  
  output$heatmapdata <- renderDataTable(
    select_(dataset.Filter(datasetLevel()[[1]], datasetLevel()[[3]]), 
            datasetLevel()[[2]], datasetLevel()[[3]]),
    options = list(pageLength=5,
                   scrollX=TRUE,
                   lengthMenu = c(5, 10, 25, 100),
                   searching=FALSE)
  )
  
  observe({
    input$reset_button
    if(input$level == "State"){
      leafletProxy("map", data = usa.states@polygons) %>% 
        setView(lat = lat, lng = lng, zoom = zoom)
    }
    if(input$level == "County"){
      leafletProxy("map", data = usa.county@polygons) %>% 
        setView(lat = lat, lng = lng, zoom = zoom)
    }
    
  }) 
  
 }
 
shinyApp(ui = ui, server = server) 

