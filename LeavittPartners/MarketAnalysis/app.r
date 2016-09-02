###############################################################################
# Market Analysis App by Krushnan Mahalingam                                  #
# July 8, 2016                                                                #
#                                                                             #
#                                                                             #
###############################################################################

#
#V1 - Basic functionality.
#V2 - Region (State) selection and markers for ACO, Hospitals, Physicians and Insurer.

library(dplyr)
library(shiny)
library(readr)
library(leaflet)
library(reshape2)
library(readstata13)
library(data.table)
library(ggplot2)
library(rgdal)

#setwd("C:/Dropbox (Leavitt Partners)/Data Plus Programming/Market Analysis/Alpha/v1")
#setwd(F:/Dropbox (Leavitt Partners)/Data Plus Programming/Market Analysis/Alpha/v1)
#setwd("C:\Users\Ben Gong\Dropbox (Leavitt Partners)\Data Plus Programming\Market Analysis\Alpha\v1")
setwd("C:\\Users\\Krushnan\\Dropbox (Leavitt Partners)\\Data Plus Programming\\Market Analysis\\Alpha\\v1")
#setwd("E:/Dropbox (Leavitt Partners)/Data Plus Programming/Market Analysis/Alpha/v1")
#setwd("E:/LeavittPartners/Market Analysis/Data")

lookup <- structure(c("AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "GU", "HI", "ID", "IL",
                      "IN", "IA", "KS", "KY", "LA", "ME", "MH", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                      "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PW", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT",
                      "VI", "VA", "WA", "WV", "WI", "WY"),
                    .Names=c("Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas",
                             "California", "Colorado","Connecticut", "Delaware", "District of Columbia",
                             "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana",
                             "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Marshall Islands",
                             "Maryland", "Massachusetts", "Michigan", "Minnesota",
                             "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                             "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                             "Ohio", "Oklahoma", "Oregon", "Palau", "Pennsylvania",
                             "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                             "Texas", "Utah", "Vermont", "Virgin Islands", "Virginia", "Washington",
                             "West Virginia", "Wisconsin", "Wyoming"))

folder <- getwd()
full_tracts <- readOGR(dsn = folder, 
                      layer = "States LP (PR)",
                      encoding = "latin1", #you may need to use a different encoding
                      verbose = FALSE)

read_csv(file="Heat Map Data.csv") %>% 
  select(zipcode, fips, latitude, longitude, statename, countyname, populationestimate, medianage, pctpop65andover, 
         pctacolives, pctselfins50more_st,readmissionpenalty, percentemployeddocs, 
         percentprimarycaredocs, diabetes, chf, cancer, standrapercapcost, MApenetration, 
         totalproviders, physicians, pctphysiciansingroups, pctphysinlargegroups, pctpcpsingroups,
         privateenrollment, medicaidenrollment,medicareadvantageenrollment, medicarepdpenrollment,
         managedcarepenetration, pctselfins, econstrength2011, percentuninsured) -> heatmapData

setnames(heatmapData, old = c('zipcode', 'fips', 'latitude', 'longitude', 'statename', 'countyname', 'populationestimate', 'medianage', 'pctpop65andover', 
                              'pctacolives', 'pctselfins50more_st','readmissionpenalty', 'percentemployeddocs', 
                              'percentprimarycaredocs', 'diabetes', 'chf', 'cancer', 'standrapercapcost', 'MApenetration', 
                              'totalproviders', 'physicians', 'pctphysiciansingroups', 'pctphysinlargegroups', 'pctpcpsingroups',
                              'privateenrollment', 'medicaidenrollment','medicareadvantageenrollment', 'medicarepdpenrollment',
                              'managedcarepenetration', 'pctselfins', 'econstrength2011', 'percentuninsured'), 
         new = c('zipcode', 'fips', 'latitude', 'longitude','StateName','CountyName', 'PopulationEstimate','MedianAge', 'PercentPopulationAge65orHigher',
                 'EstimatedPercentOfPeopleInZipCodeThatArePartOfAnACO','PercentOfInsuredEmployeesWhoseEmployerIsSelfInsured',
                 'MedicareReadmissionPenalty', 'PercentOfPhysiciansThatAreEmployed', 'PercentOfPhysciainsThatAreInPrimaryCareSpecialties',
                 'ProportionOfPeopleThatHaveBeenDiagnosedWithDiabetes', 'ProportionOfPeopleThatHaveBeenDiagnosedWithCHF', 
                 'ProportionOfPeopleThatHaveBeenDiagnosedWithNonSkinCancer', 'StandardizedRiskAdjustedPerCapitaCosts', 
                 'MedicareAdvantagePenetration', 'TotalProviders', 'TotalPhysicians', 'ProportionOfPhysiciansInGroups','ProportionOfPhysiciansInLargeGroups',
                 'ProportionOfPrimaryCarePhysiciansInGroups', 'PrivateEnrollment', 'MedicaidEnrollment', 'MedicareAdvantageEnrollment', 'MedicarePDPEnrollment',
                 'PercentOfPrivatelyInsuredThatAreCoveredByHMO', 'PercentOfPrivatelyInsuredLivesThatAreSelfInsured', 
                 'EconomicStrength', 'PercentOfPeopleLessThan65WithoutInsurance'))

heatMapSubset <- heatmapData %>% select(zipcode, longitude, latitude, fips, StateName)


# ACO data
aco <- readstata13::read.dta13("ACO.dta")
aco %>% select(aco_id, state, statenamehq, zipcode, aco_name, program1, provider_name1, provider_name2, 
               contract_type_summary, mssp, provider_type, payer_name1, payer_name2,
               estacolives) -> aco

aco$provider_name <- paste(aco$provider_name1, aco$provider_name2, sep = "\n") 
aco$payer_name <- paste(aco$payer_name1, aco$payer_name2, sep = "\n") 

ok <- complete.cases(aco$zipcode)
aco <- aco[ok,]

heat_aco <- inner_join(heatMapSubset, aco,by = c("zipcode" = "zipcode"))
heat_aco %>% filter(!is.na(latitude)) -> heatmapACO

# Hospital Data
read_csv(file="Hospitals.csv") %>% 
  filter(FIRM_TYPE=="Hospital") %>% 
  select(HOSPITAL_NAME, HOSPITAL_ID, HQ_LATITUDE, HQ_LONGITUDE, HOSPITAL_TYPE, WEBSITE,
         HQ_ADDRESS, HQ_CITY, HQ_STATE, HQ_ZIP_CODE, HQ_PHONE, NETWORK_NAME, NETWORK_PARENT_NAME, NUMBER_BEDS) -> hospitals

hospitals$HQ_ZIP_CODE <- as.integer(hospitals$HQ_ZIP_CODE)

heat_hospital <- inner_join(heatMapSubset, hospitals, by = c("zipcode" = "HQ_ZIP_CODE"))
heat_hospital %>% filter(!is.na(HQ_LATITUDE)) -> heatmapHospital


# Physicians data
physician <- readstata13::read.dta13("Physician Groups.dta")
physician %>% select(physiciangroupid, zipcode, state, statename, physiciangroupname, numberofphysicians, acoaffiliations, specialties) %>% 
  filter(numberofphysicians >= 20)-> physician
ok <- complete.cases(physician$zipcode)
physician <- physician[ok,]

heat_physician <- inner_join(heatMapSubset, physician,by = c("zipcode" = "zipcode"))
heat_physician ->heatmapPhysician

# Insurer data
insurer <- readstata13::read.dta13("Insurers.dta")
insurer %>% select(zipcode, company, revenue, state, statename) -> insurer
insuranceLives <- readstata13::read.dta13("Insurance Lives by Plan.dta")
insuranceLives %>% select(zipcode, privateenrollment, medicaidenrollment, medicareadvantageenrollment) -> insuranceLives
insurance <- inner_join(insurer, insuranceLives, by = c("zipcode" = "zipcode"))
ok <- complete.cases(insurance$zipcode)
insurance <- insurance[ok,]

heat_insurance <- inner_join(heatMapSubset, insurance,by = c("zipcode" = "zipcode"))
heat_insurance -> heatmapInsurance

ACO <- data.frame()
HOSP <- data.frame()
PHYS <- data.frame()
INSR <- data.frame()

ui <- navbarPage(title = "Market Analysis",
                 tabPanel("Map",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput('state_name', label = 'Select a state', choices = lookup, selectize = TRUE, multiple = FALSE, selected = "Utah"),
                       selectInput("city_name", label = 'Select a city', choices = NULL, multiple = FALSE, selectize = TRUE),
                       checkboxInput("cluster", "Cluster", TRUE),
                       width = 2
                       #checkboxInput("visible", "Only Show Visible Map Data in Table", TRUE),
                       #actionButton("reset_button", "Reset view")
                     ),
                     mainPanel(
                       radioButtons("options", "Variables:", choices = c("ACOs", "Hospitals", "Physicians", "Insurers"), inline = T, selected = c("ACOs")),
                       # tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                       leafletOutput("map", height = 500),
                       width = 10
                     )
                    )
                 ),
                tabPanel("Overview", dataTableOutput("table")),
                tabPanel("ACOs", dataTableOutput("acotable")),
                tabPanel("Hospital", dataTableOutput("hosptable")),
                tabPanel("Physician Groups", dataTableOutput("phystable")),
                tabPanel("Insurers", plotOutput("plot7" , width = "50%"), plotOutput("plot8", width = "50%")),
    # Add a little CSS to make the map background pure white
    tags$head(tags$style("
                         .leaflet-container { background-color: white !important; }
                         "))
  )

server <- function(input, output, session) {
  
  selectState <- reactive({
#     if (input$visible) {
#       input$map_bounds -> bounds
#       if (is.null(bounds)) {
#         bounds$north <- 86
#         bounds$south <- -86
#         bounds$east <- 181
#         bounds$west <- -181
#       }
#       filter(heatmapACO, latitude<=bounds$north, latitude>=bounds$south, 
#              longitude>=bounds$west, longitude<=bounds$east) -> ACO
#       
#       filter(heatmapHospital, latitude<=bounds$north, latitude>=bounds$south, 
#              longitude>=bounds$west, longitude<=bounds$east) -> HOSP
#       
#       filter(heatmapPhysician, latitude<=bounds$north, latitude>=bounds$south, 
#              longitude>=bounds$west, longitude<=bounds$east) -> PHYS
#       
#       filter(heatmapInsurance, latitude<=bounds$north, latitude>=bounds$south, 
#              longitude>=bounds$west, longitude<=bounds$east) -> INSR
#    }
  
    lookup_sub <- lookup[lookup %in% input$state_name]
    
    m <- full_tracts[full_tracts$STATE == names(lookup_sub), ]
    
    return(m)

  })
  
  lat <- 39.5
  lng <- -98.35
  zoom <- 4
  
  
  # Draw the map without selected tracts
  output$map <- renderLeaflet({
    
    map <- leaflet(selectState()) %>%
      addProviderTiles('CartoDB.Positron') %>% 
      addPolygons()
    
    map
    
  })
  
  observe({
   
    #what goes in the popup
    popup2 <- paste0("<strong>Name: </strong>",
                     heatmapACO$aco_name,
                     "<br><strong>Program: </strong>",
                     heatmapACO$program1,
                     "<br><strong>Contract Type: </strong>",
                     heatmapACO$contract_type_summary,
                     "<br><strong>Provider Type: </strong>",
                     heatmapACO$provider_type)
    
    #what goes in the popup
    popup3 <- paste0("<strong>Name: </strong>",
                     heatmapHospital$HOSPITAL_NAME,
                     "<br><strong>Hospital Type: </strong>",
                     heatmapHospital$HOSPITAL_TYPE,
                     "<br><strong>Network Name: </strong>",
                     heatmapHospital$NETWORK_NAME)
    
    #what goes in the popup
    popup4 <- paste0("<strong>Physician Group Name: </strong>",
                     heatmapPhysician$physiciangroupname,
                     "<br><strong>Number of Physicians: </strong>",
                     heatmapPhysician$numberofphysicians,
                     "<br><strong>Specialties: </strong>",
                     heatmapPhysician$specialties)
    
    #what goes in the popup
    popup5 <- paste0("<strong>Name: </strong>",
                     heatmapInsurance$company,
                     "<br><strong>Private Enrollment: </strong>",
                     heatmapInsurance$privateenrollment,
                     "<br><strong>Medicaid Enrollment: </strong>",
                     heatmapInsurance$medicaidenrollment,
                     "<br><strong>Medicare Advantage Enrollment: </strong>",
                     heatmapInsurance$medicareadvantageenrollment)
    
    leafletProxy("map") %>% 
      clearControls() %>%
      clearMarkers() %>% 
      clearMarkerClusters() %>% 
      clearShapes() %>%
      addPolygons(data = selectState()) %>% 
      addMarkers(data = heatmapACO, ~longitude, ~latitude,
                 clusterOptions = cluster(), popup = popup2, group = "ACO" ) %>% 
      addMarkers(data = heatmapHospital, ~longitude, ~latitude,
                 clusterOptions = cluster(), popup = popup3, group = "Hospital" ) %>% 
      addMarkers(data = heatmapPhysician, ~longitude, ~latitude,
                 clusterOptions = cluster(), popup = popup4, group = "Physician" ) %>% 
      addMarkers(data = heatmapInsurance, ~longitude, ~latitude,
                 clusterOptions = cluster(), popup = popup5, group = "Insurer" )
    
  })
  

  observeEvent(c(input$options, input$state_name, input$cluster), {
    proxy <- leafletProxy("map")
    if (input$options == "ACOs") proxy %>% showGroup('ACO')
    else proxy %>% hideGroup('ACO')
    if (input$options == "Hospitals") proxy %>% showGroup('Hospital')
    else proxy %>% hideGroup('Hospital')
    if (input$options == "Physicians") proxy %>% showGroup('Physician')
    else proxy %>% hideGroup('Physician')
    if (input$options == "Insurers") proxy %>% showGroup('Insurer')
    else proxy %>% hideGroup('Insurer')
  }
    
  )
  
#   output$table <- renderDataTable(
#     select(aco[aco$state == input$state_name, ], aco_name, program1, provider_name, contract_type_summary, mssp, 
#            provider_type, payer_name, estacolives),
#     options = list(pageLength=5,
#                    scrollX=TRUE,
#                    lengthMenu = c(5, 10, 25, 100),
#                    searching=FALSE)
#   )
  
  output$acotable <- renderDataTable(
    select(heatmapACO[heatmapACO$state == input$state_name, ], aco_name, program1, provider_name, contract_type_summary, mssp, 
           provider_type, payer_name, estacolives),
    options = list(pageLength=5,
                   scrollX=TRUE,
                   lengthMenu = c(5, 10, 25, 100),
                   searching=FALSE)
  )
  
  output$hosptable <- renderDataTable(
    select(heatmapHospital[heatmapHospital$HQ_STATE == input$state_name, ], HOSPITAL_NAME, HOSPITAL_TYPE,  WEBSITE, HQ_ADDRESS, HQ_CITY, HQ_STATE, 
           zipcode, HQ_PHONE, NETWORK_NAME, NETWORK_PARENT_NAME, NUMBER_BEDS) ,
    options = list(pageLength=5,
                   scrollX=TRUE,
                   lengthMenu = c(5, 10, 25, 100),
                   searching=FALSE)
  )
  
  output$phystable <- renderDataTable(
    select(heatmapPhysician[heatmapPhysician$state == input$state_name, ], physiciangroupid, physiciangroupname, numberofphysicians, acoaffiliations, specialties) ,
    options = list(pageLength=5,
                   scrollX=TRUE,
                   lengthMenu = c(5, 10, 25, 100),
                   searching=FALSE)
  )
  
  output$plot7 <- renderPlot({
    m <- select(heatmapInsurance[heatmapInsurance$state == input$state_name, ], everything()) %>%  group_by(company) %>% summarise(sumPrivate = sum(privateenrollment))
    m1 <- m[order(m$sumPrivate, decreasing = TRUE), ]
    m1 <- m1[0:5,]
    ggplot(m1, aes(x=factor(company, levels = names(table(company))), y=sumPrivate)) + geom_bar(stat="identity", fill="blue") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(x="Company", y="Total Private Enrollments") +
      geom_text(stat='identity',aes(label=round(sumPrivate,0)),vjust=-0.1)
    
  })
  
  output$plot8 <- renderPlot({
#     m <- select(heatmapInsurance[heatmapInsurance$state == input$state_name, ], one_of(c("company","medicaidenrollment","medicareadvantageenrollment"))) %>% 
#       group_by(company) %>% summarise(medicaid = sum(medicaidenrollment), medicaidadv = sum(medicareadvantageenrollment))
    m <- select(heatmapInsurance[heatmapInsurance$state == input$state_name, ], one_of(c("company","medicaidenrollment","medicareadvantageenrollment")))
    m1 <- melt(m, id.var="company")

    ggplot(m1, aes(x = company, y = value, fill = variable)) + 
      geom_bar(stat = "identity", position=position_dodge()) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
      labs(x="Company", y="Total Enrollments") 
    
#     m1 <- m[order(m$sumPrivate, decreasing = TRUE), ]
#     m1 <- m1[0:5,]
#      ggplot(m1, aes(x=factor(company, levels = names(table(company))), y=value, fill = variable)) + geom_bar(stat="identity") +
#        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(x="Company", y="Total Private Enrollments") #+
       # geom_text(stat='identity', position = "stack")
  })
    
    
    
  # Clusting options - whether or not to cluster
  cluster <- reactive({
    if (input$cluster) {
      markerClusterOptions(disableClusteringAtZoom=10,maxClusterRadius=120)
    } else {
      NULL
    }
  })
  
#   observe({
#     input$reset_button
#       leafletProxy("map", data = selectState()) %>% 
#         setView(lat = lat, lng = lng, zoom = zoom)
#     
#   }) 
  
 }
 
shinyApp(ui = ui, server = server) 

