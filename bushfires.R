library(shiny)
library(shinydashboard)
library(rgdal)
library(raster)
library(leaflet)
library(shinycssloaders)
library(rvest)
library(DT)
library(dplyr)
library(sf)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras)
library(jsonlite)
library(ggplot2)
library(scales)
library(plotly)
library(tidyRSS)
library(lubridate)

#######################
# LOAD FILES  # 
# Replace /Users/abhijeet/RStudio with your own directory
#######################

fireHistoryFile <- paste("/Users/abhijeet/RStudio/BushfireDashboard/Bushfire_Dataset/FireHistory/FireH.tif")
cfaBoundariesFile <- paste("/Users/abhijeet/RStudio/BushfireDashboard/Bushfire_Dataset/CFA_Districts/ll_gda2020/esrishape/whole_of_dataset/victoria/VMADMIN")
vicPointsOfInterestFile <- paste("/Users/abhijeet/RStudio/BushfireDashboard/Bushfire_Dataset/Fire_Stations/ll_gda2020/esrishape/whole_of_dataset/victoria/VMFEAT/FOI_POINT.shp")
saferPlacesFile <- paste("/Users/abhijeet/RStudio/BushfireDashboard/Bushfire_Dataset/NSP-BPLR.csv")
ausFireSeries <- paste("/Users/abhijeet/RStudio/BushfireDashboard/Bushfire_Dataset/fires_time_series.json")
firesByStateData <- paste("/Users/abhijeet/RStudio/BushfireDashboard/Bushfire_Dataset/fires_bystate.json")
firesHistoricalData <- paste("/Users/abhijeet/RStudio/BushfireDashboard/Bushfire_Dataset/fires_historical.json")
ausTempRainfallData <- paste("/Users/abhijeet/RStudio/BushfireDashboard/Bushfire_Dataset/aus_temp_rainfall.json")

#######################
# SERVER DATA PARSING #
#######################

##########################################################################################33
## Bushfire History Map

# Raster for Fire History
fireHistoryRaster <- raster(fireHistoryFile)
# Min & Max Values in Raster
rasterMinMax <- 1:8
# Custom Pallete for Raster
pallete <- colorNumeric("OrRd", rasterMinMax, reverse = TRUE, na.color = "transparent")
# Aggregrating raster for faster display
fireHistoryRaster10 <- aggregate(fireHistoryRaster, 5)

# Removing heavy unused variable from memory
rm(fireHistoryRaster)

##########################################################################################
## Bushfire History Data Wiki

# Scraping Data from Wiki Website
bushfireWiki <- read_html("https://en.wikipedia.org/wiki/List_of_major_bushfires_in_Australia")
# Accessing Tables in the webpage
bushfireWikiTables <- bushfireWiki %>% html_table(fill = TRUE)
# Getting the required table
bushfireTable <- bushfireWikiTables[[2]]
# Removing Aggregrated row
bushfireTable <- bushfireTable[-1,]
# Changing the column names for better display
colnames(bushfireTable) <- c("Date", "Name/Description", "State/Territories", "Area Burned (Ha)", "Area Burned (Acres)", "Fatalities", "Properties Damaged (Homes)", "Properties Damaged (Buildings)", "Properties Damaged (Other)", "Notes")
# Removing unused rows
bushfireTable$Notes <- NULL
# Filtering Table to fetch only Victoria Data
bushfireTable <- bushfireTable %>% 
  dplyr::filter(grepl('Victoria|Nationwide', bushfireTable$`State/Territories`))

# Removing unused variables from memory
rm(bushfireWiki, bushfireWikiTables)

##########################################################################################
## CFA & MFB Boundaries and Stations

# Loading CFA district boundaries SHAPEFILE
cfaB <- readOGR(dsn = cfaBoundariesFile, "CFA_DISTRICT")

# Loading Victoria Points of Interest - MULTIPOINT file
vcShapes <- sf::st_read(vicPointsOfInterestFile, quiet = TRUE)
# Converting the MULTIPOINT to POINT for leaflet
vcPoints <- st_cast(vcShapes, "POINT") 
# Filtering only fire stations from the point file(vicPointsOfInterestFile)
fireS <- vcPoints %>% 
  dplyr::filter(vcPoints$FTYPE == 'emergency facility' & vcPoints$FEATSUBTYP == 'fire station' & vcPoints$STATE == 'VIC')

##########################################################################################
## Hospitals in Victoria

# Filtering only hospitals from the point file(vicPointsOfInterestFile)
hospitals <- vcPoints %>% 
  dplyr::filter(vcPoints$STATE == 'VIC' & vcPoints$FTYPE == 'hospital')
# Creating Popup for leaflet map
hospitals$popup <- paste0('<b>', hospitals$NAME, '</b> <br>', hospitals$FEATSUBTYP)

# Removing unused variables from memory
rm(vcShapes, vcPoints)

##########################################################################################
## Neighbourhood Safe Places

# Reading the neighbourhood safer places CSV file
nsp <- read.csv(saferPlacesFile, stringsAsFactors = FALSE)
# Creating popups for the points
nsp$popup <- paste0('<b>', nsp$Name, '</b> <br>',
                    'Address: ', nsp$Address, '<br>',
                    case_when(nsp$Sub_Location == '' ~ '',
                              TRUE ~ paste0('Sub-Location: ', nsp$Sub_Location, '<br>')),
                    'Township: ', nsp$Township, '<br>',
                    'Municipality: ', nsp$Municipality, '<br>',
                    'Google Address: <u> <i>', nsp$Address.found, '</i> </u>')

##########################################################################################
## Australia Bushfire Statistics

# Fetching 2019-2020 bushfire statistics from Open API
ausTimeSeriesData <- fromJSON(ausFireSeries)$result
# Converting x columnn as date for plotting time series chart
ausTimeSeriesData$x <- as.Date(ausTimeSeriesData$x)
# Changing column names for better readability
colnames(ausTimeSeriesData) <- c("id", "date", "number_of_fires", "avg_frp", "avg_brightness")

##########################################################################################
## Devastation By State

# Getting bushfire statistics for devastation by state from Open API and removing total column
firesByState <- fromJSON(firesByStateData)$result %>% 
  subset(state != 'Total')

##########################################################################################
## Major Bushfires in Australia Stats

# Fetching major bushfires in Australia data from Open API
ausHistory <- fromJSON(firesHistoricalData)$result
# Removing the id field
ausHistory$id <- NULL
# Changing column names for better readability and filtering
colnames(ausHistory) <- c("date", "name", "state", "area_burned_ha", "area_burned_acres", "fatalities", "homeslost", "year")

##########################################################################################
## RSS Feeds for Fire Broadcast from CFA

# Loading RSS feeds as data frame using tidyRSS package
rssFeed <- tidyfeed("https://www.cfa.vic.gov.au/cfa/rssfeed/tfbfdrforecast_rss.xml", parse_dates = TRUE, clean_tags = FALSE)

# Loading 3 days broadcast in variables
rssFeedData1 <- paste0(rssFeed[1, "item_description"])
rssFeedData2 <- paste0(rssFeed[2, "item_description"])
rssFeedData3 <- paste0(rssFeed[3, "item_description"])

# Removing unsused variable from memory
rm(rssFeed)

##########################################################################################
## Live Incidents from FRV

# Fetching live incidents data in victoria from Open API
incidents <- fromJSON("https://data.emergency.vic.gov.au/Show?pageId=getIncidentJSON")$results
# Creating meaningful popups 
incidents$popup <- paste0('<b>', incidents$incidentLocation, '</b><br>',
                          'Updated At: ', incidents$lastUpdatedDtStr, '<br>',
                          'Category: ', incidents$category1, '-', incidents$category2, '<br>',
                          'Type: ', incidents$incidentType, '<br>',
                          'Status: ', incidents$territory, '-', incidents$incidentStatus, '<br>',
                          'Size: ', incidents$incidentSize, '<br><br>',
                          case_when(incidents$name == '' ~ '',
                                    TRUE ~ paste0('Location: ', incidents$name, '-', incidents$incidentLocation, '<br>')),
                          case_when(incidents$municipality == '' ~ '', 
                                    TRUE ~ paste0('Municipality: ', incidents$municipality)), '<br>',
                          'Coordinates: <u><i>', incidents$latitude, ',', incidents$longitude, '</i></u>')

##########################################################################################
## Temperature and Rainfall for Australia

# Fetching rainfall and temperature data from open API
tempRainData <- fromJSON(ausTempRainfallData)$result
# Converting year to date
tempRainData$year <- ymd(tempRainData$year, truncated = 2L)

##########################################################################################

#######################
# HELPER FUNCTIONS #
#######################

# Function to format big numbers
format_bignum = function(n){
  case_when(
    n >= 1e12 ~ paste(round(n/1e12), 'Tn'),
    n >= 1e9 ~ paste(round(n/1e9), 'B'),
    n >= 1e6 ~ paste(round(n/1e6), 'M'),
    n >= 1e3 ~ paste(round(n/1e3), 'K'),
    TRUE ~ as.character(n))
}

##########################################################################################

##################
# USER INTERFACE #
##################

##########################################################################################

# Creating sidebar navigation elements
sidebar <- dashboardSidebar(
  # Setting width of the sidebar
  width = 300,
  # Adding Menu Items 
  sidebarMenu(
    # Assigning id to tabs
    id = "tabs",
    # Victoria Bushfire History Tab
    menuItem("Victoria Bushfire History", tabName = "history", icon = icon("history", verify_fa = FALSE)),
    # Australia Bushfire Statistics Tab with 3 subtabs and 2 conditional panels
    menuItem("Australia Bushfire Statistics", icon = icon("fire", verify_fa = FALSE),
             menuSubItem("Major Bushfires", tabName = "bushfires", icon = icon("chart-bar", verify_fa = FALSE)),
             
             # Conditional filter attributes panel
             conditionalPanel(
               'input.tabs == "bushfires"',
               selectInput("attributeDevs", 
                           label = "Attributes", 
                           choices = c('Fatalities'='fatalities',
                                       'Homes Lost'='homeslost',
                                       'Area Burned (ha)'='area_burned_ha'),
                           selected = 'fatalities'
               )
             ),
             menuSubItem("2019-2020 Bushfire Season", tabName = "stats", icon = icon("chart-line", verify_fa = FALSE)),
             # Conditional filter range of dates panel
             conditionalPanel(
               'input.tabs == "stats"',
               dateRangeInput("range_date", 
                              "Select the range of dates",
                              start = min(ausTimeSeriesData$date),
                              end = max(ausTimeSeriesData$date),
                              min = min(ausTimeSeriesData$date),
                              max = max(ausTimeSeriesData$date))
             ),
             # Temperature and Rainfall Stats sub tab
             menuSubItem("Temperature and Rainfall Statistics", tabName = "raintemp", icon = icon("cloud", verify_fa = FALSE)),
             conditionalPanel(
               'input.tabs == "raintemp"',
               dateRangeInput("range_raintemp",
                              "Select the range of years",
                              start = min(tempRainData$year),
                              end = max(tempRainData$year),
                              min = min(tempRainData$year),
                              max = max(tempRainData$year),
                              format = "yyyy",
                              startview = "decade"))
             ),
    # Emergency Facilities Tab with 3 subtabs 
    menuItem("Emergency Facilities", icon = icon("first-aid", verify_fa = FALSE),
             menuSubItem("CFA & MFB Locations", tabName = "districts", icon = icon("tower-observation", verify_fa = FALSE)),
             menuSubItem("Neighbourhood Safer Places", tabName = "saferplaces", icon = icon("home", verify_fa = FALSE)),
             menuSubItem("Hospitals", tabName = "hospitals", icon = icon("hospital-symbol", verify_fa = FALSE))),
    # Incidents and Warnings Tab
    menuItem("Incidents & Warnings", tabName = "incidents", icon = icon("exclamation-triangle", verify_fa = FALSE), badgeLabel = "Live", badgeColor = "green")
  )
)

##########################################################################################

# Adding options for loader for heavy maps
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

##########################################################################################

# Adding Dashboard body
body <- dashboardBody(
  # Using custom CSS file to format sidebar and title headings
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    # Populating Victoria Bushfire History Tab
    tabItem(tabName = "history",
            # Heading
            h2("50 years of bushfires in Victoria"),
            # Fluid Rows for Bushfire History Map
            fluidRow(
              column(width = 12, box(width = NULL, 
                                        solidHeader = FALSE,
                                        # Adding Loader to Bushfire History Map
                                        withSpinner(leafletOutput('fire_history', height = 600), type = 2)))
            ),
            # Data Heading
            h2("Bushfires in Victoria - Data"),
            # Adding data table from Wikipedia
            fluidRow(
              column(width = 12, box(withSpinner(dataTableOutput('bushfire_data'), type = 2), width = NULL, solidHeader = FALSE))
            )
    ),
    # Populating the CFA Districts and stations tab
    tabItem(tabName = "districts",
            # Heading
            h2("CFA/MFB District Boundaries & Stations"),
            # Adding CFA stations maps with loader
            fluidRow(
              column(width = 12, box(width = NULL, 
                                     solidHeader = FALSE,
                                     # Adding Loader to Bushfire History Map
                                     withSpinner(leafletOutput('cfa_stations', height = 700), type = 2)))
            )
    ),
    # Populating the Safer Places Tab
    tabItem(tabName = "saferplaces",
            # Heading
            h2("Neighbourhood Safer Places"),
            # Adding the Safer Places Map with loader
            fluidRow(
              column(width = 12, box(withSpinner(leafletOutput('nsp_locations', height = 700), type = 2), 
                                     width = NULL,
                                     solidHeader = FALSE))
            )
    ),
    # Populating the 2019-2020 bushfire statistics Tab
    tabItem(tabName = "stats",
            # Heading
            h2("Australia Bushfire Statistics"),
            # Adding the Number of fires chart with loader
            fluidRow(
              column(width = 12, box(withSpinner(plotlyOutput('aus_fires'), type = 2),
                                    width = NULL,
                                    solidHeader = FALSE))
            ),
            # Adding the FRV and Brightness chart with loader
            fluidRow(
              column(width = 12, box(withSpinner(plotlyOutput('fire_intensity'), type = 2),
                                     width = NULL,
                                     solidHeader = FALSE))
            )
    ),
    # Populating the hospitals Tab
    tabItem(tabName = "hospitals",
            # Heading
            h2("Hospitals in Victoria"),
            # Adding the hospitals map with loader
            fluidRow(
              column(width = 12, box(withSpinner(leafletOutput('hospitals_vic', height = 700), type = 2), 
                                     width = NULL,
                                     solidHeader = FALSE))
            )
    ),
    # Populating the Major Bushfires Tab
    tabItem(tabName = "bushfires",
            # Heading
            h2("Major Bushfires in Australia"),
            # Adding the Scatterplot for Bushfire History
            # Adding the Devastation by state bar chart
            fluidRow(
              column(width = 12, box(withSpinner(plotlyOutput('devs_state', height = 600), type = 2),
                                     width = NULL,
                                     solidHeader = FALSE))
            ),
            fluidRow(
              column(width = 12, box(withSpinner(plotlyOutput('aus_history'), type = 2),
                                     width = NULL,
                                     solidHeader = FALSE))
            )
    ),
    # Populating the Incident Tab
    tabItem(tabName = "incidents",
            # Heading
            h2("Incident Report Victoria"),
            # Adding the live incidents map
            fluidRow(
              column(width = 12, box(withSpinner(leafletOutput('incidents_vic', height = 600), type = 2), 
                                     width = NULL,
                                     solidHeader = FALSE))
            ),
            # Heading
            h2("Total Fire Bans & Fire Danger Ratings - RSS Feed"),
            # Adding 3 days RSS feed text
            fluidRow(
              column(width = 4, box(htmlOutput('rssFrv1'), width = NULL)),
              column(width = 4, box(htmlOutput('rssFrv2'), width = NULL)),
              column(width = 4, box(htmlOutput('rssFrv3'), width = NULL))
            )
    ),
    # Populating the rainfall and temp statistics tab
    tabItem(tabName = "raintemp", 
            h2("Australia Weather Statistics"),
            # Adding Temperature Plot
            fluidRow(
              column(width = 12, box(withSpinner(plotlyOutput('avg_temp'), type = 2),
                                     width = NULL,
                                     solidHeader = FALSE))
            ),
            # Adding Rainfall Plot
            fluidRow(
              column(width = 12, box(withSpinner(plotlyOutput('avg_rainfall'), type = 2),
                                     width = NULL,
                                     solidHeader = FALSE))
            )
    )
  )
)

##########################################################################################

# Initializing dashboard page UI 
ui <- dashboardPage(
  # Adding header title
  dashboardHeader(title = "Victoria Fire Emergency",
                  titleWidth = 300),
  sidebar,
  body
)

##########################################################################################

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  values <- reactiveValues()
  
  ##########################################################################################
  # Creating the Victoria Bushfire History Table with options to search and sort
  output$bushfire_data <- renderDataTable(bushfireTable, 
                                          # Adding options for scroll, paginate, sort and search
                                          options = list(scrollX = TRUE,
                                                         paging = TRUE,
                                                         scrollY = TRUE,
                                                         autowidth = TRUE,
                                                         server = FALSE),
                                          filter = 'bottom',
                                          rownames = FALSE)
  
  ##########################################################################################
  # Creating the Victoria Bushfire History Raster Map
  output$fire_history <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Adding Raster To Map
      addRasterImage(fireHistoryRaster10, 
                     colors = pallete,
                     opacity = 0.8) %>%
      # Adding Legend for Raster
      addLegend(position = "bottomright", 
                pal = pallete,
                values = rasterMinMax,
                labFormat = labelFormat(transform = function(rasterMinMax) sort(rasterMinMax, decreasing = TRUE)),
                title = "Bushfire Severity") %>%
      # Adding Scale Bar
      addScaleBar(position = "bottomleft")
  })
  
  ##########################################################################################
  # Creating the CFA stations map 
  output$cfa_stations <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Adding shapefile data for boundaries of CFA with dashed lines
      addPolygons(data = cfaB,
                  weight = 2,
                  color = "red",
                  fill = FALSE,
                  smoothFactor = 1,
                  stroke = TRUE,
                  dashArray = c(5,5), 
                  highlight = highlightOptions(weight = 4,
                                               color = "red",
                                               fillOpacity = 0.2,
                                               bringToFront = TRUE)
      ) %>% 
      # Adding the fire stations point file to populate fire stations
      addAwesomeMarkers(data = fireS,
                        icon = awesomeIcons(icon = "fas fa-fire-extinguisher", 
                                            library = "fa", 
                                            iconColor = "black", 
                                            markerColor = "red"),
                        label = fireS$NAME_LABEL,
                        clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
      # Adding Minimap
      addMiniMap(toggleDisplay = TRUE) %>% 
      # Adding button for locating the user's current location
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
      # Adding search bar
      addSearchOSM() %>% 
      # Adding Scale bar
      addScaleBar(position = "bottomleft")
  })
  
  ##########################################################################################
  # Creating the Safer Places Map
  output$nsp_locations <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      # Adding the safer places data as markers
      addAwesomeMarkers(data = nsp, ~Longitude, ~Latitude,
                        icon = ~awesomeIcons("fas fa-home",
                                             library = "fa",
                                             iconColor = "black",
                                             markerColor = "red"),
                        label = nsp$Name,
                        popup = nsp$popup,
                        clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
      # Adding button for locating the user's current location
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
      # Adding Scale bar
      addScaleBar(position = "bottomleft") %>% 
      # Adding search bar
      addSearchOSM()
  })
  
  ##########################################################################################
  # Filtering the time series Australia Number of fires and Intensity charts with dates
  filteredStatsData <- reactive({
      filter(ausTimeSeriesData, ausTimeSeriesData$date >= input$range_date[1] & ausTimeSeriesData$date <= input$range_date[2])
  })
  
  ##########################################################################################
  # Creating the Australia Number of Bushfires Time series chart
  output$aus_fires <- renderPlotly({
    # Assigning filtered data a value
    ausTimeSeriesDataFiltered <- filteredStatsData()
    # Generating plot
    ausTimeSeriesDataPlot <- ggplot(data = ausTimeSeriesDataFiltered, aes(x = date, y = number_of_fires, group = 1,
                                                                  # Text for tooltip in plotly
                                                                  text = paste("Date: ", date,
                                                                               "<br>Number of Fires: ", number_of_fires))) +
      # Adding Lines
      geom_line(color = "orangered", size = 0.8) +
      # Title, X & Y axis titles
      labs(title = "Number of Fires Observed During 2019-2020 Fire Season", x = "Date", y = element_blank()) +
      # Adding Date Pretty Breaks for x axis
      scale_x_date(breaks = pretty_breaks()) +
      # Adding Continuous scale with pretty breaks
      scale_y_continuous(limits = c(0,max(ausTimeSeriesDataFiltered$number_of_fires)),
                         breaks = pretty_breaks(),
                         expand = c(0,0)) +
      # Adding theme
      theme(axis.title.x = element_text(vjust = 0, size = 12),
            plot.background = element_rect(color = "white"),
            panel.background = element_rect(color = "white", fill = "white"),
            plot.title = element_text(size = 14, face = "bold", margin = margin(10, 0, 10, 0)),
            panel.grid.major = element_line(color = "grey", size = 0.5))
    
    # Plotting using plotly with tooltip text and removing unused buttons
    ggplotly(ausTimeSeriesDataPlot, tooltip = "text", source = 'aus_fires') %>% 
      layout(dragmode = "pan") %>% 
      config(displayModeBar = "static", displaylogo = FALSE, modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines"))
  })
  
  ##########################################################################################
  # Creating the Fire intensity and Brightness Time Series chart
  output$fire_intensity <- renderPlotly({
    # Assigning filtered data a value
    ausTimeSeriesDataFiltered <- filteredStatsData()
    # Generating plot
    m <- ggplot(data = ausTimeSeriesDataFiltered, aes(x = date, group = 1,
                                              # Text for tooltip in plotly     
                                              text = paste("Date: ", date, 
                                                           "<br>Fire Radiative Power: ", round(avg_frp, digits = 2),
                                                           "<br>Brightness: ", round(avg_brightness, digits = 2)))) +
      # Lines for FRV
      geom_line(size = 0.8, aes(y = avg_frp, color = "FRP")) +
      # Lines for Brightness
      geom_line(size = 0.8, aes(y = avg_brightness, color = "Brightness"))  +
      # Title, X & Y axis titles
      labs(title = "Just how hot and intense were the 2019-20 fires?", 
           x = "Date", 
           y = element_blank()) +
      # Adding theme
      theme(axis.title.x = element_text(vjust = 0, size = 12),
            plot.background = element_rect(color = "white"),
            panel.background = element_rect(color = "white", fill = "white"),
            plot.title = element_text(size = 14, face = "bold", margin = margin(10, 0, 10, 0)),
            panel.grid.major = element_line(color = "grey", size = 0.5)) +
      # Adding Manual Color legend
      scale_color_manual("", 
                         breaks = c("FRP", "Brightness"),
                         values = c("FRP" = "orangered", "Brightness" = "#00AFBB")) +
      # Adding Date Pretty Breaks for x axis
      scale_x_date(breaks = pretty_breaks()) +
      # Adding Continuous scale with pretty breaks with limits based on both variables
      scale_y_continuous(limits = c(0, max(ausTimeSeriesDataFiltered$avg_frp, ausTimeSeriesDataFiltered$avg_brightness)), 
                         breaks = pretty_breaks(), 
                         expand = c(0,0))
    
    # Plotting using plotly with tooltip text and adding x unified tooltip
    ggplotly(m, tooltip = "text", source = 'fire_intensity') %>% 
      layout(dragmode = "pan", hovermode = "x unified") %>% 
      config(displayModeBar = "static", displaylogo = FALSE, modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines"))
  })
  
  ##########################################################################################
  # Creating Devastation by state bar plot
  output$devs_state <- renderPlotly({
    # Generating plot with y variable input from user selection
    devsByState <- ggplot(data = firesByState, aes(x = state, y = firesByState[,input$attributeDevs], group = 1,
                                                   # Text for tooltip
                                                   text = paste("State: ", state,"<br>",
                                                                input$attributeDevs, ": ", 
                                                                format_bignum(firesByState[,input$attributeDevs])))) +
      # Adding bar plots
      geom_bar(stat='identity', width=0.6, fill = "orangered") +
      # Title, X & Y axis titles
      labs(title = "Bushfire Devastation by State (2019 - 2020)", 
           x = element_blank(),
           y = element_blank()) +
      # Adding Continuous scale with pretty breaks with liimits based on user selected y axis variable
      scale_y_continuous(breaks = pretty_breaks(), limits = c(0, max(firesByState[,input$attributeDevs])), labels = comma) +
      # Adding theme
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
            plot.background = element_rect(color = "white"),
            panel.background = element_rect(color = "white", fill = "white"),
            plot.title = element_text(size = 14, face = "bold", margin = margin(10, 0, 10, 0)),
            panel.grid.major = element_line(color = "grey", size = 0.2))
    
    # Plotting using plotly with tooltip text and removing unused buttons
    ggplotly(devsByState, tooltip = "text", source = 'devs_state') %>% 
      layout(dragmode = "pan") %>% 
      config(displayModeBar = "static", displaylogo = FALSE, modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines"))
    
  })
  
  ##########################################################################################
  # Generating hospitals in Victoria Map
  output$hospitals_vic <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Adding hospitals markers
      addAwesomeMarkers(data = hospitals,
                        # Different icons based on type of hospital
                        icon = awesomeIcons(icon = case_when(hospitals$FEATSUBTYP == 'general hospital' ~ 'h-square',
                                                             hospitals$FEATSUBTYP == 'general hospital (emergency)' ~ 'ambulance',
                                                             TRUE ~ 'stethoscope'), 
                                            library = "fa", 
                                            iconColor = "black", 
                                            markerColor = case_when(hospitals$FEATSUBTYP == 'general hospital' ~ 'orange',
                                                                    hospitals$FEATSUBTYP == 'general hospital (emergency)' ~ 'red',
                                                                    TRUE ~ 'green')),
                        label = hospitals$NAME_LABEL,
                        popup = hospitals$popup,
                        clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
      # Adding mini map
      addMiniMap(toggleDisplay = TRUE) %>% 
      # Adding locate me button
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
      # Adding search button
      addSearchOSM() %>% 
      # Adding scale bar
      addScaleBar(position = "bottomleft")
  })
  
  ##########################################################################################
  # Creating the Australia bushfires devastaion statistics chart
  output$aus_history <- renderPlotly({
    # Taking user input for y axis variable
    ausHistoryBushfires <- ggplot(data = ausHistory, aes(x = year, y = ausHistory[,input$attributeDevs], group = 1,
                                        # Text for tooltip                 
                                       text = paste0('<b>', name, '</b><br>',
                                                     'Year: ', year, '<br>',
                                                     input$attributeDevs, ": ", format_bignum(ausHistory[,input$attributeDevs])))) +
      # Adding points on the chart
      geom_point(color = "firebrick", size = 2, alpha = 0.4) +
      # Adding title
      labs(title = "Major Bushfires in Australia",
           x = "Year",
           y = element_blank()) +
      # Adding y continuous scale based on y axis variable selection
      scale_y_continuous(breaks = pretty_breaks(),
                         limits = c(0, max(ausHistory[,input$attributeDevs])),
                         labels = comma) +
      # Adding x continuous scale with 12 year span
      scale_x_continuous(limits = c(min(ausHistory$year), max(ausHistory$year)+10), 
                         breaks = seq(min(ausHistory$year), max(ausHistory$year)+10, 12)) +
      # Adding theme
      theme(axis.text.x = element_text(vjust = 1, hjust = 1),
            plot.background = element_rect(color = "white"),
            panel.background = element_rect(color = "white", fill = "white"),
            plot.title = element_text(size = 14, face = "bold", margin = margin(10, 0, 10, 0)),
            panel.grid.major = element_line(color = "grey", size = 0.2))
    
    # Plotting using plotly with tooltip text and removing unused buttons
    ggplotly(ausHistoryBushfires, tooltip = "text", source = 'aus_history') %>% 
      layout(dragmode = "pan") %>% 
      config(displayModeBar = "static", displaylogo = FALSE, modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines"))
  })
  
  ##########################################################################################
  # Rendering the RSS Feeds for different days
  output$rssFrv1 <- renderPrint({
    cat(rssFeedData1)
  })
  
  output$rssFrv2 <- renderPrint({
    cat(rssFeedData2)
  })
  
  output$rssFrv3 <- renderPrint({
    cat(rssFeedData3)
  })
  
  ##########################################################################################
  # Creating the live incidents in Victoria map
  output$incidents_vic <- renderLeaflet({
    leaflet(incidents) %>% 
      addTiles() %>% 
      # Adding markers based on the data
      addAwesomeMarkers(~longitude, ~latitude, 
                        icon = ~awesomeIcons("fas fa-exclamation",
                                             library = "fa",
                                             iconColor = "black",
                                             markerColor = "orange"),
                        label = incidents$incidentLocation,
                        popup = incidents$popup) %>% 
      # Adding locate me button
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
      # Adding the search option
      addSearchOSM() %>% 
      # Adding scale bar
      addScaleBar(position = "bottomleft")
  })
  
  ##########################################################################################
  # Filtering the time series Australia temperature and rainfall chart with dates
  filteredTempRainfallData <- reactive({
    filter(tempRainData, tempRainData$year >= input$range_raintemp[1] & tempRainData$year <= input$range_raintemp[2])
  })
  
  ##########################################################################################
  # Creating chart for temperature in Australia
  output$avg_temp <- renderPlotly({
    # Adding data and text for tooltip
    temp <- ggplot(data = filteredTempRainfallData(), aes(x = year, y = avg_annual_temp, color = avg_annual_temp, group = 1,
                                                text = paste('Year: ', year(year), '<br>',
                                                             'Avg Annual Temperature: ', round(avg_annual_temp, digits = 1), '°C')
                                                )) +
      #plotting points
      geom_point() +
      labs(title = "Average Annual Temperature in Australia (1956-2019)",
           x = "Year", 
           y = "Temperature (°C)",
           color = "Temperature (°C)") +
      # Using viridis color scale
      scale_color_viridis_c(option = "magma",
                            guide = guide_colorsteps()) +
      # Creating theme
      theme(axis.title.x = element_text(vjust = 0, size = 12),
            axis.title.y = element_text(vjust = 1, size = 12),
            plot.background = element_rect(color = "white"),
            panel.background = element_rect(color = "white", fill = "white"),
            plot.title = element_text(size = 14, face = "bold", margin = margin(10, 0, 10, 0)),
            panel.grid.major = element_line(color = "grey", size = 0.5))
    
    
    # Plotting using plotly with tooltip text and removing unused buttons
    ggplotly(temp, tooltip = "text", source = 'avg_temp') %>% 
      layout(dragmode = "pan") %>% 
      config(displayModeBar = "static", displaylogo = FALSE, modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines"))
  })
  
  ##########################################################################################
  # Creating chart for rainfall in Australia
  output$avg_rainfall <- renderPlotly({
    # Adding data and text for tooltip
    rainfall <- ggplot(data = filteredTempRainfallData(), aes(x = year, y = avg_annual_rainfall, color = avg_annual_rainfall, group = 1,
                                                text = paste('Year: ', year(year), '<br>',
                                                             'Avg Annual Rainfall: ', round(avg_annual_rainfall, digits = 1), 'ml')
                                                )) +
      # Adding points
      geom_point() +
      labs(title = "Average Annual Rainfall in Australia (1956-2019)",
           x = "Year", 
           y = "Rainfall (ml)",
           color = "Rainfall (ml)") +
      #Using basic color palette
      guides(color = guide_colorsteps()) +
      # Creating theme
      theme(axis.title.x = element_text(vjust = 0, size = 12),
            axis.title.y = element_text(vjust = 1, size = 12),
            plot.background = element_rect(color = "white"),
            panel.background = element_rect(color = "white", fill = "white"),
            plot.title = element_text(size = 14, face = "bold", margin = margin(10, 0, 10, 0)),
            panel.grid.major = element_line(color = "grey", size = 0.5))
    
    # Plotting using plotly with tooltip text and removing unused buttons
    ggplotly(rainfall, tooltip = "text", source = 'avg_rainfall') %>% 
      layout(dragmode = "pan") %>% 
      config(displayModeBar = "static", displaylogo = FALSE, modeBarButtonsToRemove = list("sendDataToCloud", "toImage", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "toggleSpikelines"))
  })
  
}

##########################################################################################
##########################################################################################

#############
# RUN SHINY #
#############

shinyApp(ui, server, options=list(launch.browser=TRUE))

##########################################################################################
##########################################################################################