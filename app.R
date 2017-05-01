# set working directory to source file location
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# rm(list = ls())
# cat("\014") 

#======================================== packages =============================================
packageList = c('shiny', 'GGally', 'sp', 'rworldmap', 'tidyr', 'dplyr', 'googleVis', 'countrycode', 'leaflet', 'magrittr')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

if ("gdeltr2" %in% rownames(installed.packages())==F) {
  devtools::install_github("abresler/gdeltr2")}
library(gdeltr2)

library(shiny)
library(GGally)
library(sp)
library(rworldmap)
library(tidyr)
library(dplyr)
library(googleVis)
library(countrycode)
library(leaflet)
library(magrittr)

#======================================== functions =============================================

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

#======================================== shiny UI =============================================

ui <- navbarPage("GDELT: Project Sailor Moon",
                 
                 #======================================== Map =============================================
                 tabPanel("World Map",
                          sidebarPanel(
                            dateRangeInput("date_range_map", "Date range:",
                                           start = Sys.Date() - 1,
                                           end = Sys.Date() - 1
                            ),
                            checkboxGroupInput("type", "Event Types",
                                               c('KILL', 'ARREST', 'PROTEST', 'WOUND', 'AFFECT', 'CRISISLEX_T03_DEAD',
                                                 'CRISISLEX_CRISISLEXREC', 'CRISISLEX_T02_INJURED', 'CRISISLEX_C07_SAFETY',
                                                 'CRISISLEX_C03_WELLBEING_HEALTH'),
                                               selected = c("WOUND"))
                          ),
                          mainPanel(
                            leafletOutput("mymap", height=600)
                          )
                          ),
                 
                 
                 #======================================== sankey plot =============================================  
                 tabPanel("Sankey Plot",
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput("date_range_sankey", "Date range:",
                                             start = Sys.Date() - 1,
                                             end = Sys.Date() - 1
                              ),
                              radioButtons("end", "Sankey Plot Endpoints:",
                                           c("Continents", "Countries")
                              ),
                              selectizeInput("type_sankey", "Select News Type", 
                                             c('KILL', 'ARREST', 'PROTEST', 'WOUND', 'AFFECT', 'CRISISLEX_T03_DEAD',
                                               'CRISISLEX_CRISISLEXREC', 'CRISISLEX_T02_INJURED', 'CRISISLEX_C07_SAFETY',
                                               'CRISISLEX_C03_WELLBEING_HEALTH'),
                                             selected = c('KILL', 'ARREST', 'PROTEST', 'WOUND', 'AFFECT'),
                                             multiple = TRUE
                              ),
                              img(src='sailor_moon.png', align = "left")
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                # tabPanel("Sankey Plot", verbatimTextOutput("text")),
                                tabPanel("Sankey Plot", htmlOutput("sankey", inline = TRUE))
                              )
                            )
                          )
                          ),
                 
                 #======================================== timeline plot =============================================  
                 tabPanel("Timeline Plot",
                          sidebarLayout(
                            sidebarPanel(
                              dateInput("date_timeline", "Date:", value = "2016-06-17"),
                              selectizeInput("type_timeline", "Select News Type", 
                                             c('BLOOMBERG', 'CNBC', 'CNNW', 'COM', 'CSPAN2', 'CSPAN3', 'CSPAN', 'FBC', 'FOXNEWSW', 'KCSM',
                                               'KGO', 'KNTV', 'KOFY', 'KPIX', 'KQED', 'KQEH', 'KRON', 'KTVU', 'KYW', 'LINKTV', 'MSNBCW',
                                               'SFGTV', 'WCAU', 'WJLA', 'WPVI', 'WRC', 'WTTG', 'WTXF', 'WUSA'
                                             ),
                                             selected = c('BLOOMBERG', 'CNBC', 'FOXNEWSW', 'MSNBCW', 'CSPAN'),
                                             multiple = TRUE
                              ),
                              img(src='sailor_moon.png', align = "left")
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                # tabPanel("Sankey Plot", verbatimTextOutput("text")),
                                tabPanel("TV News Timetable", htmlOutput("tv", inline = TRUE))
                              )
                            )
                          )
                          )
                 
                 
)




#======================================== shiny server =============================================
server <- function(input, output) {
  
  #======================================== Map =============================================
  data <- reactive({
    dates <- as.character(seq(input$date_range_map[1], input$date_range_map[2], by="days"))
    
    gkg_map <-
      get_data_gkg_days_summary(
        dates = dates,
        is_count_file = T,
        return_message = T
      )
    
    gkg_map <- data.frame(gkg_map)
    data <- gkg_map[c('countArticles', 'typeEvent', 'countObject',
                      'idCountry', 'latitude', 'longitude', 'sources',
                      'urlSources', 'location')]
    
    for (col in c('typeEvent', 'idCountry')) {
      data[col] <- lapply(data[col], factor)
    }
    data <- subset(data, data$typeEvent %in% c("KILL", "KIDNAP", "WOUND", "TERROR","ASSASSINATION"))
    data$typeEvent <- droplevels(data$typeEvent)
    data$countArticles <- round(data$countArticles/10+1)
    data
  })
  
  
  output$mymap <- renderLeaflet({
    df <- subset(data(), data()$typeEvent %in% input$type)
    getColor <- function(quakes) {
      sapply(df$typeEvent, function(typeEvent) {
        if(typeEvent == "KILL") {
          "green"
        } else if(typeEvent == "WOUND") {
          "orange"
        } else if(typeEvent == "KIDNAP") {
          "purple"
        } else if(typeEvent == "TERROR") {
          "blue"
        } else if(typeEvent == "ASSASSINATION"){
          "red"
        } })
    }
    icons <- awesomeIcons(
      icon = 'ios-close',
      library = 'ion',
      markerColor = getColor(df)
    )
    m <- leaflet(df) %>% addProviderTiles(providers$Esri.WorldTopoMap)
    if (!is.null(input$type)) {
      m <- m %>% addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=icons,
                                   label= ~location,
                                   popup = ~paste("<b> Location: ", location, "</b>",
                                                  "<br><b> Source: ", sources, "</b>",
                                                  "<br><b> Number of Articles: ", countArticles, "</b>",
                                                  "<br><a><href>",  urlSources, "</a>"))
    }
    m
  })
  
  
  
  #======================================== sankey plot =============================================  
  # Reactive
  
  # Date
  gkg_summary <- reactive({
    
    dates <- as.character(seq(input$date_range_sankey[1], input$date_range_sankey[2], by="days"))
    # dates <- c('2017-03-29', '2017-03-30')
    gkg_summary <- get_data_gkg_days_summary(dates = dates, is_count_file = T, return_message = F) %>% 
      drop_na(longitude, latitude)
    
    gkg_summary$continent <- coords2continent(gkg_summary[,c('longitude', 'latitude')])
    gkg_summary$country <- countrycode(gkg_summary$idCountry, "iso2c", "country.name")
    
    gkg_summary %>%
      subset(!is.na(latitude)) %>%
      subset(!is.na(longitude)) %>%
      filter(typeLocation %in% c('country', 'usState', 'usCity'))
  })
  
  # End
  df <- reactive ({
    if (input$end == "Continents") {
      gkg_summary() %>% 
        group_by(typeEvent, continent) %>%
        summarize(count = n())
    }
    else if (input$end == "Countries") {
      gkg_summary() %>% 
        group_by(typeEvent, country) %>%
        summarize(count = n())
    }
    
  })  
  
  # Type
  df2 <- reactive ({
    df() %>%
      filter(typeEvent %in% input$type_sankey)
  })
  
  
  # Output
  # output$text <- renderText(df())
  
  output$sankey <- renderGvis(gvisSankey(df2(), from="typeEvent",
                                         to="continent", weight="count",
                                         options=list(height=1000,width=1000,
                                                      sankey="{link: {color: { fill: '#d799ae' } },
                                                      node: { color: { fill: '#a61d4c' },
                                                      label: { color: '#871b47' } }}"
                                         ))
                                         )
  
  
  
  #======================================== timeline plot =============================================
  
  # Date
  gkg_tv_test <- reactive({
    dates <- as.character(input$date_timeline)
    gkg_tv_test <- get_data_gkg_tv_days(dates = dates, return_message = T)
    gkg_tv_test
  })
  
  # UI
  # output$type <- renderUI({
  #   selectizeInput("type", "Select News Type", 
  #                unique(gkg_tv_test()$nameSource),
  #                selected = c('BLOOMBERG'),
  #                multiple = TRUE
  #                )
  #   })
  
  df_tv <- reactive ({
    df <- gkg_tv_test()
    df <- df[with(df, order(nameSource, dateTimeDocument)),]
    df <- 
      df %>%
      group_by(nameSource) %>% 
      mutate(end_time = lead(dateTimeDocument, 1))
    df$end_time[is.na(df$end_time)] <- df$dateTimeDocument[is.na(df$end_time)]+1*60*60
    df
  })
  
  # Type
  df_tv_2 <- reactive ( df_tv() %>% filter(nameSource %in% input$type_timeline) )
  
  # Output
  # output$text <- renderText(df())
  
  output$tv <- renderGvis(
    gvisTimeline(data=df_tv_2(), 
                 rowlabel="nameSource", barlabel="nameTVShow", 
                 start="dateTimeDocument", end="end_time",
                 options=list(height=1000,width=1000)
    ))
  
  
  
  
}

#### Run ####
shinyApp(ui = ui, server = server)
