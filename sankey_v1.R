# set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
cat("\014")

########################################################################
packageList = c('shiny', 'GGally', 'sp', 'rworldmap', 'tidyr', 'dplyr', 'googleVis', 'countrycode')
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

########################################################################

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


########################################################################

ui <- shinyUI(fluidPage(
  
  titlePanel("GDELT"),
  
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
))

########################################################################
#### Server ####
server <- function(input, output) {
    
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
}

#### Run ####
shinyApp(ui = ui, server = server)
