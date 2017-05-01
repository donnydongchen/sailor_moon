# Be sure you are using R-3.3.2 or later version.
# If not, download from https://cran.r-project.org/bin/macosx/
setwd('/Users/amiee/Desktop/Homework/data_viz/')

rm(list = ls())
cat("\014") 
########################################################################
# load required libraries (install if libraries not found)
if ("devtools" %in% rownames(installed.packages())==F) {
  devtools::install_github("hadley/devtools")}
if ("dplyr" %in% rownames(installed.packages())==F) {
  devtools::install_github("hadley/dplyr")}
if ("trelliscopejs" %in% rownames(installed.packages())==F) {
  devtools::install_github("hafen/trelliscopejs")}
if ("hrbrthemes" %in% rownames(installed.packages())==F) {
  devtools::install_github("hrbrmstr/hrbrthemes")}
if ("tidyverse" %in% rownames(installed.packages())==F) {
  install.packages("tidyverse", repos="http://cran.us.r-project.org")}
if ("stringr" %in% rownames(installed.packages())==F) {
  install.packages("stringr", repos="http://cran.us.r-project.org")}
if ("gdeltr2" %in% rownames(installed.packages())==F) {
  devtools::install_github("abresler/gdeltr2")}
library(gdeltr2)
library(shiny)
library(leaflet)
load_needed_packages(c('dplyr', 'magrittr'))

########################################################################

gkg_summary_count_may_15_16_2014 <-
  get_data_gkg_days_summary(
    dates = c('2014-05-15', '2014-05-16'),
    is_count_file = T,
    return_message = T
  )

gkg_summary_count_may_15_16_2014 <- data.frame(gkg_summary_count_may_15_16_2014)
data <- gkg_summary_count_may_15_16_2014[c('countArticles', 'typeEvent', 'countObject',
                                         'idCountry', 'latitude', 'longitude', 'sources',
                                         'urlSources', 'location')]

for (col in c('typeEvent', 'idCountry')) {
  data[col] <- lapply(data[col], factor)
}
data <- subset(data, data$typeEvent %in% c("KILL", "KIDNAP", "WOUND", "TERROR","ASSASSINATION"))
data$typeEvent <- droplevels(data$typeEvent)
data$countArticles <- round(data$countArticles/10+1)

ui <- fluidPage(
  sidebarPanel(
    checkboxGroupInput("type", "Event Types", levels(data$typeEvent), 
                       selected = c("ASSASSINATION", "TERROR"))
    ),
  mainPanel(
    leafletOutput("mymap", height=600)
  )
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    df <- subset(data, data$typeEvent %in% input$type)
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
}

shinyApp(ui = ui, server = server)



