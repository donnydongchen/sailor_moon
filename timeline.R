# set working directory to source file location
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
cat("\014")

#======================================== packages =============================================
packageList = c('shiny', 'GGally', 'sp', 'rworldmap', 'tidyr', 'dplyr', 'googleVis', 'countrycode', 'data.table')
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
library(data.table)

#======================================== shiny UI =============================================

ui <- shinyUI(fluidPage(
  
  titlePanel("GDELT"),
  
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
))

#======================================== shiny server =============================================
server <- function(input, output) {
    
    # Reactive
  
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




# 
# 
# gkg_tv_test <- get_data_gkg_tv_days(dates = "2016-06-17", return_message = T)
# 
# gkg_tv_test <- gkg_tv_test[with(gkg_tv_test, order(nameSource, dateTimeDocument)),]
# 
# gkg_tv_test <-
#   gkg_tv_test %>%
#   group_by(nameSource) %>%
#   mutate(end_time = lead(dateTimeDocument, 1)) %>%
#   filter(nameSource %in% c('BLOOMBERG', 'CNBC', 'CSPAN', 'FOXNEWSW', 'MSNBCW'))
# 
# gkg_tv_test$end_time[is.na(gkg_tv_test$end_time)] <- gkg_tv_test$dateTimeDocument[is.na(gkg_tv_test$end_time)]+1*60*60
# 
# 
# 
# plot(
#   gvisTimeline(data=gkg_tv_test,
#                rowlabel="nameSource", barlabel="nameTVShow",
#                start="dateTimeDocument", end="end_time",
#                options=list(height=1000,width=1000)
#                )
# )
# c('BLOOMBERG', 'CNBC', 'CNNW', 'COM', 'CSPAN2', 'CSPAN3', 'CSPAN', 'FBC', 'FOXNEWSW', 'KCSM',
#   'KGO', 'KNTV', 'KOFY', 'KPIX', 'KQED', 'KQEH', 'KRON', 'KTVU', 'KYW', 'LINKTV', 'MSNBCW',
#   'SFGTV', 'WCAU', 'WJLA', 'WPVI', 'WRC', 'WTTG', 'WTXF', 'WUSA'
#   )

