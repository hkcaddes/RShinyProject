## server.R code ##

library(googleVis)
library(ggmap)
library(leaflet)
library('leaflet.extras')
library(ggplot2)
library(scales)
library(colormap)

# function for school type legend labels
labs <- function(value) {
  ifelse(value == 1, "Public",
         ifelse(value == 2, "Private non-profit", "Private for-profit"))
}


shinyServer(function(input, output, session){
  
  ## Interactive map ##
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("OpenStreetMap.Mapnik") # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  })
  
  # observer for circles and legend
  
  observe({
    # color by type of school selected
    schoolType <- schools.df %>% filter(ownership %in% input$owner)
    pal <- colorFactor(c("viridis"),
                       domain = c(1,2,3)) # 1 for public, 2 for private non-profit, 3 for private for-profit
    
    
    # leaflet proxy map
    leafletProxy("map", data = schoolType) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~long, lat = ~lat, radius = 4, layerId = ~name,
                 stroke = FALSE, fillOpacity = 0.7, color = pal(schoolType$ownership)) %>%
      addLegend("bottomleft", colors = c(pal(1), pal(2), pal(3)), labels = c("Public", "Private non-profit", "Private for-profit"),
                layerId = "colorLegend", title = "School Type")
    
    # size by size of schools selected
    #sizeBy <- input$size
    #radius <- schools.df[[input$size]]/max(schools.df[[input$size]] * 30000)
    
    #c("red", "blue", "orange")
    #pal = pal, values = ~schoolType$ownership,
  #rgba(68,1,84,1)"    "rgba(33,144,141,1)" "rgba(253,231,37,1)
    #rgb(68,1,84), rgb(33,144,141), rgb(235,231,37)
    
  })
  
  
})