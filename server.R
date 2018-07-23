## server.R code ##

library(googleVis)
library(ggmap)
library(leaflet)
library('leaflet.extras')
library(ggplot2)
library(scales)

# list religious and special affiliations
religions <- subset(myDict, variable_name == "religious_affiliation", select = c(value, label))
# function to return list of a specific school's religious affiliation or special interest affiliation
affiliations <- function(schoolName) {
  school = schools.df %>% filter(name == schoolName)
  religion = ifelse(is.na(school$religious_affiliation) == TRUE, "None", religions %>% filter(value == school$religious_affiliation) %>% select(label))
  affiliation = ifelse(school$minority_serving.historically_black == 1, "HBC",
                       ifelse(school$minority_serving.tribal == 1, "Tribal",
                              ifelse(school$women_only == 1, "Women Only",
                                     ifelse(school$men_only == 1, "Men Only", "None"))))
  
  return(c(religion, affiliation))
}

shinyServer(function(input, output, session){
  
  ## blank nteractive map output ##
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addProviderTiles("OpenStreetMap.Mapnik") # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  })
  
  
  ## show pop-up for schools ##
  showSchoolPopup <- function(schoolName, lat, lng) {
    selectedSchool = schools.df %>% filter(name == schoolName)
    content = as.character(tagList(
      tags$b(tags$a(selectedSchool$name, href = paste0("https://", selectedSchool$school_url))), tags$br(),
      tags$b(HTML(sprintf("%s, %s",
                               selectedSchool$city, selectedSchool$state))),
      tags$br(),
      sprintf("UGrad size: %s", selectedSchool$size), tags$br(),
      sprintf("Average tuition & fees: %s", selectedSchool$attendance.academic_year), tags$br(),
      sprintf("Admission Rate: %s", selectedSchool$admission_rate.overall), tags$br(),
      sprintf("SAT verbal (25th percentile): %s", selectedSchool$sat_scores.25th_percentile.critical_reading), tags$br(),
      sprintf("SAT math (25th percentile): %s", selectedSchool$sat_scores.25th_percentile.math), tags$br(),
      sprintf("ACT (25th percentile): %s", selectedSchool$act_scores.25th_percentile.cumulative), tags$br(),
      sprintf("4 yr Completiion Rate: %s", selectedSchool$completion_rate_4yr_150nt), tags$br(),
      sprintf("Federal Loan Rate: %s", selectedSchool$federal_loan_rate), tags$br(),
      sprintf("Median Debt (upon completion): %s", selectedSchool$median_debt.completers.overall), tags$br(),
      sprintf("Median Salary 6 yrs after entry (25-75 percentile): %s-%s", selectedSchool$six_years_after_entry.working_not_enrolled.earnings_percentile.25, selectedSchool$six_years_after_entry.working_not_enrolled.earnings_percentile.25), tags$br(),
      sprintf("Relgious Affiliation: %s", affiliations(selectedSchool$name)[1]), tags$br(),
      sprintf("Special Interest: %s", affiliations(selectedSchool$name)[2])
    ))
    
    
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = schoolName)
  }
  
  
  # observer for type of school and legend
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
  
  
  
 
  
  # when map is clicked, show popup with info
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click

    if (is.null(event))
       return()
    isolate({
      showSchoolPopup(event$id, event$lat, event$lng)
    })
    

  })
  
  
})
