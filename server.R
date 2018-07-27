## server.R code ##

library(googleVis)
library(ggmap)
library(scales)
library(plotly)
library(shinyjs)
library(leaflet)



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
  
  
  ######## show/hide filters ##############
  
  observeEvent(input$show_locale, {
    toggle("locale")
  })
  observeEvent(input$show_size, {
    toggle("size")
  })
  observeEvent(input$show_cost, {
    lapply(c("tuition", "debt", "earnings", "na_tuition", "na_debt", "na_earnings"), toggle)
  })
  observeEvent(input$show_admin, {
    lapply(c("selectivity", "sat_verbal", "sat_math","act", "intake", "na_intake", "na_sat", "na_act"), toggle)
  })
  observeEvent(input$show_major, {
    toggle("major")
  })
  
  
  
  ######## show pop-up for schools ###########
  
  showSchoolPopup <- function(schoolName, lat, lng) {
    selectedSchool = schools.df %>% filter(name == schoolName)
    content = as.character(tagList(
      tags$b(tags$a(selectedSchool$name, href = paste0("https://", selectedSchool$school_url))), tags$br(),
      tags$b(HTML(sprintf("%s, %s",
                          selectedSchool$city, selectedSchool$state))),
      tags$br(),
      sprintf("Undergrad size: %s", format(selectedSchool$size, big.mark = ",", scientific = FALSE)), tags$br(),
      sprintf("In-State tuition & fees: $%s", format(selectedSchool$tuition.in_state, big.mark = ",", scientific = FALSE)), tags$br(),
      sprintf("Out-of-State tuition & fees: $%s", format(selectedSchool$tuition.out_of_state, big.mark = ",", scientific = FALSE)), tags$br(),
      sprintf("Admission Rate: %s%%", selectedSchool$admission_rate.overall * 100), tags$br(),
      sprintf("SAT verbal (25th percentile): %s", selectedSchool$sat_scores.25th_percentile.critical_reading), tags$br(),
      sprintf("SAT math (25th percentile): %s", selectedSchool$sat_scores.25th_percentile.math), tags$br(),
      sprintf("ACT (25th percentile): %s", selectedSchool$act_scores.25th_percentile.cumulative), tags$br(),
      sprintf("4 yr Completion Rate: %s%%", selectedSchool$completion_rate_4yr_150nt * 100), tags$br(),
      sprintf("Federal Loan Rate: %s%%", selectedSchool$federal_loan_rate * 100), tags$br(),
      sprintf("Median Debt (upon completion): $%s", format(selectedSchool$median_debt.completers.overall, big.mark = ",", scientific = FALSE)), tags$br(),
      sprintf("Median Salary 6 yrs after entry: $%s", format(selectedSchool$six_years_after_entry.median, big.mark = ",", scientific = FALSE)), tags$br(),
      sprintf("Relgious Affiliation: %s", affiliations(selectedSchool$name)[1]), tags$br(),
      sprintf("Special Interest: %s", affiliations(selectedSchool$name)[2])
    ))
    
    
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = schoolName)
  }
  
  
  
  
  ###### Reactive expression for map input variables ################
  
  data <- schools.df
  makeReactiveBinding("data")
  
  newData <- reactive({
    
    data <- schools.df
    
    
    ### public vs. private
    data <- data %>% filter(ownership %in% input$owner)
    
    
    ### undergrad enrolment size
    data <- data %>% filter(is.na(size) | size %in% c(seq(input$size[1], input$size[2])))
    
    
    ### locale
    data <- data %>% filter(is.na(locale) | locale %in% input$locale)
    
    
    
    ### cost and outcomes
    
    # tuition & fees
    if (input$na_tuition == TRUE) {
      data <- data %>% filter(is.na(tuition.out_of_state) | tuition.out_of_state %in% c(seq(0, input$tuition[1])))
    } else {
      data <- data %>% filter(tuition.out_of_state %in% c(seq(0, input$tuition[1])))
    }
    
    # debt
    if (input$na_debt == TRUE) {
      data <- data %>% filter(is.na(median_debt.completers.overall) | median_debt.completers.overall %in% c(seq(0, input$debt[1])))
    } else {
      data <- data %>% filter(median_debt.completers.overall %in% c(seq(0, input$debt[1])))
    }
    
    # earnings
    if (input$na_earnings == TRUE) {
      data <- data%>% filter(is.na(six_years_after_entry.median) | six_years_after_entry.median %in% c(seq(input$earnings[2], input$earnings[1])))
    } else {
      data <- data %>% filter(six_years_after_entry.median %in% c(seq(input$earnings[2], input$earnings[1])))
    }
    
    
    
    ### admissions
    
    # admissions rate
    if (input$na_intake == TRUE) {
      data <- data %>% filter(is.na(admissions) | (admissions >= input$intake[1]/100 & admissions <= input$intake[2]/100))
    } else {
      data <- data %>% filter(admissions >= input$intake[1]/100 & admissions <= input$intake[2]/100)
    }
    
    # act
    if (input$na_act == TRUE) {
      data <- data %>% filter(is.na(act_scores.25th_percentile.cumulative) | act_scores.25th_percentile.cumulative %in% c(seq(input$act[1], input$act[2])))
    } else {
      data <- data %>% filter(act_scores.25th_percentile.cumulative %in% c(seq(input$act[1], input$act[2])))
    }
    
    
    # sat
    if (input$na_sat == TRUE) {
      # sat verbal
      data <- data %>% filter(is.na(sat_scores.25th_percentile.critical_reading) | sat_scores.25th_percentile.critical_reading %in% c(seq(input$sat_verbal[1], input$sat_verbal[2])))
      # sat math
      data <- data %>% filter(is.na(sat_scores.25th_percentile.math) | sat_scores.25th_percentile.math %in% c(seq(input$sat_math[1], input$sat_math[2])))
    } else {
      # sat verbal
      data <- data %>% filter(sat_scores.25th_percentile.critical_reading %in% c(seq(input$sat_verbal[1], input$sat_verbal[2])))
      # sat math
      data <- data %>% filter(sat_scores.25th_percentile.math %in% c(seq(input$sat_math[1], input$sat_math[2])))
    }

  })
  
  
  
  
  
  ###### Observer for proxy leaflet plot ##############
  observe({
    pal <- colorFactor(c("viridis"), domain = c(1,2,3))
    
    ## MAJOR
    
    filterSchools = newData()
    
    if (1 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.agriculture == TRUE)
    }
    if (2 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.architecture == TRUE)
    }
    if (3 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.biological == TRUE)
    }
    if (4 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.business_marketing == TRUE)
    }
    if (5 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.communication == TRUE)
    }
    if (6 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.computer == TRUE)
    }
    if (7 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.personal_culinary == TRUE)
    }
    if (8 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.education == TRUE)
    }
    if (9 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.engineering == TRUE)
    }
    if (10 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.english == TRUE)
    }
    if (11 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.ethnic_cultural_gender == TRUE)
    }
    if (12 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.language == TRUE)
    }
    if (13 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.health == TRUE)
    }
    if (14 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.history == TRUE)
    }
    if (15 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.legal == TRUE)
    }
    if (16 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.humanities == TRUE)
    }
    if (17 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.mathematics == TRUE)
    }
    if (18 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.military == TRUE)
    }
    if (19 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.multidiscipline == TRUE)
    }
    if (20 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.philosophy_religious == TRUE)
    }
    if (21 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.physical_science == TRUE)
    }
    if (22 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.psychology == TRUE)
    }
    if (23 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.public_administration_social_service == TRUE)
    }
    if (24 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.science_technology == TRUE)
    }
    if (25 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.social_science == TRUE)
    }
    if (26 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.theology_religious_vocation == TRUE)
    }
    if (27 %in% input$major) {
      filterSchools = filterSchools %>% filter(program.bachelors.visual_performing == TRUE)
    }
    
    ####### UPDATE MAP ###########################################
    
    leafletProxy("map", data = filterSchools) %>%
      clearMarkers() %>%
      
      addCircleMarkers(lng = ~long, lat = ~lat, radius = 10, layerId = ~name,
                       stroke = FALSE, fillOpacity = 0.7, color = pal(filterSchools$ownership)) %>%
      
      addLegend("bottomleft", colors = c(pal(1), pal(2), pal(3)), labels = c("Public", "Private non-profit", "Private for-profit"),
                layerId = "colorLegend", title = "School Type")
    
    
    ####### DATA EXPLORER based on these observations ##############################
    
    # update state selection based on map filters
    # updateSelectInput(session, "states", choices = filterSchools %>% select(state) %>% mutate(state = state.name[match(state,state.abb)]) %>%
    #                     distinct() %>% arrange(state) %>% drop_na())
    
    # update city input with selected states
    cities = if (is.null(input$states)) character(0) else {
      filter(schools.df, state %in% input$states) %>%
        select(city) %>%
        distinct() %>%
        arrange(city)
    }
    stillSelected = isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities, selected = stillSelected)
    
    
    
    output$data <- DT::renderDataTable({
      
      df = filterSchools
      df = df %>% filter(is.null(input$states) | state %in% input$states, is.null(input$cities) | city %in% input$cities) %>%
        #mutate(religion = affiliations(name)[1], special = affiliations(name)[2]) %>%
        select("Name" = name, "City" = city, "State" = state,
               "UGrad Enrollment" = size, "In-State Tuition & Fees" = tuition.in_state,
               "Out-of-State Tuition & Fees" = tuition.out_of_state, "Admission Rate" = admission_rate.overall,
               "SAT Verbal (25th Percentile" = sat_scores.25th_percentile.critical_reading,
               "SAT Math (25th Percentile" = sat_scores.25th_percentile.math,
               "ACT (25th Percentile" = act_scores.25th_percentile.cumulative,
               "Completion Rate" = completion_rate_4yr_150nt,
               #"Religious Affiliation" = religion,
               #"Special Interest = special
               "Median Salary (6 yrs after entry)" = six_years_after_entry.median,
               "Median Debt (upon completion)" = median_debt.completers.overall,
               "Federal Loan Rate" = federal_loan_rate)
      
      
      
      
    })
    
    
  })
  
  
  
  
  
  ##### when map is clicked, show popup with info ################
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    
    if (is.null(event))
      return()
    isolate({
      showSchoolPopup(event$id, event$lat, event$lng)
    })
  })
  
  
  
  
  
  
  ##### Exploratory data analysis plot outputs #####################
  
  # color palettes
  colorPal <- colorFactor(c("viridis"), domain = c(1,2,3))
  pal2 <- colorFactor(c("viridis"), domain = majors$subgroup)
  
  # wages plot
  output$wages <- renderPlotly({
    plot_ly(data = wages, x = ~Date, colorscale = "Viridis", height = 650) %>%
      layout(title = "Median Weekly Wages by Education",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Wages ($ USD)", hoverformat = '$.0f', scaleanchor = 'x'),
             legend = list(x = 0.1, y = 0.9)) %>%
      add_trace(y = ~bach_wage, name = "Bachelor's Degree", mode = 'line', hoverinfo = 'y', line = list(color = colorPal(1))) %>%
      add_trace(y = ~hs_wage, name = "Highschool Degree", mode = 'line', hoverinfo = 'y', line = list(color = colorPal(2)))
    
  })
  
  # tuition cpi plot
  output$cpi <- renderPlotly({
    plot_ly(data = cpi, x = ~Date, height = 650) %>%
      layout(title = "Consumer Price Index",
             xaxis = list(title = "Date"),
             yaxis = list(title = "CPI"),
             legend = list(x = 0.1, y = 0.9)) %>%
      add_trace(y = ~total, name = "All Items", mode = 'line', hoverinfo = 'y', line = list(color = colorPal(2))) %>%
      add_trace(y = ~tuition, name = "College Tuition & Fees", mode = 'line', hoverinfo = 'y', line = list(color = colorPal(1)))
  })
  
  # debt plot
  output$loans <- renderPlotly({
    plot_ly(data = debt, x = ~Date, height = 650) %>%
      layout(title = "Student Loan Debt Balance Per Capita",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Debt ($ USD)", hoverformat = "$.0f")) %>%
      add_trace(y = ~debt_per_capita, name = "Student Loan Debt Per Capita", mode =  'line', hoverinfo = 'y', line = list(color = "orange"))
  })
  
  # majors plot
  output$income <- renderPlotly({
    plot_ly(data = drop_na(majors), y = ~income, color = ~subgroup, type = 'box', colors = pal2(majors$subgroup), width = 1400, height = 700) %>%
      layout(title = "Annual Income by Major",
             yaxis = list(title = "Wages ($ USD)", hoverformat = "$.0f"),
             legend = list(x = 100, y = 1))
  })
  
  
  
})
