## ui.R code ##
library(leaflet)
library(plotly)
library(shinyjs)


# create shiny UI in navbarPage format
shinyUI(
  navbarPage("Four-Year Colleges", id="nav",
             
             ## college explorer map tab panel ##
             tabPanel("Interactive map",
                      
                      div(
                        class="outer",
                        tags$head(
                          # Include custom CSS
                          
                          includeCSS("www/styles.css"),
                          includeScript("www/gomap.js")
                        ),
                        
                        # basic leafletMap output (set view and zoom in server)
                        leafletOutput(outputId = "map", width="100%", height="100%"),
                        
                        ## create selection panel
                        absolutePanel(
                          id = "controls",
                          class = "panel panel-default",
                          fixed = TRUE, draggable = FALSE,
                          top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 330, height = "auto", style = "overflow-y:scroll; max-height: 600px",
                          
                          # title of selection panel
                          h2("Explore U.S. Colleges"),
                          
                          
                          
                          ### now for the inputs ###
                          
                          # package to show/hide filter menues
                          useShinyjs(),
                          
                          ## public vs. private checkbox input
                          checkboxGroupInput(inputId = "owner", label = "Type", 
                                             choiceNames =  list("Public", "Private non-profit", "Private for-profit"),
                                             #myDict %>% filter(variable_name == 'ownership') %>% select(label),
                                             choiceValues = list(1, 2, 3),
                                             #myDict %>% filter(variable_name == 'ownership') %>% select(value),
                                             selected = list(1, 2, 3)),
                          
                          ## school size slider
                          actionButton(inputId = "show_size", label = "Filter on Undergraduate Enrollment"),
                          tags$br(),
                          sliderInput(inputId = "size", label = "Size", min = 0, max = 70000, value = c(0, 70000), step = 500, ticks = FALSE, dragRange = FALSE),
                          
                          ## fees and debt slider
                          tags$br(),
                          actionButton(inputId = "show_cost", label = "Filter on Cost & Outcomes"),
                          tags$br(),
                          
                          hidden(
                            sliderInput(inputId = "tuition", label = "Average Out-Of-State Tuition & Fees", min = 0, max = 70000, value = 70000, step = 1000, ticks = FALSE, pre = "$"),
                            checkboxInput(inputId = "na_tuition", label = "Inlcude NA?", value = TRUE),
                            
                            sliderInput(inputId = "debt", label = "Median Debt Upon Completion", min = 0, max = 70000, value = 70000, step = 1000, ticks = FALSE, pre = "$"),
                            checkboxInput(inputId = "na_debt", label = "Inlcude NA?", value = TRUE),
                            
                            sliderInput(inputId = "earnings", label = "Median Earnings 6 years After Entry", min = 0, max = 120000, value = c(0, 120000), step = 1000, ticks = FALSE, dragRange = FALSE, pre = "$"),
                            checkboxInput(inputId = "na_earnings", label = "Inlcude NA?", value = TRUE)
                          ),
                          
                          
                          
                          ## campus setting checkbox
                          tags$br(),
                          actionButton(inputId = "show_locale", label = "Filter on Campus Setting"),
                          tags$br(),
                          
                          hidden(
                            checkboxGroupInput(inputId = "locale", label = NULL,
                                               choices = c("Large City (pop > 250,000)" = 11, "Midsize City (pop 100,000-250,000)" = 12, "Small City (pop < 100,000)" = 13, 
                                                           "Large Suburb (pop > 250,000)" = 21, "Midsize Suburb (pop 100,000-250,000)" = 22, "Small Suburb ( pop < 100,000)" = 23,
                                                           "Fringe Town (up to 10 miles from urbanized area)" = 31,
                                                           "Distant Town (between 10-35 miles form urbanized area" = 32,
                                                           "Remote Town (more than 35 miles from urbanized area" = 33),
                                               selected = list(11, 12, 13, 21, 22, 23, 31, 32, 33))),
                          
                          ## filter on admissions
                          tags$br(),
                          actionButton(inputId = "show_admin", label = "Filter on Admissions Selectivity"),
                          tags$br(),
                          
                          
                          hidden(
                            # admissions
                            sliderInput(inputId = "intake", label = "Admission Rate", min = 0, max =  100, value = c(0, 100), ticks = FALSE, dragRange = FALSE, step = 0.02, post = "%"),
                            checkboxInput(inputId = "na_intake", label = "Inlcude NA?", value = TRUE),
                            
                            # ACT scores
                            sliderInput(inputId = "act", label = "ACT Scores (25th Percentile)", min = 1, max =  36, value = c(1, 36), ticks = FALSE, dragRange = FALSE),
                            checkboxInput(inputId = "na_act", label = "Inlcude NA?", value = TRUE),
                            
                            # SAT scores
                            sliderInput(inputId = "sat_verbal", label = "SAT Verbal Scores (25th Percentile)", min = 200, max = 800, value = c(200, 800), ticks = FALSE, dragRange = FALSE),
                            sliderInput(inputId = "sat_math", label = "SAT Math Scores (25th Percentile)", min = 200, max = 800, value = c(200, 800), ticks = FALSE, dragRange = FALSE),
                            checkboxInput(inputId = "na_sat", label = "Inlcude NA?", value = TRUE)),
                          
                          ## filter on special interests
                          # br(),
                          # actionButton(inputId = "show_special", label = "Filter on Special Interests"),
                          # br(),
                          # 
                          # hidden(
                          #   selectInput(inputId = "religion", label = "Religious Affiliation",
                          #               choices = c("Evangelical = "))
                          # ),
                          
                          # filter on major
                          tags$br(),
                          actionButton(inputId = "show_major", label = "Filter by Major"),
                          tags$br(),
                          
                          hidden(
                            checkboxGroupInput(inputId = "major", label = NULL, 
                                               choices = c("Agriculture" = 1,
                                                           "Architecture" = 2,
                                                           "Biological and Biomedical Sciences" = 3,
                                                           "Business, Management, Marketing, and Related Support Services" = 4,
                                                           "Communications and Journalism" = 5,
                                                           "Computer Science" = 6,
                                                           "Culinary Services" = 7,
                                                           "Education" = 8,
                                                           "Engineering" = 9,
                                                           "English Language and/or Literature" = 10,
                                                           "Ethnic, Cultural and Gender Studies" = 11,
                                                           "Foregin Languages, Literatures, and Linguistics" = 12,
                                                           "Health Professions and Related Programs" = 13,
                                                           "History" = 14,
                                                           "Legal Studies" = 15,
                                                           "Liberal Arts & Sciences, General Studies, and Humanities" = 16,
                                                           "Mathematics and Statistics" = 17,
                                                           "Military Technologies and Applied Sciences" = 18,
                                                           "Multi/Interdisciplinary Studies" = 19,
                                                           "Philosophy and Religious Studies" = 20,
                                                           "Physical Sciences" = 21,
                                                           "Psychology" = 22,
                                                           "Public Administration and Social Service Professions" = 23,
                                                           "Science Technologies/Technicians" = 24,
                                                           "Social Sciences" = 25,
                                                           "Theology and Religious Vocations" = 26,
                                                           "Visual and Performing Arts" = 27
                                                           )
                                               )
                            )
                          
                          
                        ),
                        
                        # cite
                        tags$div(id = "cite",
                                 "Map render based on rstudio's 063-superzip-example")
                      )
             ),
             
             
             
             
             ## datatable tab panel ##
             tabPanel("Explore Data",
                      fluidRow(
                        column(3,
                               selectInput("states", "State", c("All States" = "", structure(state.abb, names = state.name), "Washington, DC" = "DC"), multiple = TRUE),
                               
                               
                               column(12,
                                      conditionalPanel("input.states",
                                                       selectInput("cities", "Cities", c("All Cities" = ""), multiple = TRUE)))
                        ),
                        
                        hr(),
                        DT::dataTableOutput("data")
                      )
                      
             ),
             
             
             ## summary tab panel ##
             navbarMenu("More",
                        tabPanel("Cost & Outcomes",
                                 
                                 div(
                                   class="outer",
                                   tags$head(
                                     # Include custom CSS
                                     includeCSS("www/styles.css")
                                     
                                   ),
                                   
                                   fluidPage(
                                     fluidRow(
                                       column(4,
                                              h3("The college wage premium"),
                                              br(),
                                              "source: Federal Reserve Economic Data"),
                                       
                                       column(4,
                                              h3("The rising cost of tuition"),
                                              br(),
                                              "source: Bureau of Labor Statistics"),
                                       
                                       column(4,
                                              h3("The rising student loan debt burden"),
                                              br(),
                                              "source: Federal Reserve Bank of New York")),
                                     
                                     fluidRow(
                                       column(4,
                                              plotlyOutput("wages")),
                                       column(4,
                                              plotlyOutput("cpi")),
                                       column(4,
                                              plotlyOutput("loans")))
                                   ))),
                        
                        tabPanel("Which Major?",
                                 fluidPage(
                                   fluidRow(
                                     column(12,
                                            h3("Average Salary by Major"),
                                            plotlyOutput("income")))
                                 ))
                        
             )
             
  ))


