## ui.R code ##
library(leaflet)


# create shiny UI in navbarPage format
shinyUI(
  navbarPage("Four-Year Colleges", id="nav",
             tabPanel("Interactive map",
                      div(class="outer",
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                            ),
                          
                          # basic leafletMap output (set view and zoom in server)
                          leafletOutput(outputId = "map", width="100%", height="100%"),
                          
                          ## create selection panel
                          absolutePanel(
                            id = "controls",
                            class = "panel panel-default",
                            fixed = TRUE, draggable = TRUE,
                            top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                            
                            # title of selection panel
                            h2("College Explorer"),
                            
                            
                            
                            ### now for the inputs ###
                            
                            # public vs. private checkbox input
                            checkboxGroupInput(inputId = "owner", label = "Type", 
                                               choiceNames =  list("Public", "Private non-profit", "Private for-profit"),
                                               #myDict %>% filter(variable_name == 'ownership') %>% select(label),
                                               choiceValues = list(1, 2, 3),
                                               #myDict %>% filter(variable_name == 'ownership') %>% select(value),
                                               selected = "All")
                            ),
                          # cite
                          tags$div(id = "cite",
                                         "Map render based on rstudio's 063-superzip-example")
                          )
                      ),
             
             ## datatable tab panel ##
             tabPanel("Data Explorer",
                      "datatable will go here"
                      ),
             
             ## stat analysis tab panel ##
             tabPanel("Is College Worth It?",
                      "stat analysis will go here"
                      )
             )
  )

# 
# shinyUI(
#   navbarPage("Four-Year Colleges", id="nav",
#              tabPanel("Interactive map",
#                       div(class="outer",
#                           tags$head(
#                             # Include our custom CSS
#                             includeCSS("styles.css"),
#                             includeScript("gomap.js")
#                           ),
#                           
#                           # leafletMap --> pre-made tileset from jcheng (superzip)
#                           leafletOutput(outputId = "map", width="100%", height="100%",
#                                         initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#                                         initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
#                                         options=list(
#                                           center = c(37.45, -93.85),
#                                           zoom = 5,
#                                           maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)) # Show US only
#                                         )
#                           ),
#                           
#                           ## create selection panel
#                           absolutePanel(
#                             id = "controls",
#                             class = "panel panel-default",
#                             fixed = TRUE, draggable = TRUE,
#                             top = 60, left = "auto", right = 20, bottom = "auto",
#                             width = 330, height = "auto",
#                             
#                             # title of selection panel
#                             h2("College Explorer"),
#                             
#                             
#                             
#                             ### now for the inputs ###
#                             
#                             # public vs. private checkbox input
#                             checkboxGroupInput(inputId = "owner", label = "Type", 
#                                                choiceNames =  list("Public", "Private non-profit", "Private for-profit"),
#                                                #myDict %>% filter(variable_name == 'ownership') %>% select(label),
#                                                choiceValues = list(1, 2, 3),
#                                                #myDict %>% filter(variable_name == 'ownership') %>% select(value),
#                                                selected = 1)
#                           ),
#                           # cite
#                           tags$div(id = "cite",
#                                    "Map render based on rstudio's 063-superzip-example")
#                       )
#              ),
#              
#              ## datatable tab panel ##
#              tabPanel("Data Explorer",
#                       "datatable will go here"
#              ),
#              
#              ## stat analysis tab panel ##
#              tabPanel("Is College Worth It?",
#                       "stat analysis will go here"
#              )
#   )
# )





# shinyUI(dashboardPage(
#   dashboardHeader(
#     title = "Maximum Value for College in the U.S."),
#   
#   dashboardSidebar(
#     
#     # user panel
#     sidebarUserPanel("Is college worth it?"),
#     
#     # sidebar menu
#     sidebarMenu(
#       menuItem("Analysis", tabName = "data", icon = icon("database")),
#       menuItem("Find a School", tabName = "map", icon = icon("map")))
#   
#     ),
#   
#   dashboardBody(
#     
#     
#     fluidRow(
#   
#       # first column with map and datatable
#       column(width = 9,
#              
#              # to display the cluster map
#              box(width = NULL, solidHeader = TRUE,
#                  leafletOutput("mymap")
#                  ),
#              
#              # to display a single school's stats
#              box(width = NULL, status = "warning",
#                  dataTableOutput("schoolData")
#              )
#       ),
#       
#       # second column with map selections
#       column(width = 3,
#              
#              # box for checkboxes, sliders, etc
#              box(width = NULL, status = "warning",
#                  
#                  # public vs. private school
#                  checkboxGroupInput("ownership", label = h2("Type"),
#                                     choices = list(myDict %>% filter(variable_name == ownership) %>% select(label)),
#                                     selected = NULL)
#                  
#                  
#                  
#              )
#         
#       )
#       
#     )
#             
#     
#     
#   )
#   
# ))