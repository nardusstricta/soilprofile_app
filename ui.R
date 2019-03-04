function(request) {
  sidebar <- dashboardSidebar(
    hr(),
    sidebarMenu(id="tabs",
                menuItem("Plot", tabName="plot", icon=icon("line-chart")),
                menuItem("Table import", tabName = "table", icon=icon("table"), selected=TRUE),
                menuItem("Codes",  icon = icon("file-text-o"),
                         menuSubItem("global.R", tabName = "global", icon = icon("angle-right")),
                         menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                         menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
                         menuSubItem("structure", tabName = "structure", icon = icon("angle-right"))
                ),
                menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
                menuItem("About", tabName = "about", icon = icon("question"))
    ),
    hr(),
    conditionalPanel("input.tabs == 'plot'",
                     fluidRow(
                       column(1),
                       column(10,
                              h4("Other goodies"),
                              hr(),
                              sliderInput("plot_width", "Plot width", value = 3, min = 1, max= 5, step = .5),
                              h5("save current state"),
                              bookmarkButton(),
                              hr(),
                              h5("Download Plot"),
                              downloadButton('download1', 'Download')
                       )
                     )
    )
    
  )
  
  body <- dashboardBody(
    tabItems(
        tabItem(tabName = "readme",
                fluidPage(
                  tags$iframe(src = './readme.html', 
                              width = '100%', height = '800px', 
                              frameborder = 0, scrolling = 'auto'
                  )
                )
        ),
      tabItem(tabName = "plot",
              fluidRow(
                column(width = 6, 
                       tabBox(width = NULL,
                              tabPanel(
                                h5("Transition"),
                                fluidRow(
                                  column(
                                    width = 12,
                                    box(
                                      width = NULL, collapsible = TRUE,
                                      title = "settings", solidHeader = TRUE,
                                      splitLayout(sliderUI("sd"), 
                                                  sliderUI("numberX"), 
                                                  selectUI("sm")
                                      )
                                      
                                    )), column(
                                      width = 12,
                                      box(width = NULL, collapsible = TRUE,
                                          collapsed = TRUE,
                                          title = "advanced settings", solidHeader = TRUE,
                                          div(style = 'overflow-y:scroll;height:576px;',
                                              splitLayout(helpText("you have to choose", br(), 
                                                                   "a horizon if you want to", br(),
                                                                   "change something"),
                                                          selectUI("sel_smoth"),
                                                          numericInput("shape", 
                                                                       "shape", .1, 
                                                                       min = 0,
                                                                       max = 40)
                                              ),
                                              hr(),
                                              splitLayout(sliderUI("sm_buf_si"),
                                                          numericUI("sm_buffer_number")),
                                              hr(),
                                              splitLayout(
                                                sliderUI("sm_nSide"), 
                                                numericUI("sm_rate")
                                              )
                                          )
                                          
                                      )
                                      
                                    )
                                )
                              ),
                              tabPanel(h5("Color"),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           sliderUI("alpha_bg")
                                         )
                                       )
                              ),
                              tabPanel(h5("Texture"),
                                       outer_textureUI("texture")
                              ),
                              tabPanel(
                                h5("Rock"),
                                outer_rockUI("rock")
                              ),
                              tabPanel(h5("Process Layer"),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           h4("Global settings:"),
                                           helpText("In order to represent soil-forming processes, such as hydromorphic characteristics, a square in the plot must be marked using the mouse. In this area the symbols will be drawn. This can be repeated at will to select different symbols. However, if there is no more space, no symbol will be drawn.  Unless the box 'only one symbol' is clicked. Now in any case the symbol is drawn and the size is appended to the marked area and can no longer be adjusted with the slider. Try it!"),
                                           processUI("process"),
                                           photoUI("photo")
                                         )
                                       )
                              ),
                              tabPanel(h5("Roots"),
                                       outer_rootUI("root")
                              ),
                              tabPanel(h5("Structure"),
                                       fluidRow(
                                         column(
                                           width = 12,
                                           textUI("stru_soil")
                                         )
                                       )
                              )
                       )
                ),
                column(
                  width = 6,
                  box(width = NULL, plotOutput("plot1", height="650px", brush = "plot_brush"), 
                      title = "Plot", solidHeader = TRUE, status = "primary")
                  
                )
              )
      ),
      tabItem(tabName = "table",
              box(width = NULL, status = "primary", solidHeader = TRUE, title="Table",
                  csvFileInput("datafile", "User data (.csv format)"),
                  br(),
                  tableOutput("table1")
              )
      ),
      tabItem(tabName = "global",
              box(width = NULL, status = "primary", solidHeader = TRUE, title= "global.R",
                  downloadButton('downloadData1', 'Download'),
                  br(),br(),
                  pre(includeText("global.R"))

              )
      ),
      tabItem(tabName = "ui",
              box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                   downloadButton('downloadData2', 'Download'),
                   br(),br(),
                   pre(includeText("ui.R"))
              )
      ),
      tabItem(tabName = "server",
              box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                   downloadButton('downloadData3', 'Download'),
                   br(),br(),
                   pre(includeText("server.R"))
              )
      ),
      tabItem(tabName = "structure",
              box( width = NULL, status = "primary", solidHeader = TRUE, title= "Reactive Log Visualizer",
                helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                tags$iframe(src = './reactlog_mini.html', 
                            width = '100%', height = '800px',
                            frameborder = 0, scrolling = 'auto'
                )
              )

      ),
      tabItem(tabName = "about",
              fluidPage(
                   tags$iframe(src = './about.html', 
                               width = '100%', height = '800px',
                               frameborder = 0, scrolling = 'auto'
                   )
              )
              
      )
      
    )
  )
  
  dashboardPage(
    dashboardHeader(title = "Soilprofile"),
    sidebar,
    body
  )
  
  
}
  
