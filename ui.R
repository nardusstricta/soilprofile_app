sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Plot", tabName="plot", icon=icon("line-chart")),
              menuItem("Table import", tabName = "table", icon=icon("table"), selected=TRUE),
              menuItem("Codes",  icon = icon("file-text-o"),
                       menuSubItem("Mlxtran", tabName = "pkmodel", icon = icon("angle-right")),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
              ),
              menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
              menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr(),
  conditionalPanel("input.tabs == 'plot'",
                   fluidRow(
                     column(1),
                     column(10,
                            numericInput("buffer", "Buffer", value = -1, step =.1),
                            checkboxInput("texture", "Texture", TRUE),
                            checkboxInput("root", "Roots", FALSE),
                            checkboxInput("skeleton", "Rocks", TRUE),
                            checkboxInput("raster", "raster", FALSE)
                     )
                   )
  )
  
)

body <- dashboardBody(
   tabItems(
  #   tabItem(tabName = "readme",
  #           withMathJax(), 
  #           includeMarkdown("readMe.Rmd")
  #   ),
    tabItem(tabName = "plot",
            fluidRow(
              column(width = 6, 
                     tabBox(width = NULL,
                            tabPanel(h5("Color"),
                                     fluidRow(
                                       sliderUI("alpha_bg")
                                     )
                            ),
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
                                        title = "extention", solidHeader = TRUE,
                                        div(style = 'overflow-y:scroll;height:576px;',
                                        splitLayout(selectUI("sel_smoth"),
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
                            tabPanel(h5("Pattern"),
                                     fluidRow(
                                       selectUI("pattern")
                                     )
                                     
                            )
                            # tabPanel(
                            #   h5("Rock"),
                            #   fluidRow(
                            #     column(width = 12,
                            #            box(width = NULL, collapsible = TRUE,
                            #                title = "settings", solidHeader = TRUE,
                            #                selectUI("rock"),
                            #                colUI("rock_col"))
                            #     ), column(width = 12,
                            #               box(width = NULL, collapsible = TRUE,
                            #                   title = "extention", solidHeader = TRUE,
                            #                   sliderUI("point_shape"))
                            #     ), column(width = 12,
                            #               box(width = NULL, collapsible = TRUE,
                            #                   title = "extention", solidHeader = TRUE,      
                            #                   sliderUI("cellnumber"),
                            #                   sliderUI("rotation")
                            #               )
                            #     )
                            #   )
                            # )
                     )
              ),
              column(
                width = 6,
                box(width = NULL, plotOutput("plot1", height="650px"), collapsible = TRUE,
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
    )
    # tabItem(tabName = "pkmodel",
    #         box(width = NULL, status = "primary", solidHeader = TRUE, title="Edit",
    #             editModUI("eview", width="100%", height="400px")
    #   
    #         )
    #)
    # tabItem(tabName = "ui",
    #         box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
    #              downloadButton('downloadData2', 'Download'),
    #              br(),br(),
    #              pre(includeText("ui.R"))
    #         )
    # ),
    # tabItem(tabName = "server",
    #         box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
    #              downloadButton('downloadData3', 'Download'),
    #              br(),br(),
    #              pre(includeText("server.R"))
    #         )
    # ),
    # tabItem(tabName = "about",
    #         includeMarkdown("../../about/about.Rmd")
    # )
  )
)

dashboardPage(
  dashboardHeader(title = "Soilprofile"),
  sidebar,
  body
)