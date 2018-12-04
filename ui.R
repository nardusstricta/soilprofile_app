sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Plot", tabName="plot", icon=icon("line-chart"), selected=TRUE),
              menuItem("Table", tabName = "table", icon=icon("table")),
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
                            numericInput("col", "Color", value = 4),
                            checkboxInput("texture", "Texture", TRUE),
                            checkboxInput("root", "Roots", FALSE),
                            checkboxInput("skeleton", "Rocks", TRUE),
                            checkboxInput("raster", "raster", FALSE)
                     )
                   )
  )
)

body <- dashboardBody(
  # tabItems(
  #   tabItem(tabName = "readme",
  #           withMathJax(), 
  #           includeMarkdown("readMe.Rmd")
  #   ),
    tabItem(tabName = "plot",
            fluidRow(
              column(width = 4, 
                     tabBox(width = NULL,
                             tabPanel(h5("transition"),
                                      uiOutput("ui_transition")
                             ),
                             tabPanel(h5("Color"),
                                      uiOutput("ui_color")
                             ),
                            tabPanel(h5("number"),
                                     numericInput("n", "Number", value = 3)
                                     )
                     )),
              column(width = 8,
                     box(width = NULL, plotOutput("plot1", height="500px"), collapsible = TRUE,
                           title = "Plot", status = "primary", solidHeader = TRUE)
              ))
    )
    # tabItem(tabName = "table",
    #         box( width = NULL, status = "primary", solidHeader = TRUE, title="Table",                
    #              downloadButton('downloadTable', 'Download'),
    #              br(),br(),
    #              tableOutput("table")
    #         )
    # ),
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
  #)
)

dashboardPage(
  dashboardHeader(title = "Soilprofile"),
  sidebar,
  body
)