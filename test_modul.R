library(shiny)
library(dplyr)
library(ggplot2)


innerModUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    uiOutput(ns("inner_slider"))
  )
}

innerMod <- function(input, output, session) {
  output$inner_slider <- renderUI({
    sliderInput(session$ns("slider2"), label = "inner module slider", min = round(min(mtcars$mpg)), 
                max = round(max(mtcars$mpg)), value = c(min(mtcars$mpg), max(mtcars$mpg), step = 1))
  })
  dat <- reactive({
    filter(mtcars, between(mpg, input$slider2[1], input$slider2[2]))
  })
  
  # output$inner_plot <- renderPlot({
  #   req(input$slider2)
  #   data <- filter(mtcars, between(mpg, input$slider2[1], input$slider2[2]))
  #   ggplot(data, aes(mpg, wt)) + geom_point()
  # })
  
}




ui <- fluidPage(
  fluidRow(
    innerModUI("inner"),
    plotOutput("plot2")
  )
)

server <- function(input, output, session) {
  d <- callModule(innerMod, "inner")

  output$plot2 <- renderPlot({
    
    ggplot(d(), aes(mpg, wt)) + geom_point()
  })
  
}

shinyApp(ui = ui, server = server)




transUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_transition"))
}

transMod <- function(input, output, session) {
  
  output$ui_transition <- renderUI({
    if (pvars > 0) {
      fluidRow(column(5, h2("SD"),
                      lapply(seq(pvars), function(i) {
                        sliderInput(
                          inputId = session$ns(paste0("sd", df_global3$nameC[i])),
                          label =  df_global3$nameC[i],
                          value = df_global3$sd[i],
                          max = c(3,3,3,3,3)[i],
                          min = c(0,0,0,0,0)[i]
                        )}
                      )
      ),
      column(
        5, h2("Number"),
        lapply(seq(pvars), function(i) {
          numericInput(inputId = session$ns(paste0("number",
                                                   df_global3$nameC[i])),
                       label = df_global3$nameC[i],
                       value = df_global3$numberX[i])
        })
        
      )
      )
    }
  })
  
  
  reac_mat_list <- reactive({
    shiny::req(input$sdC)
    data.frame(name= seq(pvars),
               numberX = sapply(seq(pvars),
                                function(i) {
                                  input[[paste0("number", df_global3$nameC[i])]]
                                }),
               sd = sapply(seq(pvars),
                           function(i) {
                             input[[paste0("sd", df_global3$nameC[i])]]
                           })
               
               
    )
  })
}

# output$ui_color <- renderUI({
#   if (pvars > 0) {
#     fluidRow(column(5,  h2("Munsell color"),
#                     lapply(seq(pvars), function(i) {
#                       checkboxInput(inputId = paste0("col", 
#                                                           df_global3$nameC[i]), 
#                                          label = df_global3$nameC[i],
#                                          value = c(T,T,T,T)
#                                     )
#                     }
#                     )
#     ),
#     column(5, h2("alpha"),
#            lapply(seq(pvars), function(i) {
#              sliderInput(inputId = paste0("alpha", 
#                                           df_global3$nameC[i]), 
#                          label = df_global3$nameC[i], 
#                          value = 1, 
#                          min = 0, 
#                          max = 1
#                          )
#            }
#            )
#     )
#     )
#   }
#   
# })