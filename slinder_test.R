# Slider_input:

inputxUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_inputx"))
}

inputxMod <- function(input, output, session, label, value, min, max, foo, pvars) {
  
  output$ui_inputx <- renderUI({
    if (pvars > 0) {
      fluidRow(
        column(5, h2(label),
               lapply(seq(pvars), function(i) {
                 foo()
               },
               inputId = session$ns(label[i]),
               label =  label[i],
               value = value[i],
               max = max[i],
               min = min[i]
               
               )
        )
      )
      
    }
  })
  
  
  reac_mat_list <- reactive({
    shiny::req(input[[label[i]]])
    sapply(seq(pvars),
                   function(i) {
                     input[[label[i]]]
                   })
  })
}
