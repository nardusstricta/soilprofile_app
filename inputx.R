# Slider_input:

sliderUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_slider"))
}

sliderMod <- function(input, output, session, label, value, min, max, pvars) {
  
  output$ui_slider <- renderUI({
    if (pvars > 0) {
      fluidRow(
        column(5, h2("SD"),
               lapply(seq(pvars), function(i) {
                 sliderInput(
                   inputId = session$ns(label[i]),
                   label =  label[i],
                   value = value[i],
                   max = max[i],
                   min = min[i]
                 )}
               )
        )
      )
    }
  })
  
  
  reactive({
    shiny::req(input[[paste0(label[1])]])
    sapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })

  })
}
