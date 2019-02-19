# Slider_input:

sliderUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_slider"))
}

sliderMod <- function(input, output, session, label, value, min, max, name, step,  pvars) {
  
  output$ui_slider <- renderUI({
    if (pvars > 0) {
        column(12, h5(name),
               lapply(seq(pvars), function(i) {
                 sliderInput(
                   inputId = session$ns(label[i]),
                   label =  label[i],
                   value = value[i],
                   max = max[i],
                   min = min[i],
                   step = step[i]
                 )}
               )
        )
    }
  })
  
  outputOptions(output, "ui_slider", suspendWhenHidden = TRUE)
  reactive({
    shiny::req(input[[paste0(label[pvars])]])
    sapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })

  })
}
