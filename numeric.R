#numeric Input:
numericUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_numeric"))
}

numericMod <- function(input, output, session, label, value, min, max,
                       name,  pvars, step, sWH = TRUE) {
  
  output$ui_numeric <- renderUI({
    if (pvars > 0) {
      column(5, h5(name),
             lapply(seq(pvars), function(i) {
               numericInput(
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
  
  outputOptions(output, "ui_numeric", suspendWhenHidden = sWH)
  
  reactive({
    shiny::req(input[[paste0(label[pvars])]])
    
    sapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })
    
  })
}
