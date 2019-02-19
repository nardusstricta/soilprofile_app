# col_Input

colUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_col"))
}

colMod <- function(input, output, session, label, value, pvars, name) {
  
  output$ui_col <- renderUI({
        column(12, h2(name),
               lapply(seq(pvars), function(i) {
               colourpicker::colourInput(
                 inputId = session$ns(label[i]),
                 label = label[i],
                 value = value[i],
                 allowTransparent = TRUE)
               }
               )
        )
  })
  outputOptions(output, "ui_col", suspendWhenHidden = FALSE)
  
  reactive({
    shiny::req(input[[paste0(label[pvars])]])
    lapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })
    
  })
}
