# Select_Input

selectUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_select"))
}

selectMod <- function(input, output, session, label, value, pvars, name) {
  
  output$ui_select <- renderUI({
    if (pvars > 0) {
      column(5, h5(name),
             lapply(seq(pvars), function(i) {
               checkboxInput(
                 inputId = session$ns(label[i]),
                 label =  label[i],
                 value =  value[i]
               )}
             )
      )
        
    }
  })
  
  outputOptions(output, "ui_select", suspendWhenHidden = TRUE)
  
  
  reactive({
    # shiny::req(
    #   sapply(seq(pvars),
    #          function(i) {
    #            input[[paste0(label[i])]]
    #          })
    # )
    sapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })
    
  })
}
