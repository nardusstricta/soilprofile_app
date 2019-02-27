# Slider input:####

sliderUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_slider"))
}

sliderMod <- function(input, output, session, label, value, 
                      min, max, name, step,  pvars, sWH = TRUE) {
  
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
  
  outputOptions(output, "ui_slider", suspendWhenHidden = sWH)
  reactive({
    #shiny::req(input[[paste0(label[pvars])]])
    lapply(seq(pvars), function(i) {req(input[[paste0(label[i])]])})
    sapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })

  })
}

# Select input####

selectUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_select"))
}

selectMod <- function(input, output, session, label,
                      value, pvars, name, sWH = TRUE) {
  
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
  
  outputOptions(output, "ui_select", suspendWhenHidden = sWH)
  
  
  reactive({
    #req(is.null(input[[paste0(label[pvars])]]))
    if (any(unlist(lapply(seq(pvars), function(i) {
      is.null(input[[paste0(label[i])]])})))){
      return(NULL) 
    # }else{
    #   validate(
    #   do.call(need, lapply(seq(pvars),
    #            function(i) {
    #              input[[paste0(label[i])]]
    #            })
    #   ), message = FALSE
    #   )
         #req(input[[paste0(label[pvars])]])
       }
    
    sapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })
    
  })
}

# Color input####

colUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_col"))
}

colMod <- function(input, output, session, label, value, pvars, name, sWH = TRUE) {
  
  output$ui_col <- renderUI({
    column(12, h5(name),
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
  outputOptions(output, "ui_col", suspendWhenHidden = sWH)
  
  reactive({
    lapply(seq(pvars), function(i) {req(input[[paste0(label[i])]])})
    #shiny::req(input[[paste0(label[pvars])]])
    lapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })
  })
}

#Numeric input:####
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
    lapply(seq(pvars), function(i) {req(input[[paste0(label[i])]])})
    #shiny::req(input[[paste0(label[pvars])]])
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
