
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

