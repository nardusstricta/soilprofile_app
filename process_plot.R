#numeric Input:
processUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("rm_prozess"), "remove all")
}

processMod <- function(input, output, session, shape, plot_brush, spoint) {
  #Funktion für Modal popup.
  dataModal <- function(failed = FALSE, shape) {
    modalDialog(
      splitLayout(
        selectInput(session$ns("dataset"), "Eigenschaften",
                    c("bitte Auswählen", as.character(shape$name)), 
                    selected = "bitte Auswählen"),
        checkboxInput(session$ns("one1"), "draw one element"),
                         sliderInput(session$ns("number"), "number of symbols", min = 1,
                                     max = 30, value = 5, step = 1),
                         sliderInput(session$ns("size"), "size of symbols", step = .3, min = .3,
                                     max = 30, value = 5)            
      ),
      tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                              overflow: visible;
                              }
                              "))),
      plotOutput(session$ns("plot_mod_1")),
      span("Wähle eine Eigenschaft aus. Bei einem Abstand von 1 wird nur ein Symbol gezeichnet, welche größe
         Abhängig von der markierten Fläche ist"),
      if (failed)
        div(tags$b("Bitte wähle eine Eigenschaft aus", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("ok"), "OK")
      )
    )
  }
  
  ##
  #Interactive Prozesses####
  ##
  
  # Show modal when button is clicked.
  observeEvent(plot_brush(), { #modul action botten:
    showModal(dataModal(shape = shape))
  })
  
  
  vals <- reactiveValues(data = NULL)
  
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    if (!is.null(input$dataset) && input$dataset != "bitte Auswählen"){
      react_layer <- process_layer(polygon = c(plot_brush()$xmin,
                                               plot_brush()$ymin,
                                               plot_brush()$xmax,
                                               plot_brush()$ymax),
                                   number = input$number, 
                                   layer1 = df_geom$data,
                                   layer2 = spoint()
      )
      
      vals$data <- input$dataset
      
      df_geom$data <- rbind(
        df_geom$data,
        st_sf(
          name = id_attr()$name, 
          color = id_attr()$color,
          fill = id_attr()$fill,
          shape = id_attr()$shape, 
          #stroke = id_attr()$stroke,
          size = ifelse(input$one1 == TRUE, 
                        abs(plot_brush()$ymax - plot_brush()$ymin) * 3,
                        input$size),
          if(input$one1 == TRUE | is.null(react_layer)){
            geometry = st_sfc(
              st_point(
                c((plot_brush()$xmax + plot_brush()$xmin)/2, 
                  (plot_brush()$ymax + plot_brush()$ymin)/2
                )
              )
            )
          }else{
            geometry = react_layer
          }
        )
      )
      
      removeModal()
      
      session$resetBrush("plot_brush")
      
    } else {
      showModal(dataModal(failed = TRUE, shape = shape))
    }
  })
  
  onBookmark(function(state) {
    state$values$currentSum <- df_geom$data
  })
  
  onRestore(function(state) {
    df_geom$data <- state$values$currentSum
  })
  
  output$plot_mod_1 <- renderPlot({
    if(input$dataset != "bitte Auswählen"){
      data <- data.frame(
        x <- 1,
        y <- 1
      )
      
      ggplot() +
        geom_point(data = data, aes(x = x, y = y),
                   shape = id_attr()$shape, 
                   color = id_attr()$color,
                   fill = id_attr()$fill,
                   stroke = id_attr()$stroke, 
                   size = 12 * input$size) + 
        theme_void() + theme(legend.position="none")
    }
    
  })
  
  
  df_geom <- reactiveValues(
    data = 
      st_sf(
        name = character(0),
        color = character(0),
        fill = character(0),
        shape = numeric(0),
        size = numeric(0),
        st_sfc()
      )
  )
  
  observeEvent(input$rm_prozess, { 
    df_geom$data <- NULL
    df_geom <<- reactiveValues(
      data = 
        st_sf(
          name = character(0),
          color = character(0),
          fill = character(0),
          shape = numeric(0),
          size = numeric(0),
          st_sfc()
        )
    )
  })
  
  id_attr <- reactive({
    shape[which(shape$name == input$dataset),]
  })
  
  legend_react <- reactive({
    req(df_geom$data$name)
    data.frame(name = unique(df_geom$data$name)) %>% 
      left_join(shape, by = "name")
  })
  
  erg <- reactive({
    geom_sf(data = df_geom$data, 
            shape = df_geom$data$shape,
            size = df_geom$data$size,
            color = df_geom$data$color,
            fill = df_geom$data$fill)
  })
  return(erg)
}
