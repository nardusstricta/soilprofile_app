outer_textureUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      splitLayout(h5("Global settings:"),
                  sliderInput(ns("buffer"), "Buffer", value = -1,
                              step =.1, min = -2, max = 0),
                  checkboxInput(ns("texture_bg"), "add background", TRUE)
      ),
      splitLayout(
        colourpicker::colourInput(ns("outline_col"), "outline color",
                                  value = "grey", palette = "limited",
                                  returnName = TRUE),
        sliderInput(ns("dist_grain"), "Distance between texture points",
                    value = 1, min = 0.1, max = 2, step = .1),
        sliderInput(ns("size_grain"), "Difference of point size", 
                    value = 1, min = .3, max = 4, step = .3)
      ),
      hr(),
      h5("Horizont settings"),
      selectUI(ns("pattern"))
    )
  )
}


outer_textur <- function(input, output, session, df_global3, pvars) {
  
  row_select_pat <- callModule(selectMod,
                               id = "pattern",
                               label = df_global3()$nameC,
                               value = rep(FALSE, pvars()),
                               name = "Texture",
                               pvars = pvars())
  
  texture_layer <- reactive({
    shiny::req(input$buffer)
    if(!any(unlist(row_select_pat()))== TRUE)
      return(NULL)
    
    set.seed(12)
    
    row_select_pat <- unlist(row_select_pat())
    
    texture_sf <- apply_texture(shape = df_global3()[row_select_pat,],
                                buffer = input$buffer, background = input$texture_bg,
                                dist_size = input$dist_grain
    ) 
    
    texture_par <- par_default(texture_sf, 
                               outline_col = input$outline_col, 
                               size_grain = input$size_grain)
    return(texture_par)
    
  })
  
  print_plot2 <- reactive({
    if(is.null(texture_layer()))
      return(NULL)
    geom_sf(data = texture_layer(),
            fill = texture_layer()$bgc,
            col = texture_layer()$col,
            shape = texture_layer()$pch,
            linetype = texture_layer()$linetype, 
            size = texture_layer()$size)
  })
  
  return(print_plot2)
  
}