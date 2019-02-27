outer_rootUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      box(
        width = NULL, collapsible = TRUE,
        title = "settings", solidHeader = TRUE,
        splitLayout(h5("Global settings:"),
                    numericInput(ns("variation"), "length variation",
                                 value = .4, max = 2, min = .1, step = .1),
                    numericInput(ns("wide"), "wide variation",
                                 value = 1, max = 2, min = .1, step = .1),
                    colourpicker::colourInput(ns("rc11"), "root color",
                                              value = "#8B5A00",
                                              allowTransparent = TRUE)
        ),
        hr(),
        splitLayout(
          selectUI(ns("root_select")),   
          numericUI(ns("number_root"))
        )
      ),
      box(
        width = NULL, collapsible = TRUE,
        title = "advance settings", solidHeader = TRUE,
        splitLayout(sliderUI(ns("length_root")),
                    sliderUI(ns("root_size")),
                    colUI(ns("roots_col"))
        )
      )
    )
  )
}

outer_root <- function(input, output, session, df_global3, pvars) {
  select_root <-   callModule(selectMod, id = "root_select", 
                              label = df_global3()$nameC,
                              value = rep(FALSE, pvars()),
                              name = "add roots",
                              pvars = pvars())
  
  root_number <- callModule(numericMod,
                            id = "number_root",
                            label = df_global3()$nameC,
                            value = rep(500, pvars()),
                            min = rep(30, pvars()),
                            max = rep(1200, pvars()),
                            pvars = pvars(),
                            name = "number of Points",
                            step = rep(1, pvars()))
  
  root_length <- callModule(sliderMod,
                            id = "length_root",
                            label = df_global3()$nameC,
                            value = rep(.5, pvars()),
                            min = rep(.1, pvars()),
                            max = rep(5, pvars()),
                            pvars = pvars(),
                            name = "length of roots",
                            step = rep(.1, pvars()))
  
  root_size <- callModule(sliderMod,
                          id = "root_size",
                          label = df_global3()$nameC,
                          value = rep(1, pvars()),
                          min = rep(.1, pvars()),
                          max = rep(4, pvars()),
                          pvars = pvars(),
                          name = "size of roots",
                          step = rep(.1, pvars()))
  
  root <- reactive({
    shiny::req(input$variation)
    if(!any(unlist(select_root())) == TRUE)
      return(NULL)
    
    row_select_root <- unlist(select_root())
    
    mutiple_roots(polygon = df_global3()[row_select_root,], 
                  number = root_number()[row_select_root], 
                  line_length = root_length()[row_select_root],
                  horizont_id = df_global3()$nameC[row_select_root],
                  variation = input$variation,
                  par_attr = par_df)
  })
  
  
  print_plot4 <- reactive({
    shiny::req(input$rc11)
    if(is.null(root()))
      return(NULL)
    
    par_df <- data.frame(rs11 = root_size(),
                         horizont_id = df_global3()$nameC)
    
    root_data <- root() %>% 
      left_join(par_df, by = "horizont_id") %>% 
      group_by(horizont_id) %>% 
      mutate(size = abs(rnorm(n(),  rs11,  input$wide)))
    
    geom_sf(data = root_data, 
            color = input$rc11,
            size = root_data$size,
            lineend = "round"
    )
  })
  
  return(print_plot4)
}