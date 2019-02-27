outer_rockUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 12,
           box(
             width = NULL, collapsible = TRUE,
             title = "settings", solidHeader = TRUE,
             splitLayout(
               selectUI(ns("rock")),
               selectUI(ns("rock_sm")),
               colUI(ns("rock_col"))
             ),
             tags$head(tags$style(HTML(
               ".shiny-split-layout > div {overflow: visible;}")))
           )
    ), column(width = 12,
              box(width = NULL, collapsible = TRUE, 
                  title = "advanced settings", solidHeader = TRUE,
                  div(style = 'overflow-y:scroll;height:576px;',
                      splitLayout(
                        sliderUI(ns("point_shape")),
                        selectUI(ns("union_select")),
                        sliderUI(ns("rock_phi"))
                      ))
              )
    ), column(width = 12,
              box(width = NULL, collapsible = TRUE, 
                  title = "advanced settings stratification", 
                  solidHeader = TRUE,
                  div(style = 'overflow-y:scroll;height:576px;',
                      splitLayout(
                        selectUI(ns("strat_select")),
                        sliderUI(ns("cellnumber")),
                        numericUI(ns("rotation"))
                      ))
                  
              )
    )
  )
}


outer_rock <- function(input, output, session, df_global3, pvars) {
  
  #first box 
  row_select <- callModule(selectMod,
                           id = "rock",
                           label = df_global3()$nameC,
                           value = rep(FALSE, pvars()),
                           name = "hide stones",
                           pvars = pvars(),
                           sWH = FALSE)
  
  rock_sm <- callModule(selectMod,
                        id = "rock_sm",
                        label = df_global3()$nameC,
                        value = rep(TRUE, pvars()),
                        name = "rounded stones",
                        pvars = pvars(),
                        sWH = FALSE)
  
  
  rock_col <- callModule(colMod,
                         id = "rock_col",
                         label = df_global3()$nameC,
                         value = rep("#CED5D6CA", pvars()),
                         name = "stone color ",
                         pvars = pvars(), sWH = FALSE)
  #second box
  
  point_shape <- callModule(sliderMod,
                            id = "point_shape",
                            label = df_global3()$nameC,
                            value = rep(6, pvars()),
                            min = rep(3, pvars()),
                            max = rep(50, pvars()),
                            pvars = pvars(),
                            name = "Sides number of stones",
                            step = rep(1, pvars()), sWH = FALSE)
  
  select_union <-   callModule(selectMod, id = "union_select", 
                               label = df_global3()$nameC,
                               value = rep(FALSE, pvars()),
                               name = "stones  be united",
                               pvars = pvars(), sWH = FALSE)
  
  slider_phi <- callModule(sliderMod,
                           id = "rock_phi",
                           label = df_global3()$nameC,
                           value = rep(0, pvars()),
                           min = rep(0, pvars()),
                           max = rep(1, pvars()),
                           pvars = pvars(),
                           name = "elliptical shape",
                           step = rep(.1, pvars()), sWH = FALSE)
  #3. box
  
  select_strat <-   callModule(selectMod, id = "strat_select", 
                               label = df_global3()$nameC,
                               value = rep(FALSE, pvars()),
                               name = "shall be stratified",
                               pvars = pvars(), sWH = FALSE)
  
  
  
  cellnumber <- callModule(sliderMod,
                           id = "cellnumber",
                           label = df_global3()$nameC,
                           value = rep(6, pvars()),
                           min = rep(1, pvars()),
                           max = rep(40, pvars()),
                           pvars = pvars(),
                           name = "number of strata",
                           step = rep(1, pvars()), sWH = FALSE)
  
  rotation <- callModule(numericMod,
                         id = "rotation",
                         label = df_global3()$nameC,
                         value = rep(45, pvars()),
                         min = rep(-180, pvars()),
                         max = rep(+180, pvars()),
                         pvars = pvars(),
                         name = "angle of strata",
                         step = rep(1, pvars()), sWH = FALSE)
  
  spoint <- reactive({
    #req(row_select())
    
    if(!any(unlist(row_select())) == TRUE)
      return(NULL)
    
    mat_spoint <- data.frame(
      name = df_global3()$name,
      nSides = as.numeric(point_shape()),
      smooth = rock_sm(),
      union = select_union(),
      phi = slider_phi(),
      strat = select_strat(),
      cellnumber = cellnumber(),
      rotation = rotation()
    )
    set.seed(12)
    skeleton(shape_mod = df_global3(),
             skeleton_mat = mat_spoint[unlist(row_select()),]
    )
    
  })
  
  print_plot3 <- reactive({
    if(is.null(spoint()))
      return(NULL)
    data_rock <- data.frame(fill = unlist(rock_col()),
                            name = df_global3()$name,
                            selct = unlist(row_select())
    ) %>% 
      right_join(spoint(), by = "name") %>% 
      filter(selct == TRUE)
    
    geom_sf(data = data_rock, fill = data_rock$fill) 
  })
  
  
  return(list(print_plot3, spoint))

}