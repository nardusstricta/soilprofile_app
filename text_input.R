###
#structure Layer####
###
textUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_text"))
}

textMod <- function(input, output, session, label, 
                    name, pvars, choices, global_df) {
  
  mask_poly <- function(import_poly, horizont){
    bb_ho <- st_bbox(horizont)
    shift_ho <- c(bb_ho["xmax"], bb_ho["ymax"] - bb_ho["ymin"])
    expan_poly <- import_poly$geometry * max(shift_ho)
    shift_y <- (expan_poly + c(0, bb_ho["ymax"])) %>%
      sf::st_intersection(sf::st_buffer(horizont, -.5)) %>% 
      sf::st_sf(fill = horizont$fill)
    return(shift_y)
  }
  
  output$ui_text <- renderUI({
    if (pvars > 0) {
      column(12, h5("Add a new layer"),
             splitLayout(
               fileInput(session$ns("mypng"), "Choose a file", accept = c('image/png')),
               textInput(session$ns("stru_name"), "Structure name", value = "", width = NULL, 
                                    placeholder = "Aggregatgefüge"), 
               actionButton(session$ns("savepng"), "upload file")
             ),
             hr(),
             h5(name),
             splitLayout(
               lapply(seq(pvars), function(i) {
                 selectInput(
                   inputId = session$ns(label[i]),
                   label =  label[i],
                   choices = c("", vals$data$name),
                   selected = ""
                 )}
               ),
               lapply(seq(pvars), function(i) {
                 colourpicker::colourInput(
                   inputId = session$ns(paste0("col_",label[i])),
                   label = label[i],
                   value = "black",
                   allowTransparent = TRUE)
               })
             )
      )
    }
  })
  
  
  struc_par <- reactive({
    sapply(seq(pvars),
           function(i) {
             input[[paste0(label[i])]]
           })
  })
  
  struc_col <-   reactive({
    lapply(seq(pvars),
           function(i) {
             input[[paste0("col_",label[i])]]
           })
  })
  
  
  userPng <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$mypng, message =NULL))
    input$mypng
  })
  
  
  vals <- reactiveValues(data = struc_poly[,-1])
  
  observeEvent(input$savepng,{
    if(is.null(input$stru_name)| is.null(userPng()$datapath)){
      showNotification("You have to select a file and enter a name")
    }else{
      
      withProgress(message = 'Structure is created',
                   detail = 'please wait a moment.', value = 0,{
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.25)
                     }
                     polygon1 <- sf::st_polygon(list(rbind(c(0,0),
                                                           c(1,0), 
                                                           c(1,-1),
                                                           c(0,-1),
                                                           c(0,0)))) %>% 
                       sf::st_sfc() %>% 
                       sf::st_sf()  
                     library(rgeos)
                     erg34 <-  png_import(userPng()$datapath, polygon1) %>% 
                       mutate(name = input$stru_name) %>% 
                       select(-layer)
                     
                     vals$data <-  rbind(
                       vals$data,  erg34
                     )
                     
                   })
    }
  })
  ###
  #Bookmarking for server applications
  ###
  
  # onBookmark(function(state) {
  #   state$values$current_stru <- vals$data
  # })
  # 
  # onRestore(function(state) {
  #   vals$data <- state$values$current_stru
  # })

  erg0 <- reactive({
    if(all(unlist(struc_par())== ""))
      return(NULL)
    temp <-  data.frame(
      name = as.factor(unlist(struc_par())),
      horizont_id = as.factor(label),
      fill = as.factor(unlist(struc_col()))
    ) %>% 
      filter(name != "") %>% 
      left_join(vals$data, by = "name") %>% 
      sf::st_sf()

    
    temp_geom <- temp
    st_geometry(temp) <- NULL
    
  erg1 <- global_df() %>% 
    select(nameC, geometry) %>% 
    mutate(horizont_id = nameC) %>% 
    right_join(temp, by = "horizont_id")
  
  erg2 <- do.call(rbind, lapply(1:nrow(temp), function(i) { 
    mask_poly(temp_geom[i,], erg1[i,])
    })) 
  
  erg3 <- geom_sf(data = erg2, fill = erg2$fill)
  
  return(erg3)
  })

}