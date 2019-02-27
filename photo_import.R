photoUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_photo"))
}
photoMod <- function(input, output, session, pvars, global_df) {
  
  output$ui_photo <- renderUI({
    if (pvars > 0) {
      column(12, h5("Add a Photo"),
             splitLayout(
               fileInput(session$ns("myphoto"), "Choose a file", accept = c('image/png')),
               selectInput(session$ns("horizont_name"), "Horizont name",
                           choices = c("", as.character(global_df()$nameC)), selectize = FALSE), 
               actionButton(session$ns("save_photo"), "add Photo to plot")
             ),
             actionButton("rmphoto", "remove Photo")
      )
    }
  })
  
  userPhoto <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$myphoto, message =NULL))
    input$myphoto
  })
  
  
  raster_list <- reactiveValues(data = NULL)#raster::brick()
  
  observeEvent(input$save_photo,{
    if(input$horizont_name == "" | is.null(userPhoto()$datapath))
      showNotification("You have to select a file and select a horizont name")
    
    shiny::req(input$horizont_name, userPhoto()$datapath)
    
    test <- png_import(userPhoto()$datapath, 
                       global_df()[which(global_df()$nameC == as.character(input$horizont_name)),],
                                    raster2polygon = F)
    raster_list$data <- ggspatial::layer_spatial(test)
    #print(test)
    #raster_list$data <- list(raster_list$data, test)
  })
  observeEvent(input$rmphoto, {
    raster_list$data <<- NULL 
  })
  
  erg_photo <- reactive({
    if(is.null(raster_list$data))
      return(NULL)
    raster_list$data
    
  })
  return(erg_photo)
}