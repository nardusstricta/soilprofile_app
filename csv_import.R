# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("use_example"), "use example Data")
  )
}

# Module server function
csvFile <- function(input, output, session) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = "please choose a csv.file from your computer or have a look at the example dataset"))
    input$file
  })
  
  # The user's data, parsed into a data frame

    dataframe <- reactive({
      if(input$use_example == FALSE){
        readr::read_csv(userFile()$datapath)
      }else{
        df_global_example
      }
      
    })

  # We can run observers in here if we want to
  # observe({
  #   msg <- sprintf("File %s was uploaded", userFile()$name)
  #   cat(msg, "\n")
  # })
  
  return(dataframe)
}

