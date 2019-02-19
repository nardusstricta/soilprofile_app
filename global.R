
#devtools::install_github("nardusstricta/soilprofile2")
library(tidyverse)
library(sf)
library(smoothr)
library(soilprofile2)
library(shinydashboard)

source("slider.R")
source("select.R")
source("numeric.R")
source("col_input.R")

df_global_example <-  data.frame(name = c("Ah", "Bvh", "BvCv"),
                                 depth = c("0-15", "15-43.4", "43.4-70"),
                                 col = c("7.5YR 2/1","10YR 4/3", "2.5Y 5/3"),
                                 skel_dim = c(".1-.8","1-2", "2-3"),
                                 skel_ab = c(0.2, 0.4, .9),
                                 grain_size = c(3, 2, 1),
                                 grain_sd = c(3, 10, 4)) #%>% 
  #data_mod()

default_yval <- function(polygon){
  do.call(
    rbind,
    lapply(
      1:nrow(polygon), function(i){
        if(i == 1){
          vec <- sf::st_bbox(polygon[i,])
          erg <- (vec[[4]] - vec[[2]]) / 3
          return(round(erg))
        }else{
          vec1 <- sf::st_bbox(polygon[i-1,])
          vec2 <- sf::st_bbox(polygon[i,])
          erg1 <- (vec1[[4]] - vec1[[2]]) / 3
          erg2 <- (vec2[[4]] - vec2[[2]]) / 3
          if(erg1 > erg2){
            return(round(erg1))
          }else{
            return(round(erg2))
          }
        }
        
        
      }
    )
  )
} 

# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label)
  )
}

# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = "please choose a csv.file from your 
                  computer or have a look at the example dataset"))
    input$file
  })
  
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    if(is.null(input$file)){
      df_global_example
    }else{
      readr::read_csv(userFile()$datapath)
    }
    
  })
  

  
  # Return the reactive that yields the data frame
  return(dataframe)
}

