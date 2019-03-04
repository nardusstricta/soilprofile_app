#devtools::install_github("nardusstricta/soilprofile2")
library(sf)
library(dplyr)
library(ggplot2)
library(soilprofile2)
library(shinydashboard)

###
#Moduls####
###

#data import
source("csv_import.R")
source("input_moduls.R")
#layer Inputs
source("rock_modul.R")
source("textur_modul.R")
source("root_modul.R")
source("text_input.R")
source("photo_import.R")
source("process_plot.R")

###
#Bookmarking####
###
#enableBookmarking(store = "server")
enableBookmarking(store = "url")

shape <- soilprofile2::process_symbols

###
#example Profile####
###

df_global_example <-  data.frame(name = c("AhZ", "Bvh", "BvCv"),
                                 depth = c("0-15", "15-43.4", "43.4-70"),
                                 col = c("7.5YR 2/1","10YR 4/3", "2.5Y 5/3"),
                                 skel_dim = c(".1-.8","1-2", "2-3"),
                                 skel_ab = c(0.2, 0.4, .9),
                                 clay = c(1, .8, 0),
                                 silt = c(0, .5, .5),
                                 sand = c(0, .2, .5))

###
#function for dafault settings####
###

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




