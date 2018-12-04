#install.packages("devtools")
#devtools::install_github("nardusstricta/soilprofile2")
library(tidyr)
library(soilprofile2)
library(readr)
library(lwgeom)
library(shinydashboard)



df_global <- read_csv("braunerde.csv") 

df_global1 <-  data_mod(df_global[1:4,])
df_global2  <- cord_setting(df_global1)
df_global3  <- sf_polgon(database = df_global1, df_polygon = df_global2)


pvars <- nrow(df_global3)

