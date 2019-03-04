library(shiny)
library(profvis)
profvis({
  runApp(display.mode = "normal")
})


#Creat same example data:
library(dplyr)
library(soilprofile2)
library(ggplot2)
profvis({
geom_example <- data.frame(name = c(1, 2),
                           from1 = c(0,20),
                           to1 = c(20, 40)
) %>%
  cord_setting(plot_width = 3)

attri_example <- data.frame(name = c(1, 2),
                            rgb_col = c("#6F5F4CFF", "#947650FF"),
                            nameC = c("Ah", "Bv")
)

##
sf_example <- sf_polygon(df_geom = geom_example, df_attri = attri_example)

## Line attributes data frame:
lattri_example <- data.frame(name= c(1,2),
                             numberX = c(2, 10),
                             sd = c(1,1),
                             sm = c(TRUE, TRUE)
)

## Apply the line_mod fuction
line_example <- line_mod(df = geom_example,
                         line_attri = lattri_example)

## Split the Profile with the new line shape:
example_profile <- split_polygon(polygon = sf_example,
                                 line = line_example)

## Attributes table with the specifications for the transition:
##
df_smooth <- data.frame(
  buffer_size = c(5),
  buffer_number = c(30),
  nSides = c(10),
  rate = c(15),
  name = c(2)
)
#Applying the function
smooth_profile <- smooth_trans(lmod = line_example,
                               shape_mod = example_profile,
                               attr_df = df_smooth)
})


profvis({
lmod = line_example
shape_mod = example_profile
attr_df = df_smooth
shape = 10
seed = 33
smoothness = 2

stopifnot("sf" %in% class(lmod))
stopifnot("sf" %in% class(shape_mod))
stopifnot("data.frame" %in% class(attr_df))
stopifnot(c("buffer_size","buffer_number","nSides","rate","name") %in% colnames(attr_df))

##
#select attributes
##

df <- attr_df


attr_default <- shape_mod
sf::st_geometry(attr_default) <- NULL 

##
#select geometrys
##

shape_mod <- shape_mod %>%
  dplyr::select_(~ name, ~ geometry)

df_line <- lmod %>% 
  dplyr::select_(~name, ~geometry)

##
#Use polygons to create a smooth transition:
##

##
#1 Buffer:
##
df_buffer <- df_line %>% 
  dplyr::right_join(df, by = "name") %>% 
  #dplyr::group_by_(~name) %>% 
  sf::st_buffer(dist = df$buffer_size, endCapStyle = "FLAT") %>% 
  dplyr::filter_(~ buffer_size != 0) %>%
  sf::st_intersection(shape_mod) %>% 
  dplyr::select_(~name, ~nSides, ~rate, ~buffer_number) 

##
#Distribute points on the buffer:
##
set.seed(seed)
df_sample  <- sf::st_sample(df_buffer, size = df_buffer$buffer_number)
df_inter <-  sf::st_intersection(df_buffer, df_sample) 
##
#create the polygone: 
##
temp0 <- df_inter %>% 
  dplyr::group_by_(~name) %>% 
  dplyr::mutate_(area_size = ~rgamma(n(), shape = shape, rate = max(rate))) %>% 
  dplyr::ungroup()

temp1 <- point_2_polygon(temp0)

##
#smooth the polygons
##


temp1 <- smoothr::smooth(temp1, method = "ksmooth", smoothness = smoothness)

##
#Get id from neighboring horizon 
##

tf <- sf::st_intersects(temp1, shape_mod$geometry,  sparse = F )
tf1 <- cbind(tf, temp1$name)
index <- ncol(tf1)

for(i in 1:(index-1)){
  for(j in 1:nrow(tf1)){
    if(tf1[j,i] == 1){
      if(i == tf1[j,index]){
        tf1[j,index] <- tf1[j,index] -1
      }
    }
  }
}

temp1$name <- tf1[,index] #Overwrite names 

#clean cut 
temp_int <- temp1 %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate_(int = ~ dplyr::if_else(any(sf::st_intersects(geometry, df_line, sparse = F)) == T, T, F)) %>%
  sf::st_sf() %>% 
  dplyr::select_(~name, ~int)

##
#Resolving the polygons lying on the border 
##

#Select all polygons on the border 
temp2 <- temp_int %>% 
  dplyr::filter_(~ int == T) %>% 
  dplyr::select_(~ name) %>%
  dplyr::group_by_(~ name) %>% 
  dplyr::summarise(do_union = F) %>% 
  sf::st_buffer(0.0) %>% 
  sf::st_intersection(sf::st_union(shape_mod)) 

shape_mod1 <- sf::st_difference(shape_mod, sf::st_combine(temp2)) %>% 
  dplyr::select_(~name) %>% 
  rbind(temp2) %>%   
  dplyr::group_by_(~name) %>% 
  dplyr::summarise(do_union = T) 

#2. and  all others 
temp3 <- temp_int %>% 
  dplyr::filter_(~ int == F) %>% 
  dplyr::select_(~ name) %>%
  dplyr::group_by_(~ name) %>% 
  dplyr::summarise(do_union = F) %>% 
  sf::st_buffer(0.0) %>% 
  sf::st_intersection(sf::st_union(shape_mod1))

shape_mod2 <- sf::st_difference(shape_mod1, sf::st_combine(temp3)) %>% 
  dplyr::select_(~name) %>% 
  rbind(temp3) %>%   
  dplyr::group_by_(~name) %>% 
  dplyr::summarise(do_union = FALSE) %>% 
  dplyr::left_join(attr_default, by = "name")
})

library(shinytest)
recordTest()
