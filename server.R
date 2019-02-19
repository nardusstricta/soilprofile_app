shinyServer(function(input, output, session){

##
#data Import:####
##

  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE) 
  
  df_global1 <- reactive({
    data_mod(datafile())
  })
  #table output:
  output$table1 <- renderTable({
    df_global1()
  })
  
  #set coordinates:

  df_global2 <- reactive({
    
      cord_setting(df_global1(), plot_width = 3)

  })
  
  df_global3 <- reactive({
    sf_polygon(df_geom = df_global2(), df_attri = df_global1())
  })
  
  pvars <- reactive({
    nrow(df_global3())
  })
  
  y_vals <- reactive({
    default_yval(df_global3())
  })
   
##
#Plot1#####
##
 
  numberX <- callModule(sliderMod, id = "numberX",
                        label = df_global3()$nameC,
                        value = rep(1, pvars()), 
                        min = rep(1, pvars()),
                        max = rep(10, pvars()),
                        pvars = pvars(),
                        name = "Wave period",
                        step = rep(1, pvars())
  )
  
  sd <- callModule(sliderMod, id = "sd",
                   label = df_global3()$nameC,
                   value = rep(0, pvars()), 
                   min = rep(0, pvars()),
                   max = round(y_vals()/3),
                   pvars = pvars(),
                   name = "Amplitude",
                   step = rep(0.1, pvars()))
  
  sm <- callModule(selectMod, id = "sm", 
                   label = df_global3()$nameC,
                   value = rep(TRUE, pvars()),
                   name = "smooth",
                   pvars = pvars())
  #extention:
  select_smooth <- callModule(selectMod, id = "sel_smoth", 
                              label = df_global3()$nameC[-1],
                              value = rep(FALSE, pvars()-1),
                              name = "advanced settings",
                              pvars = pvars()-1)

  sm_buffer_size <- callModule(sliderMod, id = "sm_buf_si",
                               label = df_global3()$nameC[-1],
                               value = (y_vals()-1)/2, 
                               min = rep(0, pvars()-1),
                               max = y_vals()-1,
                               pvars = pvars()-1,
                               name = "buffer size",
                               step = rep(0.1, pvars()-1))
  
  sm_buffer_number <- callModule(numericMod, id = "sm_buffer_number",
                                 label = df_global3()$nameC[-1], 
                                 value = rep(50, pvars()-1),
                                 min = rep(3, pvars()-1),
                                 max = rep(100, pvars()-1),
                                 pvars = pvars()-1, 
                                 name = "number of polygons",
                                 step = rep(1, pvars()-1))
  
  sm_nSide <- callModule(sliderMod, id = "sm_nSide",
                         label = df_global3()$nameC[-1],
                         value = rep(10, pvars()-1),
                         min = rep(3, pvars()-1),
                         max = rep(40, pvars()-1),
                         pvars = pvars()-1,
                         name = "number of Sides",
                         step = rep(1, pvars()-1))
  
  sm_rate <- callModule(numericMod, id = "sm_rate",
                        label = df_global3()$nameC[-1], 
                        value = rep(0.6, pvars()-1),
                        min = rep(.1, pvars()-1),
                        max = rep(46, pvars()-1),
                        pvars = pvars()-1, 
                        name = "rate",
                        step = rep(.1, pvars()-1))
                               
  
                   
  df_smooth <- reactive({
    data.frame(
      buffer_size = sm_buffer_size(), 
      buffer_number = as.numeric(sm_buffer_number()),  
      nSides = as.numeric(sm_nSide()), 
      rate = sm_rate(),
      name = df_global3()$name[-1]
    )
  })


  
  shape_mod_print <- reactive({
    line1 <- line_mod(df_geom = df_global2(),
                      line_attri = data.frame(name= seq(pvars()),
                                            numberX = numberX(),
                                            sd = sd(), 
                                            sm = sm()
                      )
    )
    
    line2 <- split_polygon(polygon = df_global3(),
                           line = line1)
    
     #von 12 auf .1
    
    if(any(unlist(select_smooth()))== TRUE){
      #Applying smooth trans  function
      smooth_profile <- smooth_trans(lmod = line1,
                                     shape_mod = line2,
                                     attr_df = df_smooth()[select_smooth(),], 
                                     smoothness = 3, 
                                     shape = 1)
    }else{
      line2
    }
  })
  
  alpha_bg <- callModule(
    sliderMod, id = "alpha_bg", 
    label = df_global3()$nameC,
    value = rep(1, pvars()),
    min = rep(0, pvars()),
    max = rep(1, pvars()),
    pvars = pvars(),
    name = "number of Sides",
    step = rep(.1, pvars()))
  
  print_plot1 <- reactive({
    
      geom_sf(data = shape_mod_print(),
              fill = shape_mod_print()$rgb_col, 
              alpha = alpha_bg()
              )
  }) 
  
  ##
  #Raster Import:####
  ##

  



  
  ##
  #Plot2#####
  ##
  
  # row_select_pat <- callModule(selectMod,
  #                              id = "pattern",
  #                              label = df_global3()$nameC,
  #                              value = c(T,T,T,T),
  #                              name = "pattern",
  #                              pvars = pvars())
  # 
  # 
  # 
  # print_plot2 <- reactive({
  #   shiny::req(input$buffer)
  #   row_select_pat <- unlist(row_select_pat())
  #   texture_sf <- apply_texture(shape = shape_mod_print()[row_select_pat,],
  #                               buffer = input$buffer
  #                               )
  #   texture_sf <- par_default(texture_sf)
  # 
  #   data <- texture_sf %>%
  #     group_by(nameC)
  # 
  # 
  #     p <- geom_sf(data = data,
  #                  fill = texture_sf$bgc,
  #                  col = texture_sf$col,
  #                  shape = texture_sf$pch,
  #                  linetype = texture_sf$linetype)
  #     return(p)
  # })
  
  ##
  #Plot3#####
  ##
  
  # point_shape <- callModule(sliderMod,
  #                           id = "point_shape",
  #                           label = df_global3()$nameC,
  #                           value = c(0,7,9,10), 
  #                           min = c(0, 0, 0, 0),
  #                           max = c(10, 10, 10, 10),
  #                           pvars = pvars(), 
  #                           name = "Pointshape", 
  #                           step = rep(0.1, pvars()))
  # 
  # cellnumber <- callModule(sliderMod, 
  #                          id = "cellnumber", 
  #                          label = df_global3()$nameC,
  #                          value = c(0,7,9,10), 
  #                          min = c(0, 0, 0, 0),
  #                          max = c(20, 20, 20, 20),
  #                          pvars = pvars(), 
  #                          name = "cellnumber", 
  #                          step = rep(1, pvars())
  # )
  # 
  # rotation <- callModule(sliderMod, 
  #                          id = "rotation", 
  #                          label = df_global3()$nameC,
  #                          value = c(0,1,9,0), 
  #                          min = c(0, 0, 0, 0),
  #                          max = c(20, 20, 20, 20),
  #                          pvars = pvars(), 
  #                          name = "rotation", 
  #                          step = rep(1, pvars())
  # )
  #                          
  # spoint <- reactive({
  #   mat_spoint <- data.frame(
  #     name = c(1, 2, 3, 4),
  #     nSides = point_shape(),
  #     smooth = c(T, T, F, F),
  #     union = c(T, T, T, T),
  #     phi = c(0,.1,.5,0),
  #     strat = c(F, T, T, T), 
  #     cellnumber = cellnumber(),
  #     rotation = rotation()
  #   )
  #   skeleton(shape_mod = df_global3(),
  #            skeleton_mat = mat_spoint
  #   )
  #   
  # })
  # 
  # 
  # 
  # row_select <- callModule(selectMod, 
  #                          id = "rock",
  #                          label = df_global3()$nameC,
  #                          value = c(T,T,F,T),
  #                          name = "Skeleton",
  #                          pvars = pvars())
  # 
  # rock_col <- callModule(colMod, 
  #                        id = "rock_col",
  #                        label = df_global3()$nameC,
  #                        value = rep("#505C50A8", pvars()),
  #                        name = "Col",
  #                        pvars = pvars())
  # 
  # 
  # print_plot3 <- reactive({
  #   row_select <- unlist(row_select())
  #   geom_sf(data = spoint()[row_select,], fill = rock_col()[row_select])
  # })
  
  ##
  #renderCachedPlot#####
  ##

  output$plot1 <- renderCachedPlot({
    #shiny::req(print_plot1(), print_plot2(),print_plot3())
   t <-   ggplot() +
      print_plot1() +
      # print_plot2() +
      # print_plot3() +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             panel.background = element_blank())
   print(t)
   
      
    },
    cacheKeyExpr = {list(print_plot1(), shape_mod_print())}
  )
 

})

