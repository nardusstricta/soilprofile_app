shinyServer(function(input, output, session){

##
#data import:####
##

  datafile <- callModule(csvFile, "datafile") 
  
  df_global1 <- reactive({
    data_mod(datafile())
  })
  
  #table output:
  output$table1 <- renderTable({
    df_global1()
  })
  
  #set coordinates:

  df_global2 <- reactive({
      cord_setting(df_global1(), plot_width = input$plot_width)
  })
  
  df_global3 <- reactive({
    sf_polygon(df_geom = df_global2(), df_attri = df_global1())
  })
  
  #set length of each input paramert (number of horizons)
  pvars <- reactive({
    nrow(df_global3())
  })
  
  #set dafault values depending of the horizont dimension
  y_vals <- reactive({
    default_yval(df_global3())
  })
   
##
#Plot1 expansion#####
##
 
  #Adjustment for horizon shape expansion and color 
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
  #extention (horizont transitions):
  select_smooth <- callModule(selectMod, id = "sel_smoth", 
                              label = df_global3()$nameC[-1],
                              value = rep(FALSE, pvars()-1),
                              name = "horizon selection",
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
                               
  
  #Attribute table with the properties of the horizon transitions                  
  df_smooth <- reactive({
    data.frame(
      buffer_size = sm_buffer_size(), 
      buffer_number = as.numeric(sm_buffer_number()),  
      nSides = as.numeric(sm_nSide()), 
      rate = sm_rate(),
      name = df_global3()$name[-1]
    )
  })


  #Calculating the horizon polygons 
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
  
  #Adjustment of the colour transparency
  alpha_bg <- callModule(
    sliderMod, id = "alpha_bg", 
    label = df_global3()$nameC,
    value = rep(1, pvars()),
    min = rep(0, pvars()),
    max = rep(1, pvars()),
    pvars = pvars(),
    name = "color transparency",
    step = rep(.1, pvars()),
    sWH = FALSE)
  
  #plotting
  print_plot1 <- reactive({
      geom_sf(data = shape_mod_print(),
              fill = shape_mod_print()$rgb_col, 
              alpha = alpha_bg()
              ) 
  }) 
  

  
  ##
  #Plot2 texture#####
  ##
  
  print_plot2 <- callModule(outer_textur, "texture",
                            df_global3 = shape_mod_print, 
                            pvars = pvars
                            )
  
  ##
  #Plot3 rocks#####
  ##
  
  list_plot3 <- callModule(outer_rock,
                            "rock", 
                            df_global3 = df_global3, 
                            pvars = pvars)
  print_plot3 <- list_plot3[[1]]
  
  ##
  #Plot4 roots ####
  ##
  
  print_plot4 <- callModule(outer_root, 
                            "root", 
                            df_global3 = df_global3,
                            pvars = pvars)
  
  ##
  #Plot5 structure####
  ##
  
  print_plot5 <- callModule(textMod, 
                          id = "stru_soil",
                          label = df_global3()$nameC,
                          choices = struc_poly$name,
                          pvars = pvars(),
                          name = "select structure", 
                          global_df = shape_mod_print
                          )
  ##
  #Plot6 Photo import
  ##
  print_plot6 <- callModule(photoMod,
                            id = "photo",
                            pvars = pvars(),
                            global_df = shape_mod_print
  )
  ##
  #Process Layer####
  ##
  
  geom_process <- callModule(processMod, "process", 
                             shape = shape,
                             plot_brush = reactive({input$plot_brush}), 
                             spoint = list_plot3[[2]]) 
  

  ##
  #renderCachedPlot#####
  ##

  output$plot1 <- renderCachedPlot({
    plotInput()
    },
    cacheKeyExpr = {list(print_plot1(),
                         print_plot2(),
                         print_plot3(),
                         print_plot4(),
                         print_plot5(),
                         print_plot6(),
                         geom_process()
      )}
  )
  
  plotInput = function() {
    ggplot() +
      print_plot1() +
      print_plot4() +
      print_plot2() +
      print_plot3() +
      print_plot5() +
      print_plot6() + 
      geom_process() +
      geom_sf_text(data = df_global3(), aes(label = nameC),
                   nudge_x = (sf::st_bbox(df_global3())[[3]] -
                                sf::st_bbox(df_global3())[[1]]) /2 + 3) + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.background = element_blank())
  }
  
  #download file
  output$download1 <- downloadHandler(
    filename = function() {paste("soilprofile", '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "pdf")
    }
  )
  

})



