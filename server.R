shinyServer(function(input, output, session){
  
  output$ui_transition <- renderUI({
    if (pvars > 0) {
      fluidRow(column(5, h2("SD"),
                      lapply(seq(pvars), function(i) {
                        sliderInput(
                          inputId = paste0("sd", df_global3$nameC[i]),
                          label =  df_global3$nameC[i],
                          value = df_global3$sd[i],
                          max = c(3,3,3,3,3)[i],
                          min = c(0,0,0,0,0)[i]
                        )}
                      )              
      ),
      column(
        5, h2("Number"),
        lapply(seq(pvars), function(i) {
          numericInput(inputId = paste0("number", 
                                        df_global3$nameC[i]), 
                       label = df_global3$nameC[i], 
                       value = df_global3$numberX[i])
        })
        
      )
      )
    }
  })
  
  output$ui_color <- renderUI({
    if (pvars > 0) {
      fluidRow(column(5,  h2("Munsell color"),
                      lapply(seq(pvars), function(i) {
                        checkboxInput(inputId = paste0("col", 
                                                            df_global3$nameC[i]), 
                                           label = df_global3$nameC[i],
                                           value = c(T,T,T,T)
                                      )
                      }
                      )
      ),
      column(5, h2("alpha"),
             lapply(seq(pvars), function(i) {
               sliderInput(inputId = paste0("alpha", 
                                            df_global3$nameC[i]), 
                           label = df_global3$nameC[i], 
                           value = 1, 
                           min = 0, 
                           max = 1
                           )
             }
             )
      )
      )
    }
    
  })
  
  
  
  
  
  
  reac_mat_list <- reactive({
    shiny::req(input$sdC)
    data.frame(name= seq(pvars),
               numberX = sapply(seq(pvars), 
                                function(i) {
                                  input[[paste0("number", df_global3$nameC[i])]]
                                  }),
               sd = sapply(seq(pvars), 
                           function(i) {
                             input[[paste0("sd", df_global3$nameC[i])]]
                           })
                             

               )

    })
  
  
  shape_mod_print <- reactive({
    
    line1 <- line_mod(df_polygon = df_global2, mat_line = reac_mat_list())
    
    line2 <- split_polygon(Polygon = df_global3,
                  Line = line1)
    return(line2)
  })
  
  
  print_plot1 <- reactive({
    row_select <- lapply(seq(pvars), function(i) {
      input[[paste0("col", df_global3$nameC[i])]]
    })
    row_select <- unlist(row_select)
    
    col_alpha <- lapply(seq(pvars), function(i) {
      input[[paste0("alpha", df_global3$nameC[i])]]
    })
    
    col_alpha <- unlist(col_alpha)
    
    data_temp <- shape_mod_print()[row_select,]
      geom_sf(data = data_temp, fill = data_temp$rgb_col, alpha = col_alpha) 
  }) 
  
  print_plot2 <- reactive({
    
    texture_sf <- apply_texture(shape = shape_mod_print())
    
    texture_sf <- par_default(texture_sf)
    
    data <- texture_sf %>% 
      group_by(nameC) 
    
      p <- geom_sf(data = data, 
                   fill = texture_sf$bgc,
                   col = texture_sf$col,
                   shape = texture_sf$pch, 
                   linetype = texture_sf$linetype)
      return(p)
  })
  
  spoint <- reactive({
    shiny::req(input$n)
    skeleton(shape_mod = df_global3[c(input$n,4),],
             point_shape = c(9,9),
             smooth = T,
             union = T, 
             jitter = F)
  })
  
  print_plot3 <- reactive({
    geom_sf(data = spoint(), col = "grey")
  })
  
  
  output$plot1 <- renderCachedPlot({
      ggplot() +
      print_plot1() +
      print_plot2() +
      print_plot3() +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             panel.background = element_blank())
       
      
    
    },
    cacheKeyExpr = {list(print_plot1(), print_plot2(), print_plot3())}
    
  )
  

})