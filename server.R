shinyServer(function(input, output) {
  #addClass(selector = "body", class = "sidebar-collapse")
  #ouput plot1
  
  #UIOUPUT
  output$perbandigan <- renderUI({
    
    stunting <- d_tinggi %>% 
      group_by(Provinsi) %>% 
      filter(tahun %in% 2018 & Provinsi %in% input$b_provinsi) %>%
      summarise(prevalensi= sum(prevalensi))
    lmgizi <- d_gizi %>% 
      group_by(Provinsi) %>% 
      filter(tahun %in% 2018 & Provinsi %in% input$b_provinsi) %>% 
      summarise(prevalensi = mean(prevalensi))
    lmipm <- ipm_long %>% 
      group_by(Provinsi) %>% 
      filter(tahun %in% 2018 & Provinsi %in% input$b_provinsi) %>% 
      summarise(ipm = mean(ipm))
    lmkemiskinan <- miskin %>% 
      group_by(Provinsi) %>% 
      filter(tahun %in% 2018 & Provinsi %in% input$b_provinsi & daerah %in% "jumlah" & periode %in% "smt_1") %>% 
      summarise(kemiskinan = mean(kemiskinan))
    
    fluidPage(
      sliderInput("b_gizi",
                  min = 0,
                  max = 100,
                  value = lmgizi$prevalensi,
                  label = "Prevalensi Gizi"
      ),
      sliderInput("b_ipm",
                  min = 0,
                  max = 100,
                  value = lmipm$ipm,
                  label = "IPM (Indeks Pembangunan Manusia)"
      ),
      sliderInput("b_miskin",
                  min = 0,
                  max = 50,
                  value = lmkemiskinan$kemiskinan,
                  label = "Indeks Kemiskinan"
      ),
    )
    
  })

  output$jenis <- renderUI({
    print(input$jenis)
    if(input$jenis == "gizi"){
      print("sini")
      column(
      width = 12,
      selectInput(inputId="umur",
                  label = "Umur(bulan)",
                  choices = c("Semua","23","59")),
      selectInput(inputId="gizi",
                  label = "Kategori gizi",
                  choices =c("Semua","buruk","kekurangan","kurang"))
      )
    }else if (input$jenis == "tinggi"){
      selectInput(inputId="tinggi",
                  label = "Kategori tinggi",
                  choices =c("Semua","pendek","sangat_pendek"))
    }
    
    })
    
  output$info <- renderUI({
    d_tinggi1 <- d_tinggi %>% 
      filter(tinggi %in% "pendek") %>% 
      summarise(prev=mean(prevalensi))
    d_tinggi2 <- d_tinggi %>% 
      filter(tinggi %in% "sangat_pendek") %>% 
      summarise(prev=mean(prevalensi))
    fluidRow(
      
    valueBox(value = paste(round(mean(d_tinggi1$prev),digits = 2),"%") ,
             subtitle = "Rata-rata Prevalensi Pendek",
             icon = icon("tags"),
             color = "red"),
    valueBox(value = paste(round(mean(d_tinggi2$prev),digits = 2),"%") ,
             subtitle = "Rata-rata Prevalensi Sangat Pendek",
             icon = icon("tags"),
             color = "yellow")
    
    )
  })
  
  output$ratastunt <- renderUI({
    d_tinggi1 <- d_tinggi %>% 
      #filter(tinggi %in% "pendek" & tahun %in% input$b_tahun[1]:input$b_tahun[2]) %>% 
      summarise(prev=mean(prevalensi))
    d_tinggi2 <- d_tinggi %>% 
      #filter(tinggi %in% "sangat_pendek" & tahun %in% input$b_tahun[1]:input$b_tahun[2]) %>% 
      summarise(prev=mean(prevalensi))
      d_tinggi_tot = mean(d_tinggi1$prev+d_tinggi2$prev)
      valueBox(width=3,value = paste(round(d_tinggi_tot,digits = 2),"%"),
               subtitle = "Rata-rata Prevalensi jumlah Stunting",
               icon = icon("tag"),
               color = "yellow")
  })
  
  output$ratamiskin <- renderUI({
    d_miskin1 <- miskin %>% 
      group_by(Provinsi) %>% 
      filter(daerah %in% "jumlah" & periode %in% "smt_1") %>% 
      summarise(kemiskinan=mean(kemiskinan))
    valueBox(width=3,value = paste(round(mean(d_miskin1$kemiskinan),digits = 2),"%"),
             subtitle = "Rata-rata Indeks Kemiskinan",
             icon = icon("tag"),
             color = "aqua")
  })
  
  output$rataipm <- renderUI({
    d_ipm1 <- ipm_long %>% 
      #filter(tahun %in% input$b_tahun[1]:input$b_tahun[2]) %>% 
      summarise(ipm=mean(ipm))

    valueBox(width=3,value = paste(round(mean(d_ipm1$ipm),digits = 2)),
             subtitle = "Rata-rata IPM",
             icon = icon("tag"),
             color = "red")
  })
  
  output$ratagizi <- renderUI({
    d_gizi1 <- d_gizi %>% 
      #filter(gizi %in% "buruk" & tahun %in% input$b_tahun[1]:input$b_tahun[2]) %>% 
      summarise(prev=mean(prevalensi))
    d_gizi2 <- d_gizi %>% 
      #filter(gizi %in% "kurang" & tahun %in% input$b_tahun[1]:input$b_tahun[2]) %>% 
      summarise(prev=mean(prevalensi))
    d_gizi3 <- d_gizi %>% 
      #filter(gizi %in% "kekurangan" & tahun %in% input$b_tahun[1]:input$b_tahun[2]) %>% 
      summarise(prev=mean(prevalensi))
    d_gizi_tot = mean(d_gizi1$prev+d_gizi2$prev+d_gizi3$prev)
    valueBox(width=3,value = paste(round(d_gizi_tot,digits = 2),"%"),
             subtitle = "Rata-rata Prevalensi jumlah gizi",
             icon = icon("tags"),
             color = "orange")
  })

  output$judul <- renderUI({
      h2(style="text-align:center;color:#D7466C",
         paste("SEBARAN KASUS STUNTING DI INDONESIA\n TAHUN ",paste(input$tahun,collapse = " ")))
    
  })
  output$provinsi <- renderUI({
    h4(style="text-align:center;color:black",
       paste("Provinsi ",paste(input$provinsi,collapse = ","),"Tahun ",paste(input$tahun,collapse = ",")))
    
  })
  
  output$rerataindo <- renderUI({
    rerataindo <- d_tinggi %>% 
      filter(tahun %in% input$tahun) %>% 
      mutate(state = str_to_title(Provinsi))
    h4(paste(round(mean(rerataindo$prevalensi),digits = 2)))
  })
  output$tidak_rawan <- renderUI({
    tidak_rawan <- d_tinggi %>% 
      filter(tahun %in% input$tahun)
    tidak_rawan<-tidak_rawan %>% 
      group_by(Provinsi) %>% 
      summarise(prev = sum(prevalensi)) %>% 
      arrange(prev) %>% 
      head(1)
    tidak_rawan
    h4(paste(tidak_rawan$Provinsi))
  })
  output$paling_rawan <- renderUI({
    paling_rawan <- d_tinggi %>% 
      filter(tahun %in% input$tahun) %>% 
      mutate(state = str_to_title(Provinsi))
    
    paling_rawan<-paling_rawan %>% 
      group_by(Provinsi) %>% 
      summarise(prev = sum(prevalensi)) %>% 
      arrange(-prev) %>% 
      head(1)
    paling_rawan
    h4(paste(paling_rawan$Provinsi))
  })
  
  #end UIOUPUT
  #plot
  
  output$plotgizi1 <- renderPlotly({
      plot_prev<-d_gizi %>% 
        filter(gizi %in% "buruk" ) %>% 
        group_by(state) %>%
        summarise(prev = mean(prevalensi)) %>% 
        arrange(prev) %>% 
        mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prev}%"))
    
    plotgizi <- plot_prev %>% 
      ggplot(mapping = aes(
        x= prev,
        y= reorder(state,prev),
        text=label))+
      geom_col(mapping = aes(fill=prev))+
      scale_fill_gradient(low = "#00C0EF",high = "blue")+
      labs(
        #title = "Prevalensi masalah gizi",
        y="Provinsi",
        x="Prevalensi")+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(p=plotgizi,tooltip = "label")
  })
  
  output$plotgizi2 <- renderPlotly({
    plot_prev<-d_gizi %>% 
      filter(gizi %in% "kekurangan" ) %>%  
      group_by(state) %>%
      summarise(prev = mean(prevalensi)) %>% 
      arrange(prev) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prev}%"))
    
    plotgizi <- plot_prev %>% 
      ggplot(mapping = aes(
        x= prev,
        y= reorder(state,prev),
        text=label))+
      geom_col(mapping = aes(fill=prev))+
      scale_fill_gradient(low = "#00C0EF",high = "blue")+
      labs(
        #title = "Prevalensi masalah gizi",
        y="Provinsi",
        x="Prevalensi")+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(p=plotgizi,tooltip = "label")
  })
  
  output$plotgizi3 <- renderPlotly({
    plot_prev<-d_gizi %>% 
      filter(gizi %in% "kurang" ) %>% 
      group_by(state) %>%
      summarise(prev = mean(prevalensi)) %>% 
      arrange(prev) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prev}%"))
    
    plotgizi <- plot_prev %>% 
      ggplot(mapping = aes(
        x= prev,
        y= reorder(state,prev),
        text=label))+
      geom_col(mapping = aes(fill=prev))+
      scale_fill_gradient(low = "#00C0EF",high = "blue")+
      labs(
        #title = "Prevalensi masalah gizi",
        y="Provinsi",
        x="Prevalensi")+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(p=plotgizi,tooltip = "label")
  })
  
  output$plotipm <- renderPlotly({
    plot_prev<-d_tinggi %>% 
      #filter(tahun %in% input$tahun2 ) %>% 
      group_by(state) %>%
      summarise(prev = mean(prevalensi)) %>% 
      arrange(prev) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prev}%"))
    
    plottinggi <- plot_prev %>% 
      ggplot(mapping = aes(
        x= prev,
        y= reorder(state,prev),
        text=label))+
      geom_col(mapping = aes(fill=prev))+
      scale_fill_gradient(low = "orange",high = "firebrick")+
      labs(
        #title = "Prevalensi masalah tinggi",
        y="Provinsi",
        x="Prevalensi")+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(p=plottinggi,tooltip = "label")
  })
  
  output$plottinggi1 <- renderPlotly({
    plot_prev<-d_tinggi %>% 
      filter(tinggi %in% "pendek" ) %>% 
      group_by(state) %>%
      summarise(prev = mean(prevalensi)) %>% 
      arrange(prev) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prev}%"))
    
    plottinggi <- plot_prev %>% 
      ggplot(mapping = aes(
        x= prev,
        y= reorder(state,prev),
        text=label))+
      geom_col(mapping = aes(fill=prev))+
      scale_fill_gradient(low = "orange",high = "firebrick")+
      labs(
        #title = "Prevalensi masalah tinggi",
        y="Provinsi",
        x="Prevalensi")+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(p=plottinggi,tooltip = "label")
  })
  output$plottinggi2 <- renderPlotly({
    plot_prev<-d_tinggi %>% 
      filter(tinggi %in% "sangat_pendek" ) %>% 
      group_by(state) %>%
      summarise(prev = mean(prevalensi)) %>% 
      arrange(prev) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prev}%"))
    
    plottinggi <- plot_prev %>% 
      ggplot(mapping = aes(
        x= prev,
        y= reorder(state,prev),
        text=label))+
      geom_col(mapping = aes(fill=prev))+
      scale_fill_gradient(low = "orange",high = "firebrick")+
      labs(
        #title = "Prevalensi masalah tinggi",
        y="Provinsi",
        x="Prevalensi")+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(p=plottinggi,tooltip = "label")
  })
  
  #sscatter
  output$plotbandingscatter <- renderPlotly({
    plot_prev<-data_gabung %>% 
      filter(tahun %in% input$tahun2 ) %>%
      mutate(label = glue("Provinsi: {state}
                      tinggi: {prevalensi_tinggi}%
                     gizi: {prevalensi_gizi}%"))
    
    plot_prev %>% 
      mutate(quadrant = case_when(
        prevalensi_gizi <= median(prevalensi_gizi, na.rm = TRUE) & prevalensi_tinggi <= median(prevalensi_tinggi, na.rm = TRUE) ~ "Q1",
        prevalensi_gizi > median(prevalensi_gizi, na.rm = TRUE) & prevalensi_tinggi < median(prevalensi_tinggi, na.rm = TRUE) ~ "Q2",
        prevalensi_gizi >= median(prevalensi_gizi, na.rm = TRUE) & prevalensi_tinggi >= median(prevalensi_tinggi, na.rm = TRUE) ~ "Q3", 
        TRUE ~ "Q4"
      ))
    
    plotbandingscatter<-plot_prev %>% 
      ggplot(mapping = aes(
        x= prevalensi_gizi,
        y= prevalensi_tinggi,
        color=quadrant,
        label=label
        ))+
      geom_vline(xintercept = median(plot_prev$prevalensi_gizi, na.rm=TRUE))+
      geom_hline(yintercept = median(plot_prev$prevalensi_tinggi, na.rm=TRUE))+
      geom_point(aes(size = plot_prev$prevalensi_gizi))+
      labs(
        title = "Perbandingan indikator tinggi dan gizi",
        y="Tinggi(Pendek & Sangat Pendek)",
        x="Gizi(Kurang,Kekurangan & Buruk)")+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(p=plotbandingscatter,tooltip = "label")
  })
  
  output$bandinggizistunting <- renderPlotly({
    stunting <- d_tinggi %>% 
      group_by(tinggi,Provinsi,tahun) %>% 
      summarise(prevalensi=sum(prevalensi)) %>% 
      group_by(Provinsi) %>% 
      filter(tahun %in% c(2016:2018)) %>%
      summarise(prevalensi= mean(prevalensi))
    lmgizi <- d_gizi %>% 
      group_by(Provinsi) %>% 
      filter(tahun %in% c(2016:2018)) %>% 
      summarise(prevalensi = mean(prevalensi))
    lmipm <- ipm_long %>% 
      group_by(Provinsi) %>% 
      filter(tahun %in% c(2016:2018)) %>% 
      summarise(ipm = mean(ipm))
    lmkemiskinan <- miskin %>% 
      group_by(Provinsi) %>% 
      filter(tahun %in% c(2016:2018) & daerah %in% "jumlah" & periode %in% "smt_1") %>% 
      summarise(kemiskinan = mean(kemiskinan))
    
    #regresi
    lmstunting= lm(stunting$prevalensi~lmgizi$prevalensi+lmipm$ipm+lmkemiskinan$kemiskinan)
    i_intercept <-lmstunting$coefficients[1]
    i_gizi <- lmstunting$coefficients[2]
    i_ipm <- lmstunting$coefficients[3]
    i_miskin<-lmstunting$coefficients[4]
    
    lmstunting$fitted.values
    
    #rumus
    fit<- i_intercept+input$b_gizi*i_gizi+input$b_ipm*i_ipm+input$b_miskin*i_miskin
    fit_df <- stunting %>% 
      filter(Provinsi %in% input$b_provinsi) %>% 
      mutate(fit =fit)
    
    d1<-d_tinggi %>% 
      group_by(Provinsi,tahun) %>% 
      filter(Provinsi %in% input$b_provinsi) %>% 
      summarise(prev = mean(prevalensi)) %>% 
      mutate(label = glue("Provinsi {Provinsi}
                          prevalensi: {prev}%"))

    plotbanding<-ggplot(mapping = aes(
      x=d1$tahun,
      color="red"
    ))+
      geom_segment(aes(x = 2018,
                       xend = 2019,
                       y = d1$prev[3],
                       yend =fit_df$fit ),
                   color="orange")+
      geom_point(aes(x=2019
                     ,y=fit_df$fit,
                     size=10,
                     text=glue("Provinsi: {fit_df$Provinsi}
                               Estimasi: {fit_df$fit}")),
                      color="orange",
                 )+
      geom_line(y=d1$prev)+
      geom_point(aes(y=d1$prev,size = 10,label=d1$label))+
      
      labs(
        title = "Estimasi angka stunting tiap provinsi tahun 2019 berdasarkan indikator",
        y="Prevalensi Stunting(Pendek & Sangat Pendek)",
        x="Tahun",
        size=d1$prev)+
    theme_classic()+
    theme(legend.position = "none")
    
    ggplotly(p=plotbanding,tooltip = c("d1$label","text"))
  })
  
  output$bandinggizistunt <- renderPlotly({
    d1<-d_tinggi %>% 
      group_by(Provinsi) %>% 
      summarise(prev = mean(prevalensi))
    mean(d1$prev)
    d1<-d1 %>% 
      mutate(status = case_when(prev > mean(d1$prev) ~ "Butuh perhatian",
                                            TRUE ~ "Aman"))
    d2<-d_gizi %>% 
      group_by(Provinsi) %>% 
      summarise(prev = mean(prevalensi)) %>% 
      mutate(label = glue("Provinsi: {Provinsi}
                      stunting: {d1$prev}%
                     gizi: {prev}%"))
    
      plotbanding<-ggplot(mapping = aes(
        x= d2$prev,
        y= d1$prev,
        label=d2$label
      ))+
      geom_smooth(method = "lm", se = FALSE)+
      geom_point(aes(size = d2$prev,color=d1$status))+
      
      labs(
        title = "Perbandingan Prevalensi stunting dengan indikator gizi",
        y="Prevalensi Stunting(Pendek & Sangat Pendek)",
        x="Prevalensi Gizi(Kurang,Kekurangan & Buruk)",
        size="Status",col="")
      theme_classic()
      
    
    ggplotly(p=plotbanding,tooltip = "d2$label")
  })
  
  output$bandingipmstunt <- renderPlotly({
    d1_ipm<-d_tinggi %>% 
      group_by(Provinsi) %>% 
      summarise(prev = mean(prevalensi)) %>% 
      arrange(Provinsi)
    
    d1_ipm<-d1_ipm %>% 
      mutate(status = case_when(prev > mean(d1_ipm$prev) ~ "Butuh perhatian",
                                TRUE ~ "Aman"))
    
    d2<-ipm_long %>% 
      group_by(Provinsi) %>% 
      summarise(prev = mean(ipm)) %>% 
      mutate(label = glue("Provinsi: {Provinsi}
                      stunting: {d1_ipm$prev}%
                     ipm: {prev}")) %>%
      arrange(Provinsi)
    
    plotbanding<-ggplot(mapping = aes(
      x= d2$prev,
      y= d1_ipm$prev,
      label=d2$label
    ))+
      geom_smooth(method = "lm", se = FALSE)+
      geom_point(aes(size = d2$prev,color=d1_ipm$status))+
      labs(
        title = "Prevalensi stunting dengan indikator ipm",
        y="Prevalensi Stunting(Pendek & Sangat Pendek)",
        x="Indeks Pembangunan Manusia",
        size="Status",col="")
    theme_classic()
    
    
    ggplotly(p=plotbanding,tooltip = "d2$label")
  })
  output$bandingkemiskinanstunt <- renderPlotly({
    d1_gdp<-d_tinggi %>% 
      group_by(Provinsi) %>% 
      summarise(prev = mean(prevalensi))
    
    d1_gdp<-d1_gdp %>% 
      mutate(status = case_when(prev > mean(d1_gdp$prev) ~ "Butuh perhatian",
                                TRUE ~ "Aman"))
    
    d2<-miskin %>% 
      group_by(Provinsi) %>% 
      filter(daerah %in% "jumlah") %>% 
      summarise(kemiskinan = mean(kemiskinan)) %>% 
      mutate(label = glue("Provinsi: {Provinsi}
                      stunting: {d1_gdp$prev}%
                     kemiskinan: {kemiskinan}"))
    
    plotbanding<-ggplot(mapping = aes(
      x= d2$kemiskinan,
      y= d1_gdp$prev,
      label=d2$label
    ))+
      geom_smooth(method = "lm", se = FALSE)+
      geom_point(aes(size = d2$kemiskinan,color=d1_gdp$status))+
      labs(
        title = "Prevalensi stunting dengan indikator Indeks Kemiskinan",
        y="Prevalensi Stunting(Pendek & Sangat Pendek)",
        x="Indeks kemiskinan",
        size="Status",col="")+
    theme_classic()
    
    
    ggplotly(p=plotbanding,tooltip = "d2$label")
  })
  
  output$bandingipmgizi <- renderPlotly({
    d1_ipm<-d_gizi %>% 
      group_by(Provinsi) %>% 
      summarise(prev = mean(prevalensi)) %>% 
      arrange(Provinsi)
    
    d1_ipm<-d1_ipm %>% 
      mutate(status = case_when(prev > mean(d1_ipm$prev) ~ "Butuh perhatian",
                                TRUE ~ "Aman"))
    
    d2<-ipm_long %>% 
      group_by(Provinsi) %>% 
      summarise(prev = mean(ipm)) %>% 
      mutate(label = glue("Provinsi: {Provinsi}
                      gizi: {d1_ipm$prev}%
                     ipm: {prev}")) %>%
      arrange(Provinsi)
    
    plotbanding<-ggplot(mapping = aes(
      x= d2$prev,
      y= d1_ipm$prev,
      label=d2$label
    ))+
      geom_smooth(method = "lm", se = FALSE)+
      geom_point(aes(size = d2$prev,color=d1_ipm$status))+
      labs(
        title = "Prevalensi Gizi dengan indikator ipm",
        y="Prevalensi Gizi(Kurang,Kekurangan & Buruk)",
        x="Indeks Pembangunan Manusia",
        size="Status",col="")
    theme_classic()
    
    
    ggplotly(p=plotbanding,tooltip = "d2$label")
  })
  output$bandingkemiskinangizi <- renderPlotly({
    d1_gdp<-d_gizi %>% 
      group_by(Provinsi) %>% 
      summarise(prev = mean(prevalensi))
    
    d1_gdp<-d1_gdp %>% 
      mutate(status = case_when(prev > mean(d1_gdp$prev) ~ "Butuh perhatian",
                                TRUE ~ "Aman"))
    
    d2<-miskin %>% 
      group_by(Provinsi) %>% 
      filter(daerah %in% "jumlah") %>% 
      summarise(kemiskinan = mean(kemiskinan)) %>% 
      mutate(label = glue("Provinsi: {Provinsi}
                      gizi: {d1_gdp$prev}%
                     kemiskinan: {kemiskinan}"))
    
    plotbanding<-ggplot(mapping = aes(
      x= d2$kemiskinan,
      y= d1_gdp$prev,
      label=d2$label
    ))+
      geom_smooth(method = "lm", se = FALSE)+
      geom_point(aes(size = d2$kemiskinan,color=d1_gdp$status))+
      labs(
        title = "Prevalensi gizi dengan indikator Indeks Kemiskinan",
        y="Prevalensi Gizi(Kurang,Kekurangan & Buruk)",
        x="Indeks Kemiskinan",
        size="Status",col="")+
    theme_classic()
    
    
    ggplotly(p=plotbanding,tooltip = "d2$label")
  })
  
  #end scatter
  output$plottotal <- renderPlotly({
    plot_prev<-data_gabung %>% 
      filter(tahun %in% input$tahun2 ) %>% 
      group_by(state) %>%
      summarise(prev = mean(prevalensi)) %>% 
      arrange(prev) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prev}%"))
    
    plottotal <- plot_prev %>% 
      ggplot(mapping = aes(
        x= prev,
        y= reorder(state,prev),
        text=label))+
      geom_col(mapping = aes(fill=prev))+
      scale_fill_gradient(low = "orange",high = "firebrick")+
      labs(
        title = "Prevalensi Total Rawan Potensi Stunting",
        y="Provinsi",
        x="Prevalensi")+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(p=plottotal,tooltip = "label")
  })
  
  output$plotperbandingan <- renderPlotly({
    plot_perbandingan<-data_gabung %>% 
      filter(state %in% input$prov1) %>% 
      mutate(tahun= ISOdate(tahun, 1, 1)) %>% 
      mutate(tahun= as.Date(tahun)) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prevalensi}%"))
    
    plotbanding <- plot_perbandingan %>% 
      ggplot(mapping = aes(
        x= tahun,
        y= prevalensi,
        group=state,
        color=state,
        label=label))+
      labs(
        title = "Perbandingan Prevalensi Provinsi",
        y="Prevalensi",
        x="Tahun")+
      geom_line()+
      geom_point()+
      theme_classic()
    
    ggplotly(p=plotbanding,tooltip = "label")
  })
  
  output$plottrenstunting <- renderPlotly({
    plot_perbandingan<-d_tinggi %>% 
      group_by(tahun) %>% 
      summarise(prev = mean(prevalensi)) %>% 
      mutate(label = glue("Tahun : {tahun}
                          Prevalensi: {prev}%"))
    
    plotbanding <- plot_perbandingan %>% 
      ggplot(mapping = aes(
        x= tahun,
        y= prev,
        label=label))+
      scale_y_continuous(breaks = seq(0,20,by=1))+
      scale_x_continuous(breaks = seq(2016,2018,by=1))+
      
      labs(
        title = "Tren Prevalensi Nasional 2016-2018",
        y="Prevalensi",
        x="Tahun")+
      geom_line()+
      geom_point()+
      theme_classic()
    
    ggplotly(p=plotbanding,tooltip = "label")
  })
  
  output$plotperbandingantinggi <- renderPlotly({
    plot_perbandingan<-d_tinggi %>% 
      filter(state %in% input$prov1) %>% 
      mutate(tahun= ISOdate(tahun, 1, 1)) %>% 
      mutate(tahun= as.Date(tahun)) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prevalensi}%
                      tinggi :{tinggi}"))
    
    plotbanding <- plot_perbandingan %>% 
      ggplot(mapping = aes(
        x= tahun,
        y= prevalensi,
        group=state,
        color=state,
        label=label))+
      labs(
        title = "Perbandingan Prevalensi indikator tinggi Provinsi",
        y="Prevalensi",
        x="Tahun")+
      geom_point(size=6)
      #geom_line()+
      #geom_point()+
      theme_classic()
    
    ggplotly(p=plotbanding,tooltip = "label")
  })
  
  output$plotperbandingangizi <- renderPlotly({
    plot_perbandingan<-d_gizi %>% 
      filter(state %in% input$prov1) %>% 
      mutate(tahun= ISOdate(tahun, 1, 1)) %>% 
      mutate(tahun= as.Date(tahun)) %>% 
      mutate(label = glue("Provinsi: {state}
                     Prevalensi: {prevalensi}%"))
    
    plotbanding <- plot_perbandingan %>% 
      ggplot(mapping = aes(
        x= tahun,
        y= prevalensi,
        group=state,
        color=state,
        label=label))+
      labs(
        title = "Perbandingan Prevalensi indikator gizi Provinsi",
        y="Prevalensi",
        x="Tahun")+
      geom_line()+
      geom_point()+
      theme_classic()
    
    ggplotly(p=plotbanding,tooltip = "label")
  })
  
  #end plot
  #tabel
  output$tabelgizi <- renderDataTable({
    datatable(
      data=d_gizi,
      options= list(
        scrollX=TRUE,
        pageLength=10
      )
    )
  })
  output$tabelkemiskinan <- renderDataTable({
    datatable(
      data=miskin,
      options= list(
        scrollX=TRUE,
        pageLength=10
      )
    )
  })
  output$tabelipm <- renderDataTable({
    datatable(
      data=ipm_long,
      options= list(
        scrollX=TRUE,
        pageLength=10
      )
    )
  })
  output$tabeltinggi <- renderDataTable({
    datatable(
      data=d_tinggi,
      options= list(
        scrollX=TRUE,
        pageLength=10
      )
    )
  })
  output$tabelpeta <- renderDataTable({
    datatable(
      data=map_indo,
      options= list(
        scrollX=TRUE,
        pageLength=10
      )
    )
  })
  
  output$rank <- renderTable({
    rank<-d_tinggi %>% 
      filter(tahun %in% input$tahun) %>% 
      group_by(Provinsi) %>% 
      summarise(prev = sum(prevalensi)) %>% 
      arrange(-prev) %>% 
      head(5)
    rank
  })
  output$rankpeningkatan <- renderDataTable({
    rank<-d_tinggi %>% 
      group_by(Provinsi,tahun) %>% 
      summarise(prevalensi= sum(prevalensi)) %>% 
      filter(tahun %in% c(2016,2017)) 
    
    rank <- pivot_wider(data = rank,
                names_from = tahun,
                values_from = prevalensi) %>% 
      mutate(peningkatan = round((`2017`-`2016`),digits = 2)) %>% 
            filter(peningkatan > 0) %>% 
            select(-`2016`,-`2017`) %>% 
            arrange(-peningkatan)
    datatable(
      data=rank,
      options= list(
        scrollX=TRUE,
        pageLength=5
      )
    )
  })
  
  output$rankpenurunan <- renderDataTable({
    rank<-d_tinggi %>% 
      group_by(Provinsi,tahun) %>% 
      summarise(prevalensi= sum(prevalensi)) %>% 
      filter(tahun %in% c(2016,2017))
    
    rank <- pivot_wider(data = rank,
                        names_from = tahun,
                        values_from = prevalensi) %>% 
      mutate(peningkatan = round((`2017`-`2016`),digits = 2)) %>% 
      filter(peningkatan < 0) %>% 
      select(-`2016`,-`2017`) %>% 
      arrange(peningkatan)
    datatable(
      data=rank,
      options= list(
        scrollX=TRUE,
        pageLength=10
      )
    )
  })
  #endtabel
  #map
  output$mymap <- renderLeaflet({
    pal <- colorNumeric("magma", NULL)
    if(input$jenis == "gizi"){
      d_gizi_summ <- d_gizi 
      d_gizi_summ
      d_gizi_join<- left_join(map_indo,d_gizi_summ,by="state")
      peta<- d_gizi_join
      if(input$gizi == "Semua" & input$umur == "Semua"){
        peta<- peta %>% 
          filter(tahun %in% input$tahun)
      }else if(input$gizi == "Semua" & input$umur != "Semua"){
        peta<- peta %>% 
          filter(tahun %in% input$tahun & umur %in% input$umur)
      }else if(input$gizi != "Semua" & input$umur == "Semua"){
        peta<- peta %>% 
        filter(tahun %in% input$tahun & gizi %in% input$gizi)
      }else{
        peta<- peta %>% 
        filter(tahun %in% input$tahun & gizi %in% input$gizi & umur %in% input$umur)
      }
    }else if(input$jenis == "tinggi"){
      d_tinggi_summ <- d_tinggi
      d_tinggi_join<-left_join(map_indo,d_tinggi_summ,by="state")
      peta<- d_tinggi_join
      if(input$tinggi == "Semua"){
        peta<- peta %>% 
          filter(tahun %in% input$tahun)
      }else{
        peta<- peta %>% 
          filter(tahun %in% input$tahun & tinggi %in% input$tinggi)
      }
    }else{
      data_gabung_summ <- data_gabung 
      gabung<- left_join(map_indo,data_gabung_summ,by="state")
      peta<- gabung
      peta<- peta %>% 
        filter(tahun %in% input$tahun)
    }
    peta
    leaflet(peta) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(log10(prevalensi)),
                  label = ~paste0(state, ": ", formatC(prevalensi, big.mark = ","),"%")) %>%
      addLegend("topright",pal = pal, values = ~log10(prevalensi), opacity = 1.0,title="Prevalensi",
                  labFormat = labelFormat(transform = function(x) round(10^x))) %>% 
      addProviderTiles(providers$CartoDB.Positron) 
  })
  
  output$mapstunting <- renderLeaflet({
    pal <- colorNumeric("magma", NULL,reverse = TRUE)
      d_stunting_summ <- d_tinggi %>% 
        group_by(Provinsi,tahun) %>% 
        summarise(prevalensi= sum(prevalensi)) %>% 
        filter(tahun %in% input$tahun) %>% 
        mutate(slug= gsub(" ","",paste("indonesia-",str_to_lower(gsub(" ","",Provinsi)))))
      d_stunting_join<-left_join(map_indo,d_stunting_summ,by="slug")
      peta<- d_stunting_join
      
    leaflet(peta) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(log10(prevalensi)),
                  label = ~paste0(Provinsi, ": ", formatC(prevalensi, big.mark = ","),"%")) %>%
      addLegend("topright",pal = pal, values = ~log10(prevalensi), opacity = 1.0,title="Prevalensi",
                labFormat = labelFormat(transform = function(x) round(10^x))) %>% 
      addProviderTiles(providers$CartoDB.Positron) 
  })
  #endmap

  
 
  
})