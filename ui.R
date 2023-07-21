navbarPage(
  
  #theme = bs_theme(version = 4, bootswatch = "minty"),
  theme=shinytheme("united"),
  title=div(img(width=65,src="lan.png")),
  header = tagList(
    useShinydashboard()
  ),
  tags$style(HTML("
        .tabs-above > .nav > li[class=active] > a {
           background-color: #000;
           color: #FFF;
        }")),
           
          tabPanel("Overview",
                   valueBox(value = length(unique(d_tinggi$Provinsi))-1,
                            subtitle = "Total Provinsi",
                            icon = icon("asterisk"),
                            color = "red"),
                   valueBox(value = paste(head(unique(d_tinggi$tahun),1),"-",tail(unique(d_tinggi$tahun),1)),
                            subtitle = "Tahun",
                            icon = icon("calendar"),
                            color = "yellow"),
                   fluidPage(
                     box(
                       width=12,
                       h2(tags$b("Stunting")),
                       br(),
                      div(style = "text-align:justify", 
                         p("Stunting adalah masalah gizi kronis akibat kurangnya asupan gizi dalam jangka waktu panjang sehingga mengakibatkan terganggunya pertumbuhan pada anak.
                           Stunting juga menjadi salah satu penyebab tinggi badan anak terhambat, sehingga lebih rendah dibandingkan anak-anak seusianya. Tidak jarang masyarakat menganggap kondisi tubuh pendek merupakan faktor genetika dan tidak ada kaitannya dengan masalah kesehatan. 
                           maka dari itu penjabaran data dengan visualisasi yang tepat dan perhatian pemerintah kepada masyarakat yang spesifik sangatlah penting dalam upaya mengambat adanya stunting."),
                         br()
                     ))
                   ),
                   fluidPage(
                     tabBox(width = 8,
                            title = "Stunting di Indonesia (per Provinsi)",
                            id = "tabset1",
                            side = "right",
                            tabPanel("Pendek", 
                                     plotlyOutput("plottinggi1")
                            ),
                            tabPanel("Sangat Pendek", 
                                     plotlyOutput("plottinggi2")
                            )    
                     ),
                      uiOutput("info")
                     ),
                   fluidPage(
                     box(
                       width=4,
                       h4(tags$b("Asupan Gizi (Penyebab Langsung)")),
                       br(),
                       div(style = "text-align:justify", 
                           p("Salah satu akibat jika seorang anak mengalami kurang gizi terutama protein adalah terkena stunting. Faktor gizi ini merupakan penyebab langsung terjadinya stunting, 
                             terlihat dari grafik yang menunjukkan adanya korelasi hubungan yang positif dari stunting dan indikator gizi."),
                           br()
                       )),
                       box(
                         width=8,
                         plotlyOutput("bandinggizistunt")
                       ),
                   ),
                   fluidPage(
                     box(
                       width=12,
                       h4(tags$b("Indeks Pembangunan Manusia dan Upah Minimum Regional (Penyebab tidak Langsung)")),
                       br(),
                       div(style = "text-align:justify", 
                           p("Faktor pertumbuhan stunting juga dapat dipengaruhi oleh faktor penyebab yang tidak langsung, Diantaranya ada Indeks Pembangunan Manusia dan Upah Minimum Regional, 
                             yang dimana ketika di perbandingan dengan prevalensi stunting juga menunjukkan korelasi cukup kuat hubungan yang negatif dengan Indeks Pembangunan Manusia
                             dan korelasi yang lemah terhadap Upah Minimum Regional."),
                           br()
                       )),
                     box(
                       width=6,
                       plotlyOutput("bandingkemiskinanstunt")
                     ),
                     box(
                       width = 6,
                       plotlyOutput("bandingipmstunt")
                     )
                   ),
                   fluidPage(
                     box(
                       width=12,
                       h4(tags$b("Keterkaitan Gizi dengan Penyebab stunting tidak langsung")),
                       br(),
                       div(style = "text-align:justify", 
                           p("Pada Grafik menunjukkan keterkaitan yang lemah antara gizi - Indeks kemiskinan dan hubungan yang cukup kuat antara gizi dan indikator indeks pembangunan manusia."),
                           br()
                       )),
                     box(
                       width=6,
                       plotlyOutput("bandingkemiskinangizi")
                     ),
                     box(
                       width = 6,
                       plotlyOutput("bandingipmgizi")
                     )
                   )
                   
                   
                  ),
  tabPanel("Sebaran Kasus Stunting",
           leafletOutput("mapstunting",width = "100%",height = 700),
           absolutePanel(
             top="20%",bottom = "20%",left = "1%",width = "15%",
             height = "auto",
             box(
               width = 12,
               setSliderColor("#D7466C", 1),
               sliderInput("tahun",
                           label = "Pilih Tahun",
                           min = 2016,
                           max = 2018,
                           value = 2018,
                           sep = ""
               )
             )
           ),
           absolutePanel(
             id = "controls",
             top = "10%", left = "10%",right = "10%", width = "80%", fixed=FALSE,
             draggable = TRUE, height = "auto",
             uiOutput("judul")
           ),
           absolutePanel(
             id = "controls", class = "panel panel-default",
             bottom = 0, left = "10%", width = "80%", fixed=FALSE,
             draggable = TRUE, style = "min-height:100px",
             column(
               style="padding:1%",
               width=7,
               box(
                 style="background-color:#D7466C;padding:0.2%;text-align:center;color:white",
                 width = "100%",
                 h5("Rata-rata prevalensi Indonesia (pendek dan sangat pendek"),
                 uiOutput("rerataindo")
               ),
               box(
                 style="background-color:#D7466C;padding:0.2%;text-align:center;color:white",
                 width = "100%",
                 h5("Provinsi paling sedikit kasus"),
                 uiOutput("tidak_rawan")
               ),
               box(
                 style="background-color:#D7466C;padding:0.2%;text-align:center;color:white",
                 width = "100%",
                 h5("Provinsi paling banyak kasus"),
                 uiOutput("paling_rawan")
               ),
               
             ),
             column(
               width=5,
               br(),
               h4("Kasus terbanyak di 5 Provinsi:"),
               tableOutput("rank")
              
             ),
           ),
           
  ),
          tabPanel("Perkembangan Kasus Stunting",
                   box(
                     width = 4,
                     h3("Terjadi Peningkatan kasus yang cukup banyak pada tahun 2016 ke tahun 2017 sebanyak 1.5% ."),
                     h3("Di Tahun 2018 terjadi penurunan prevalensi yang menjadi indikator yang bagus dalam hal keberhasilan pemerintah menekan kasus stunting.")
                   ),
                   box(
                     width = 8,
                     plotlyOutput("plottrenstunting")
                   ),
                   box(
                     width = 6,
                     h4("Tabel Peningkatan kasus di tahun 2016-2017"),
                     dataTableOutput("rankpeningkatan")
                   ),
                   box(
                     width = 6,
                     h4("Tabel Penurunan kasus di tahun 2016-2017"),
                     dataTableOutput("rankpenurunan")
                   ),
                   div(style = "text-align:justify", 
                       p("Terlihat dari tabel per provinsi, peningkatan kasus paling banyak terjadi di provinsi Sulawesi Utara yaitu sebanyak 5,09%, dan juga terjadi penurunan terbanyak pada provinsi Yogyakarta sebanyak 1,02%."),
                       br()
                   )
           ),
            
          tabPanel("Estimasi Stunting tahun berikutnya",
                   uiOutput("ratastunt"),
                   uiOutput("ratagizi"),
                   uiOutput("rataipm"),
                   uiOutput("ratamiskin"),
                   
                box(
                  width = 3,
                  selectInput("b_provinsi",
                              label = "Pilih Provinsi",
                              choices = unique(d_tinggi$Provinsi),
                              selected = "ACEH"),
                  uiOutput("perbandigan")
                              
                ),
                box(
                  width = 9,
                  plotlyOutput("bandinggizistunting")
                )    
          ),
          
           navbarMenu("Data Referensi",
                      tabPanel("Tabel Gabungan Prevalensi gizi Balita",
                               h2("Data Tabel Tinggi Balita"),
                               DT::dataTableOutput("tabelgizi")
                      ),
                      tabPanel("Tabel Prevalensi Tinggi Balita",
                               h2("Data Tabel Tinggi Balita"),
                               dataTableOutput("tabeltinggi")
                      ),
                      tabPanel("Tabel Kemiskinan",
                               h2("Data Tabel Kemiskinan"),
                               DT::dataTableOutput("tabelkemiskinan")
                      ),
                      tabPanel("Tabel Indeks Pembangunan Manusia",
                               h2("Data Tabel Indeks Pembangunan Manusia"),
                               dataTableOutput("tabelipm")
                      ),
                      tabPanel("Tabel peta provinsi",
                               h2("Data Tabel Peta Provinsi"),
                               dataTableOutput("tabelpeta")
                      ),
           ),
            tabPanel("Source Code",
                    fluidPage(
                      box(
                        width=12,
                        h3("Download Source di link berikut : "),
                        tags$a(href = "https://github.com/rezkyyayang/pekerjasejahtera/",h3("Link"))
                      )
                    )
                   
            )
)
