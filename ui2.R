# Fungsi dashboardPage() diperuntuhkan untuk membuat ketiga bagian pada Shiny
dashboardPage(
  
  # Fungsi dashboardHeader() adalah bagian untuk membuat header
  dashboardHeader(title = "Persebaran Potensi Stunting Di Indonesia"),
  
  # Fungsi dashboardSidebar() adalah bagian untuk membuat sidebar
  dashboardSidebar(
    #collapsed = TRUE,
    sidebarMenu(
      menuItem(
        text = "Sebaran Keseluruhan",
        tabName="menu1",
        icon =icon("check") 
      ),
      menuItem(
        text = "Sebaran per Provinsi",
        tabName="menu2",
        icon =icon("bars") 
      ),
      menuItem(
        text = "Data Referensi",
        tabName="menu3",
        icon =icon("house") 
      )
      
    )
  ),
  
  # Fungsi dashboardBody() adalah bagian untuk membuat isi body
  dashboardBody(
    #shinyjs::useShinyjs(),
    
    tabItems(
      tabItem(
        style="background-color:white;padding:2%",
        tabName = "menu1",
        fluidRow(
            #title=h1(strong("DASHBOARD DAERAH POTENSI RAWAN STUNTING DI INDONESIA"),align="center")
        ),
        fluidRow(
          column(
            width=2,
            box(
              h5("Daerah Paling Rawan"),
              width = 12
            ),
            box(
              h5("Prevalensi rata gizi rendah paling tinggi"),
              width = 12
            ),
            box(
              h5("Prevalensi rata rendah badan balita paling tinggi"),
              width = 12
            ),
          ),
          column(
            width=10
          ),
          
        )
        
      ),
      tabItem(
        tabName = "menu2",
        fluidRow(
          box(
            width = 12,
            
          ),
        ),
        fluidRow(
          box(
            width = 12,
            
          ),
        )
      ),
      tabItem(
        tabName = "menu3",
        fluidRow(
          box(
            h2("Tabel data Gabungan Gizi"),
            dataTableOutput("tabelgizi"),
            width=12
          ),
          box(
            h2("Tabel data Tinggi"),
            dataTableOutput("tabeltinggi"),
            width=12
          )
        )
      )
    )
  )
)
