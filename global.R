# Library

library(shiny)
library(shinydashboard)
library(DT) # datatable

options(scipen = 99) # me-non-aktifkan scientific notation
library(dplyr) # koleksi beberapa package R
library(plotly) # plot interaktif
library(glue) # setting tooltip
library(scales) # mengatur skala pada plot
library(lubridate)
library(shinyjs)
library(rsconnect)
library(tidyr)
library(readxl)
library(cellranger)
library(tidyverse)
library(shinythemes)
library(bslib)
library(leaflet)
library(markdown)
library(sf)
library(stringr)
library(ggplot2)
library(readr)
library(rvest)
library(shinyjs)
library(shinyWidgets)

# Read Data
#data tinggi
tinggi_badan_prov <- readxl::read_xlsx("dataset/persentase_Balita_Pendek/persentase_tinggi_balita_prov.xlsx")
tinggi_badan_prov<- tail(tinggi_badan_prov,-2)
tinggi_badan_prov<- head(tinggi_badan_prov,-4)

#IPM
ipm1 <- readxl::read_xlsx("dataset/ipm_2017-2019.xlsx")
ipm1<- tail(ipm1,-1)
ipm1<- head(ipm1,-4)
colnames(ipm1)[c(2,3)]  <- c(2017,2018)
ipm1<- ipm1 %>% select(-...4)
ipm2 <- readxl::read_xlsx("dataset/ipm_2014-2016.xlsx")
ipm2<- tail(ipm2,-1)
ipm2<- head(ipm2,-4)
colnames(ipm2)[c(4)]  <- c(2016)
ipm2 <- ipm2 %>% select(Provinsi,`2016`)
ipm <- left_join(ipm1,ipm2)
ipm_long <- pivot_longer(data = ipm,
                        cols = c("2017","2018","2016"),
                        names_to = "tahun",
                        values_to = "ipm")
ipm_long <- ipm_long %>% 
  mutate(ipm = as.numeric(ipm)) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(Provinsi = case_when(Provinsi %in% "KEP. BANGKA BELITUNG" ~ "BANGKA BELITUNG",
                              Provinsi %in% "KEP. RIAU"~ "KEPULAUAN RIAU",
                              Provinsi %in% "DKI JAKARTA"~ "JAKARTA RAYA",
                              Provinsi %in% "DI YOGYAKARTA"~ "YOGYAKARTA",
                              TRUE ~ Provinsi))

#datagdp kapita
gdp1 <- readxl::read_xlsx("dataset/gdp2017-2019.xlsx")
gdp1<- tail(gdp1,-2)
gdp1<- head(gdp1,-4)
colnames(gdp1)[c(2,3)]  <- c(2017,2018)
gdp1 <- gdp1 %>%  select(Provinsi,`2017`,`2018`)
gdp2 <- readxl::read_xlsx("dataset/gdp2014-2016.xlsx")
gdp2<- tail(gdp2,-2)
gdp2<- head(gdp2,-4)
colnames(gdp2)[c(4)]   <- c(2016)
gdp2 <- gdp2 %>%  select(Provinsi,`2016`)
gdp <- left_join(gdp1,gdp2)
gdp_long <- pivot_longer(data = gdp,
                         cols = c("2017","2018","2016"),
                         names_to = "tahun",
                         values_to = "prevalensi")
gdp_long <- gdp_long %>% 
  mutate(prevalensi = as.numeric(prevalensi)) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(Provinsi = case_when(Provinsi %in% "KEP. BANGKA BELITUNG" ~ "BANGKA BELITUNG",
                              Provinsi %in% "KEP. RIAU"~ "KEPULAUAN RIAU",
                              Provinsi %in% "DKI JAKARTA"~ "JAKARTA RAYA",
                              Provinsi %in% "DI YOGYAKARTA"~ "YOGYAKARTA",
                              TRUE ~ Provinsi))

#kemiskinan
miskin1 <- readxl::read_xlsx("dataset/miskin2016-2017.xlsx")
miskin1 <-head(miskin1,-4)
miskin1 <-tail(miskin1,-3)
miskin1 <-miskin1 %>% 
  rename_with(.cols = 1,.fn = ~ paste("Provinsi")) %>% 
  rename_with(.cols = 2:4,.fn = ~ paste(rep("kota_2016", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 5:7,.fn = ~ paste(rep("kota_2017", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 8:10,.fn = ~ paste(rep("desa_2016", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 11:13,.fn = ~ paste(rep("desa_2017", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 14:16,.fn = ~ paste(rep("jumlah_2016", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 17:19,.fn = ~ paste(rep("jumlah_2017", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) 
miskin_long1 <- pivot_longer(miskin1,
                            cols = c("kota_2016_smt_1","kota_2016_smt_2","kota_2016_tahunan","kota_2017_smt_1","kota_2017_smt_2","kota_2017_tahunan",
                                     "desa_2016_smt_1","desa_2016_smt_2","desa_2016_tahunan","desa_2017_smt_1","desa_2017_smt_2","desa_2017_tahunan",
                                     "jumlah_2016_smt_1","jumlah_2016_smt_2","jumlah_2016_tahunan","jumlah_2017_smt_1","jumlah_2017_smt_2","jumlah_2017_tahunan"),
                            names_to = "kategori",
                            values_to = "kemiskinan") %>% 
                mutate(kemiskinan = as.numeric(kemiskinan))
miskin_long1 <- na.omit(miskin_long1)

miskin_long1 %>%  
  filter(str_detect(kategori,"jumlah"))

miskin2 <- readxl::read_xlsx("dataset/miskin2018-2019.xlsx")
miskin2 <-head(miskin2,-4)
miskin2 <-tail(miskin2,-3)
miskin2 <-miskin2 %>% 
  rename_with(.cols = 1,.fn = ~ paste("Provinsi")) %>% 
  rename_with(.cols = 2:4,.fn = ~ paste(rep("kota_2018", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 5:7,.fn = ~ paste(rep("kota_2019", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 8:10,.fn = ~ paste(rep("desa_2018", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 11:13,.fn = ~ paste(rep("desa_2019", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 14:16,.fn = ~ paste(rep("jumlah_2018", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) %>% 
  rename_with(.cols = 17:19,.fn = ~ paste(rep("jumlah_2019", each = 3), "_", c("smt_1", "smt_2", "tahunan"), sep = "")) 
miskin_long2 <- pivot_longer(miskin2,
                             cols = c("kota_2018_smt_1","kota_2018_smt_2","kota_2018_tahunan","kota_2019_smt_1","kota_2019_smt_2","kota_2019_tahunan",
                                      "desa_2018_smt_1","desa_2018_smt_2","desa_2018_tahunan","desa_2019_smt_1","desa_2019_smt_2","desa_2019_tahunan",
                                      "jumlah_2018_smt_1","jumlah_2018_smt_2","jumlah_2018_tahunan","jumlah_2019_smt_1","jumlah_2019_smt_2","jumlah_2019_tahunan"),
                             names_to = "kategori",
                             values_to = "kemiskinan") %>% 
  mutate(kemiskinan = as.numeric(kemiskinan)) 
miskin_long2 <- na.omit(miskin_long2)
miskin_long2 %>%  
  filter(str_detect(kategori,"jumlah"))
miskin <- rbind.data.frame(miskin_long1,miskin_long2) %>% 
  mutate(daerah = substr(kategori,1,nchar(kategori)-11)) %>% 
  mutate(tahun = substr(kategori,nchar(kategori)-9,nchar(kategori)-6)) %>% 
  mutate(periode = substr(kategori,nchar(kategori)-4,nchar(kategori))) %>% 
  mutate_if(is.character,as.factor) %>% 
  select(-kategori)
#dataupahprovinsi
upah1 <- readxl::read_xlsx("dataset/upah2017-2019.xlsx")
upah1<- tail(upah1,-1)
upah1<- head(upah1,-4)
colnames(upah1)[c(2)]  <- c(2018)
upah1 <- upah1 %>%  select(Provinsi,`2018`)
upah2 <- readxl::read_xlsx("dataset/upah2014-2016.xlsx")
upah2<- tail(upah2,-1)
upah2<- head(upah2,-4)
colnames(upah2)[c(4)]   <- c(2016)
upah2 <- upah2 %>%  select(Provinsi,`2016`) %>% 
  mutate(`2017` = `2016`)
upah <- left_join(upah1,upah2)
upah_long <- pivot_longer(data = upah,
                         cols = c("2018","2016","2017"),
                         names_to = "tahun",
                         values_to = "upah")
upah_long <- upah_long %>% 
  mutate(upah = as.numeric(upah)) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(Provinsi = case_when(Provinsi %in% "KEP. BANGKA BELITUNG" ~ "BANGKA BELITUNG",
                              Provinsi %in% "KEP. RIAU"~ "KEPULAUAN RIAU",
                              Provinsi %in% "DKI JAKARTA"~ "JAKARTA RAYA",
                              Provinsi %in% "DI YOGYAKARTA"~ "YOGYAKARTA",
                              TRUE ~ Provinsi))

#data gizi
gizi_buruk_prov <- readxl::read_xlsx("dataset/prevalensi_Balita_Gizi/Prevalensi_gizi_buruk_prov.xlsx")
gizi_buruk_prov<- tail(gizi_buruk_prov,-2)
gizi_buruk_prov<- head(gizi_buruk_prov,-4)
gizi_kurang_prov <- readxl::read_xlsx("dataset/prevalensi_Balita_Gizi/Prevalensi_gizi_kurang_prov.xlsx")
gizi_kurang_prov<- tail(gizi_kurang_prov,-2)
gizi_kurang_prov<- head(gizi_kurang_prov,-4)
gizi_kekurangan_prov <- readxl::read_xlsx("dataset/prevalensi_Balita_Gizi/Prevalensi_kekurangan_gizi_prov.xlsx")
gizi_kekurangan_prov<- tail(gizi_kekurangan_prov,-2)
gizi_kekurangan_prov<- head(gizi_kekurangan_prov,-4)






#end read data

#clean data

tinggi_badan_prov_clean <- tinggi_badan_prov %>% 
  rename_with(.cols = 1,.fn = ~ paste("Provinsi")) %>% 
  rename_with(.cols = 2:4,.fn = ~ paste(rep("pendek", each = 3), "_", c("2016", "2017", "2018"), sep = "")) %>% 
  rename_with(.cols = 5:7,.fn = ~ paste(rep("sangat_pendek", each = 3), "_", c("2016", "2017", "2018"), sep = ""))

gizi_buruk_prov_clean <- gizi_buruk_prov %>% 
  rename_with(.cols = 1,.fn = ~ paste("Provinsi")) %>% 
  rename_with(.cols = 2:4,.fn = ~ paste(rep("gizi_buruk(0-23)", each = 3), "_", c("2016", "2017", "2018"), sep = "")) %>% 
  rename_with(.cols = 5:7,.fn = ~ paste(rep("gizi_buruk(0-59)", each = 3), "_", c("2016", "2017", "2018"), sep = ""))


gizi_kurang_prov_clean <- gizi_buruk_prov %>% 
  rename_with(.cols = 1,.fn = ~ paste("Provinsi")) %>% 
  rename_with(.cols = 2:4,.fn = ~ paste(rep("gizi_kurang(0-23)", each = 3), "_", c("2016", "2017", "2018"), sep = "")) %>% 
  rename_with(.cols = 5:7,.fn = ~ paste(rep("gizi_kurang(0-59)", each = 3), "_", c("2016", "2017", "2018"), sep = ""))

gizi_kekurangan_prov_clean <- gizi_kekurangan_prov %>% 
  rename_with(.cols = 1,.fn = ~ paste("Provinsi")) %>% 
  rename_with(.cols = 2:4,.fn = ~ paste(rep("gizi_kekurangan(0-23)", each = 3), "_", c("2016", "2017", "2018"), sep = "")) %>% 
  rename_with(.cols = 5:7,.fn = ~ paste(rep("gizi_kekurangan(0-59)", each = 3), "_", c("2016", "2017", "2018"), sep = ""))
#end clean data

#convert long format
long_tinggi_badan_prov_clean<- pivot_longer(data = tinggi_badan_prov_clean,
                                    cols = c("pendek_2016","pendek_2017","pendek_2018","sangat_pendek_2016","sangat_pendek_2017","sangat_pendek_2018"),
                                    names_to = "kategori",
                                    values_to = "prevalensi")
long_gizi_buruk_prov_clean <- pivot_longer(data = gizi_buruk_prov_clean,
                                           cols = c("gizi_buruk(0-23)_2016","gizi_buruk(0-23)_2017","gizi_buruk(0-23)_2018","gizi_buruk(0-59)_2016","gizi_buruk(0-59)_2017","gizi_buruk(0-59)_2018"),
                                           names_to = "kategori",
                                           values_to = "prevalensi")
long_gizi_kurang_prov_clean <- pivot_longer(data = gizi_kurang_prov_clean,
                                            cols = c("gizi_kurang(0-23)_2016","gizi_kurang(0-23)_2017","gizi_kurang(0-23)_2018","gizi_kurang(0-59)_2016","gizi_kurang(0-59)_2017","gizi_kurang(0-59)_2018"),
                                            names_to = "kategori",
                                            values_to = "prevalensi")
long_gizi_kekurangan_prov_clean <- pivot_longer(data = gizi_kekurangan_prov_clean,
                                                cols = c("gizi_kekurangan(0-23)_2016","gizi_kekurangan(0-23)_2017","gizi_kekurangan(0-23)_2018","gizi_kekurangan(0-59)_2016","gizi_kekurangan(0-59)_2017","gizi_kekurangan(0-59)_2018"),
                                                names_to = "kategori",
                                                values_to = "prevalensi")
#end convert long

#modify data



#tinggi
long_tinggi_badan_prov_clean<-long_tinggi_badan_prov_clean %>% 
  mutate(tinggi = substr(kategori,1,nchar(kategori)-5)) %>% 
  mutate(prevalensi = as.numeric(prevalensi)) %>% 
  mutate(tahun = as.numeric(substr(kategori,nchar(kategori)-3,nchar(kategori)))) %>% 
  mutate_if(is.character,as.factor)
#gizi
long_gizi_kurang_prov_clean<-long_gizi_kurang_prov_clean %>% 
  mutate(tahun = as.numeric(substr(kategori,nchar(kategori)-3,nchar(kategori)))) %>%
  mutate(umur= as.numeric(substr(kategori,nchar(kategori)-7,nchar(kategori)-6))) %>% 
  mutate(prevalensi = as.numeric(prevalensi)) %>% 
  mutate(gizi = substr(kategori,6,nchar(kategori)-11)) %>% 
  mutate_if(is.character,as.factor) %>% 
  select(-kategori)

long_gizi_buruk_prov_clean<-long_gizi_buruk_prov_clean %>% 
  mutate(tahun = as.numeric(substr(kategori,nchar(kategori)-3,nchar(kategori)))) %>%
  mutate(umur= as.numeric(substr(kategori,nchar(kategori)-7,nchar(kategori)-6))) %>% 
  mutate(prevalensi = as.numeric(prevalensi)) %>% 
  mutate(gizi = substr(kategori,6,nchar(kategori)-11)) %>% 
  mutate_if(is.character,as.factor) %>% 
  select(-kategori)

long_gizi_kekurangan_prov_clean<- long_gizi_kekurangan_prov_clean %>% 
  mutate(tahun = as.numeric(substr(kategori,nchar(kategori)-3,nchar(kategori)))) %>%
  mutate(umur= as.numeric(substr(kategori,nchar(kategori)-7,nchar(kategori)-6))) %>% 
  mutate(prevalensi = as.numeric(prevalensi)) %>% 
  mutate(gizi = substr(kategori,6,nchar(kategori)-11)) %>% 
  mutate_if(is.character,as.factor) %>% 
  select(-kategori)
#end modifi

#gizi gabungan
accum_gizi<-full_join(long_gizi_buruk_prov_clean,long_gizi_kekurangan_prov_clean)
#datamap
map_indo <- read_sf("indonesia.geojson")
map_indo$state

#datafinal
d_gizi<- full_join(accum_gizi,long_gizi_kurang_prov_clean)
d_tinggi<- long_tinggi_badan_prov_clean %>% 
  select(-kategori)
d_gizi %>% 
  filter(tahun=="2016") %>% 
  group_by(Provinsi) %>% 
  summarise(prevalensi_tot=mean(prevalensi)) %>% 
  arrange(-prevalensi_tot)


#join with map
d_gizi<-d_gizi %>% 
  mutate(state = str_to_title(d_gizi$Provinsi)) 
d_tinggi<- d_tinggi %>% 
  mutate(state=str_to_title(Provinsi))

gabung1<- d_gizi %>% 
  group_by(Provinsi,tahun) %>% 
  summarise(prevalensi_gizi=mean(prevalensi))
gabung2<- d_tinggi %>% 
  group_by(Provinsi,tahun) %>% 
  summarise(prevalensi_tinggi = mean(prevalensi))

data_gabung<-left_join(gabung1,gabung2)
data_gabung
data_gabung<- data_gabung %>% 
  mutate(prevalensi = (prevalensi_gizi+prevalensi_tinggi)/2) %>% 
  mutate(state=str_to_title(Provinsi))


