
#César Ruiz Galicia
#Tarea 1 de Garrido-Juvenal Campos

library(sf)
library(leaflet)
library(htmlwidgets)
library(webshot)
library(dplyr)
library(readr)
library(tidyverse)
library(plotly)

Municipios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson", quiet = TRUE) %>% filter(CVE_ENT == "09")

View(Municipios)

library(readr) 

bd <- library(readr)
carpetas_de_investigacion_pgj_de_la_ciudad_de_mexico <- read_csv("carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico.csv")
View(carpetas_de_investigacion_pgj_de_la_ciudad_de_mexico)

Carpetas_Violacion <- bd %>% 
  filter(alcaldia_hechos == "ALVARO OBREGON"| alcaldia_hechos == "AZCAPOTZALCO" | alcaldia_hechos == "BENITO JUAREZ" | alcaldia_hechos == "COYOACAN" | alcaldia_hechos == "CUAJIMALPA" | alcaldia_hechos == "CUAUHTEMOC" | alcaldia_hechos == "GUSTAVO A MADERO" | alcaldia_hechos == "IZTACALCO" | alcaldia_hechos == "IZTAPALAPA" | alcaldia_hechos == "MAGDALENA CONTRERAS" | alcaldia_hechos == "MIGUEL HIDALGO" | alcaldia_hechos == "MILPA ALTA" | alcaldia_hechos == "TLAHUAC" | alcaldia_hechos == "TLALPAN" | alcaldia_hechos == "VENUSTIANO CARRANZA" | alcaldia_hechos == "XOCHIMILCO", categoria_delito == "VIOLACIÓN") %>% 
  group_by(alcaldia_hechos) %>% 
  count()

Carpetas_Violacion %>% 
  ggplot(aes(x = alcaldia_hechos, y = n))+
  geom_col()+
  coord_flip()

ggplotly(tooltip = "text") %>% 
  config(displayModeBar = F)

#mapa estático
plot(Municipios, max.plot = 1)

Carpetas_Violacion %>% 
  mutate(CVE_MUN = case_when(alcaldia_hechos == "AZCAPOTZALCO" ~ "002", alcaldia_hechos == "ALVARO OBREGON" ~ "010", alcaldia_hechos == "BENITO JUAREZ" ~ "014", alcaldia_hechos == "COYOACAN" ~ "003", alcaldia_hechos == "CUAUHTEMOC" ~ "015", alcaldia_hechos == "GUSTAVO A MADERO" ~ "005", alcaldia_hechos == "IZTACALCO" ~ "006", alcaldia_hechos == "IZTAPALAPA" ~ "007", alcaldia_hechos == "MIGUEL HIDALGO" ~ "016", alcaldia_hechos == "MILPA ALTA" ~ "009", alcaldia_hechos == "TLAHUAC" ~ "011", alcaldia_hechos == "TLALPAN" ~ "012", alcaldia_hechos == "VENUSTIANO CARRANZA" ~ "017", alcaldia_hechos == "XOCHIMILCO" ~ "013"))

write_csv("carpetas_violacion.csv")

Carpetas_Violacion %>% 
  write_csv("carpetas_violacion.csv")


merge(x = Municipios, 
      y= Carpetas_Violacion,
      by.x= "CVE_MUN",
      by.y= "")

mapa <- merge(x= )

