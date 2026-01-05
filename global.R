options(java.parameters = "-Xmx512m")

library(shiny)
library(mapview)
library(r5r)
library(sf)
library(data.table)
library(dplyr)
library(leaflet)
library(here)

caminho_projeto <- getwd()
caminho_arquivo <- file.path(caminho_projeto, "r5r_regiao")

dados_ufba <- st_read("edif_ufba.gpkg", quiet = TRUE)
superficie <- st_point_on_surface(dados_ufba)

pontos_r5r <- superficie %>%
  mutate(
    id = paste0("loc_", row_number()), 
    name = ifelse(is.na(name) | name == "", paste("Local", row_number()), name),
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_set_geometry(NULL) %>%
  select(id, name, lat, lon)

r5r_core <- setup_r5(data_path = caminho_arquivo, verbose = FALSE)
rotas <- detailed_itineraries(
  r5r_network = r5r_core,
  origins = pontos_r5r,
  destinations = pontos_r5r, 
  mode = "WALK",             
  shortest_path = TRUE,      
  all_to_all = TRUE          
)  

rotas_nome <- rotas %>%
  left_join(pontos_r5r %>% select(id, nome_origem = name), by = c("from_id" = "id")) %>%
  left_join(pontos_r5r %>% select(id, nome_destino = name), by = c("to_id" = "id"))
