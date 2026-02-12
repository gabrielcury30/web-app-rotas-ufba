library(shiny)
library(shinyjs)
library(googlesheets4)
library(leaflet)
library(mapview)
library(leaflet.extras)
library(sf)    
library(dplyr) 
library(sf)
library(data.table)
library(here)

rotas_base <- readRDS("rotas_entre_pafs.rds")
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

nomes_ordenados <- sort(pontos_r5r$name)

if(file.exists("rotaspeufba.json")) {
  gs4_auth(path = "rotaspeufba.json")
}

URL_PLANILHA <- "16_UyvTv7CYSLYQs-0ZnoB1xuHK9oZ7s8vQvRnc07yWc"

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .info-box { background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 5px solid #007bff; }
    .warn-box { border-left-color: #ffc107; }
    .success-box { background-color: #d4edda; color: #155724; padding: 15px; border-radius: 5px; border: 1px solid #c3e6cb; }
  "))),
  
  titlePanel("Rotas UFBA - Caminhabilidade"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Planejar Caminhada"),
      
      div(style="display: flex; align-items: flex-end;",
          div(style="flex-grow: 1;", selectizeInput("origem", "Origem:", choices = nomes_ordenados)),
          div(style="padding-bottom: 15px; padding-left: 5px;", 
              actionButton("btn_inverter", "", icon = icon("exchange-alt"), title = "Inverter sentidos"))
      ),
      selectizeInput("destino", "Destino:", choices = nomes_ordenados),
      
      actionButton("btn_calcular", "Gerar Rota", class = "btn-primary", width = "100%", icon = icon("walking")),
      hr(),
      uiOutput("ui_feedback")
    ),
    
    mainPanel(
      leafletOutput("mapa", height = "85vh")
    )
  )
)

server <- function(input, output, session) {
  
  dados_rota <- reactiveValues(rota_sf = NULL, stats = NULL)
  # Variável para controlar se o feedback já foi enviado para a rota atual
  feedback_enviado <- reactiveVal(FALSE)
  
  observeEvent(input$btn_inverter, {
    origem_atual <- input$origem
    destino_atual <- input$destino
    updateSelectizeInput(session, "origem", selected = destino_atual)
    updateSelectizeInput(session, "destino", selected = origem_atual)
  })
  
  observeEvent(c(input$origem, input$destino), {
    dados_rota$rota_sf <- NULL 
  })
  
  observeEvent(input$btn_calcular, {
    req(input$origem, input$destino)
    
    # Reseta o estado do feedback para permitir novo envio se a rota mudar
    feedback_enviado(FALSE)
    
    if(input$origem == input$destino) {
      showNotification("Origem e Destino são iguais.", type = "warning")
      return()
    }
    
    pt_origem <- pontos_r5r %>% filter(name == input$origem)
    pt_destino <- pontos_r5r %>% filter(name == input$destino)
    
    id_notif <- showNotification("Calculando melhor caminho...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id_notif), add = TRUE)
    
    tryCatch({
      rota <- rotas_base %>% 
        filter(nome_origem == input$origem, nome_destino == input$destino) 
      
      if(nrow(rota) == 0) {
        showNotification("Nenhuma rota encontrada entre estes pontos.", type = "error")
        dados_rota$rota_sf <- NULL
      } else {
        dados_rota$stats <- list(
          tempo = round(rota$total_duration, 0),
          distancia = round(rota$distance, 0),
          subida = if("elevation_gain" %in% names(rota)) round(rota$elevation_gain, 0) else 0
        )
        dados_rota$rota_sf <- st_as_sf(rota, crs = 4326)
      }
      
    }, error = function(e) {
      showNotification(paste("Erro ao recuperar rota:", e$message), type = "error")
    })
  })
  
  mapas_base <- c("Esri.WorldImagery", "CartoDB.Positron", "OpenStreetMap")
  
  output$mapa <- renderLeaflet({
    m_final <- NULL
    
    if (is.null(dados_rota$rota_sf)) {
      pontos_sf <- st_as_sf(pontos_r5r, coords = c("lon", "lat"), crs = 4326)
      m <- mapview(pontos_sf, zcol = NULL, color = "white", col.regions = "#007bff", 
                   cex = 5, layer.name = "Locais UFBA", label = "name", 
                   map.types = mapas_base)
      
      m_final <- m@map
      
    } else {
      pts_od <- bind_rows(
        pontos_r5r %>% filter(name == input$origem) %>% mutate(tipo = "Origem"),
        pontos_r5r %>% filter(name == input$destino) %>% mutate(tipo = "Destino")
      ) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
      
      m1 <- mapview(dados_rota$rota_sf, color = "yellow", lwd = 6, layer.name = "Rota Sugerida", map.types = mapas_base)
      m2 <- mapview(pts_od, zcol = "tipo", col.regions = c("red", "green"), layer.name = "Início/Fim")
      
      m_final <- (m1 + m2)@map
    }
    
    m_final %>%
      addControlGPS(
        options = gpsOptions(
          position = "topleft", 
          activate = FALSE,
          autoCenter = TRUE, 
          maxZoom = 20, 
          setView = TRUE
        )
      )
  })
  
  output$ui_feedback <- renderUI({
    req(dados_rota$rota_sf)
    stats <- dados_rota$stats
    
    # Bloco de estatísticas (sempre visível se houver rota)
    stats_box <- div(class = "info-box",
                     h4(icon("info-circle"), "Detalhes da Rota"),
                     p(strong("Tempo:"), stats$tempo, "min"),
                     p(strong("Distância:"), stats$distancia, "metros"),
                     if(stats$subida > 5) p(strong("Subida acumulada:"), stats$subida, "metros") else NULL
    )
    
    # Lógica de exibição condicional: Formulário OU Mensagem de Sucesso
    if (feedback_enviado()) {
      # Mensagem de agradecimento
      interaction_area <- div(class = "success-box",
                              h4("Obrigado pela sua participação!"),
                              p("Se quiser testar uma nova rota, é só produzi-la alterando a origem e/ou destino acima.")
      )
    } else {
      # Formulário de validação
      interaction_area <- wellPanel(
        h5("Essa rota faz sentido?"),
        radioButtons("validacao", label = NULL, choices = c("Sim" = "sim", "Não" = "nao"), inline = TRUE),
        conditionalPanel(
          condition = "input.validacao == 'nao'", 
          textAreaInput("justificativa", "Justifique ou sugira outra rota", rows = 3, placeholder = "Ex: Esse portão vive fechado...")
        ),
        actionButton("btn_salvar", "Enviar Validação", class = "btn-success", width = "100%")
      )
    }
    
    tagList(stats_box, br(), interaction_area)
  })
  
  observeEvent(input$btn_salvar, {
    shinyjs::disable("btn_salvar")
    
    novo_registro <- data.frame(
      data_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      origem = input$origem,
      destino = input$destino,
      tempo_calc = dados_rota$stats$tempo,
      distancia_calc = dados_rota$stats$distancia,
      avaliacao = input$validacao,
      comentario = ifelse(input$validacao == "nao", input$justificativa, NA)
    )
    
    tryCatch({
      sheet_append(ss = URL_PLANILHA, data = novo_registro)
      showNotification("Obrigado!", type = "message")
      
      # Marca que o feedback foi enviado com sucesso, acionando a troca de UI
      feedback_enviado(TRUE)
      
      # Não precisa dar enable no botão aqui porque o botão vai sumir da tela
    }, error = function(e) {
      showNotification("Erro ao salvar. Verifique a conexão.", type = "error")
      shinyjs::enable("btn_salvar") # Reabilita apenas se der erro, para tentar de novo
    })
  })
}

shinyApp(ui, server)