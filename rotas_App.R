library(shiny)
library(shinyjs)
library(googlesheets4)

caminho_ <- "rotaspeufba.json"
gs4_auth(path = caminho_)
URL_PLANILHA <- "16_UyvTv7CYSLYQs-0ZnoB1xuHK9oZ7s8vQvRnc07yWc"

#interface
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Validação de Rotas a Pé - UFBA"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Seleção de Rota"),
      selectizeInput("origem", "Origem:", choices = pontos_r5r$name),
      selectizeInput("destino", "Destino:", choices = pontos_r5r$name),
      
      actionButton("btn_calcular", "Gerar Rota", class = "btn-primary", width = "100%"),
      hr(),
      uiOutput("ui_feedback")
    ),
    
    mainPanel(
      leafletOutput("mapa", height = "85vh")
    )
  )
)

#server

server <- function(input, output, session) {
  
  dados_rota <- reactiveValues(rota_sf = NULL, distancia = NULL, tempo = NULL)
  
  observeEvent(c(input$origem, input$destino), {
    dados_rota$rota_sf <- NULL 
  })
  
  observeEvent(input$btn_calcular, {
    req(input$origem, input$destino)
    
    if(input$origem == input$destino) {
      showNotification("Origem e Destino devem ser diferentes.", type = "warning")
      return()
    }
    
    pt_origem <- pontos_r5r %>% filter(name == input$origem)
    pt_destino <- pontos_r5r %>% filter(name == input$destino)
    
    id_notif <- showNotification("Calculando rota...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id_notif), add = TRUE)
    
    tryCatch({
      rota <- detailed_itineraries(
        r5r_core = r5r_core,
        origins = pt_origem,
        destinations = pt_destino,
        mode = "WALK",
        shortest_path = TRUE,
        verbose = FALSE
      )
      
      if(nrow(rota) == 0) {
        showNotification("Nenhuma rota encontrada.", type = "error")
        dados_rota$rota_sf <- NULL
      } else {
        dados_rota$tempo <- round(rota$total_duration, 1)
        dados_rota$distancia <- round(rota$distance, 0)
        dados_rota$rota_sf <- st_as_sf(rota, crs = 4326)
      }
      
    }, error = function(e) {
      showNotification(paste("Erro:", e$message), type = "error")
    })
  })
  

  output$mapa <- renderLeaflet({
    if (is.null(dados_rota$rota_sf)) {
      pontos_sf <- st_as_sf(pontos_r5r, coords = c("lon", "lat"), crs = 4326)
      m <- mapview(pontos_sf, zcol = NULL, color = "black", col.regions = "blue", 
                   layer.name = "Locais UFBA", label = "name", map.types = "Esri.WorldImagery")
      return(m@map)
    } else {
      # Mapa com rota
      pts_od <- bind_rows(
        pontos_r5r %>% filter(name == input$origem) %>% mutate(tipo = "Origem"),
        pontos_r5r %>% filter(name == input$destino) %>% mutate(tipo = "Destino")
      ) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
      
      m1 <- mapview(dados_rota$rota_sf, color = "yellow", lwd = 5, layer.name = "Rota", map.types = "Esri.WorldImagery")
      m2 <- mapview(pts_od, zcol = "tipo", col.regions = c("green", "red"), layer.name = "O/D")
      return((m1 + m2)@map)
    }
  })
  
  output$ui_feedback <- renderUI({
    req(dados_rota$rota_sf)
    tagList(
      wellPanel(
        h4("Validar Rota"),
        p(strong("Tempo estimado:"), dados_rota$tempo, "min"),
        radioButtons("validacao", "Rota ideal?", choices = c("Sim" = "sim", "Não" = "nao"), inline = TRUE),
        conditionalPanel(condition = "input.validacao == 'nao'", textAreaInput("justificativa", "Sugira um caminho mais curto:", rows = 2)),
        actionButton("btn_salvar", "Salvar", class = "btn-success")
      )
    )
  })
  
  observeEvent(input$btn_salvar, {
    shinyjs::disable("btn_salvar")
    
    novo_registro <- data.frame(
      origem = input$origem,
      destino = input$destino,
      tempo_r5r = dados_rota$tempo,
      distancia_r5r = dados_rota$distancia,
      rota_ideal = input$validacao,
      sugestao = ifelse(input$validacao == "nao", input$justificativa, NA)
    )
    
    tryCatch({
      sheet_append(ss = URL_PLANILHA, data = novo_registro)
      showNotification("Salvo no Google Sheets!", type = "message")
    }, error = function(e) {
      showNotification("Erro ao salvar no Google Drive.", type = "error")
      shinyjs::enable("btn_salvar")
      print(e)
    })
  })
}

shinyApp(ui, server)
