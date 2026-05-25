library(shiny)
library(shinyjs)
DIRETORIO <- "B:/R/previsão de rem. vendedor - amostra/previsão de rem. vendedor - amostra"
setwd(DIRETORIO)
source("predict function - A.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Simulação de remuneração - Vendedor"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "vendedor",
        label = "Nome do vendedor",
        choices = NULL,   # carregado depois
        multiple = FALSE,
        options = list(
          placeholder = "Digite o nome",
          maxOptions = 10,
          openOnFocus = TRUE
        )
      ),
      textInput("dep", "Departamento"),
      #textInput("marca", "Marca"),
      selectizeInput(
        inputId = "unidade",
        label = "Unidade do vendedor",
        choices = NULL,   # carregado depois
        multiple = FALSE,
        options = list(
          placeholder = "Digite a unidade",
          maxOptions = 10,
          openOnFocus = TRUE
        )
      ),
      numericInput("vol", "Volume (carros)", value = 1, min = 1),
      checkboxInput(inputId = "mediavol", label   = "Usar volume médio",value   = FALSE),
      numericInput("rentab", "Rentabilidade (opcional)", value = NA),
      numericInput("fat", "Faturamento (opcional)", value = NA),
      actionButton("simular", "Simular"),
      downloadButton("download_relatorio", "Baixar relatório")
    ),
    
    mainPanel(
      uiOutput("relatorio")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    if (isTRUE(input$mediavol)) {
      disable("vol")
    } else {
      enable("vol")
    }
  })
  
  dados          <- readRDS(paste0(DIRETORIO,"/data/data.rds"))
  unidades <- sort(unique(dados$unidade))
  
  updateSelectizeInput(
    session,
    "unidade",
    choices = unidades,
    server = TRUE
  )
  
  vendedores <- sort(unique(dados$vendedor))
  
  updateSelectizeInput(
    session,
    "vendedor",
    choices = vendedores,
    server = TRUE
  )
  
  observeEvent(input$simular, {
    
    
    tmp <- tempdir()
    addResourcePath("relatorios", tmp)
    
    outfile <- paste0("rel_", session$token, ".html")
    fullpath <- file.path(tmp, outfile)
    
    
    
    rmarkdown::render(
      input = normalizePath("Modelo anonimizado_html.Rmd"), # Removi todos os toupper() dos parametros, como normalizei os dados para o case, não preciso tratar os dados aqui...
      output_file = fullpath,
      params = list(
        vendedor = (input$vendedor),
        vol = input$vol,
        dep = (input$dep),
        unidade = (input$unidade),
        rentab = input$rentab,
        fat = input$fat, 
        mediavol = input$mediavol
      ),
      envir = new.env(parent = globalenv())
    )
    
    output$relatorio <- renderUI({
      tags$iframe(
        src = paste0("relatorios/", outfile),
        width = "100%",
        height = "800px",
        style = "border:none;"
      )
    })
    
  })
  
  output$download_relatorio <- downloadHandler(
    filename = function() {
      paste0(input$vendedor,"_", Sys.Date(), ".html")
    },
    content = function(file) {
      rmarkdown::render(
        "Modelo anonimizado_html.Rmd",
        output_file = file,
        params = list(
          vendedor = (input$vendedor),
          vol = input$vol,
          dep = (input$dep),
          unidade = (input$unidade),
          rentab = input$rentab,
          fat = input$fat, 
          mediavol = input$mediavol
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

shinyApp(ui, server)
