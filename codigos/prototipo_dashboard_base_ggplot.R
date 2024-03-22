# Pacotes necessários
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, shinythemes,
  readtext, dplyr, ggplot2, ggrepel, scales
)

shinyApp(
  ui = dashboardPage(
    skin = "blue-light",
    # Definições de layout
    header = dashboardHeader(title = "Projeto Tubarão Azul"),
    sidebar =  dashboardSidebar(
      width = 250,
      id = "sidebar",
      minified = FALSE,
      collapsed = FALSE,
      tags$head(
        tags$style(HTML('
          /* Ajuste o tamanho dos títulos das abas dentro do sidebarPanel */
          .custom-sidebar .nav-tabs > li > a {
            width: 240px; /* Largura fixa para os títulos das abas */
            text-align: center; /* Centraliza o texto */
          }
          /* Estilo para alterar a cor do texto do sidebar para preto */
          #sidebar .sidebar-menu li a,
          #sidebar .sidebar-menu li a:hover,
          #sidebar .sidebar-menu label {
            color: #000000 !important; /* Cor preta */
          }
          .texto-accordion {
            display: inline-block;
            margin: 10px auto;
          }
          .texto-accordion .accordion-title {
            text-align: center; /* Centraliza o texto apenas nos títulos */
          }
          .graficos-accordion {
            margin: 0 auto;
          }
        '))
      ),
      sidebarMenu(
        id = "sidebarMenu",
        sidebarLayout(
          sidebarPanel(
            width = 250,class = "custom-sidebar",
            tabsetPanel(
              id = "headerTab",
              tabPanel("Apresentação", value = "tab1header"),
              tabPanel("Distribuição de comprimentos", value = "tab2header"),
              tabPanel("Desembarques", value = "tab3header"),
              tabPanel("Distribuição espacial das capturas", value = "tab4header")
            ),
            imageOutput("creditos_img")
          ),
          mainPanel()
        )
      )
    ),
    body = dashboardBody(
      uiOutput("tabset_ui"),
      fluidRow(
        column(9, uiOutput("accordion_ui"))
      )
    ),
    controlbar = dashboardControlbar(
      collapsed = F,
      id = "controlbar",
      controlbarMenu(
        id = "controlbarMenu",
        controlbarItem(
          "Opções",
          sliderInput(
            "intervalo_anos",
            "Intervalo de Anos:",
            min = 2018,
            max = 2023,
            value = c(2018,2023),
            step = 1,
            animate = animationOptions(
              playButton = icon("play"),
              pauseButton = icon("pause")
            )
          ),
          radioButtons(
            "sexo_escolhido", 
            "Escolha o Sexo:",
            choices = c("Todos", "Macho", "Fêmea"),
            selected = "Todos"
          ),
          checkboxGroupInput(
            "species", "Seletor de Espécies:",
            c("Albacora bandolim","Albacora branca","Albacora laje",
              "Meca", "Outros"),
            selected = c("Albacora bandolim","Albacora branca",
                         "Albacora laje", "Meca","Outros")
          )
        )#,
        # controlbarItem(
        #   "Tema",
        #   "Bem-Vindo ao Seletor de Tema",
        #   skinSelector()
        # )
      )
    ),
    title = "Teste ShinyDashboardPlus"
  ),
  
  server = function(input, output, session) {
    # Leitura dos dados
    dados_tubaroes <- reactive({
      read.table("dados_brutos/dados_tubaroes_criados.csv",
                 header = TRUE, sep = ";", dec = ",")
    })
    
    # Leitura dos arquivos PDF
    pdf_content1 <- readtext("dados_brutos/testepdf.pdf")
    pdf_content2 <- readtext("dados_brutos/leiame.pdf")
    
    # Geração dinâmica dos painéis de abas
    output$tabset_ui <- renderUI({
      if(input$headerTab == "tab1header") {
        tabsetPanel(
          id = "bodyTab",
          tabPanel("Projeto", value = "tab1body"),
          tabPanel("Leia-me", value = "tab2body")
        )
      }
    })
    
    # Renderização da imagem
    output$creditos_img <- renderImage({
      list(src = "dados_brutos/ImagemTeste.png",  # Substitua pelo caminho da sua imagem PNG
           contentType = "image/png",
           alt = "Créditos")  # Texto alternativo para acessibilidade
    }, deleteFile = FALSE)
    
    # Geração dinâmica dos accordions
    output$accordion_ui <- renderUI({
      if(input$headerTab == "tab1header") {
        div(class = "texto-accordion",
            accordion(
              id = "accordion1",
              accordionItem(
                title = "Visualização de Texto",
                status = "primary",
                collapsed = FALSE,
                if(input$bodyTab == "tab1body") {
                  uiOutput("textOut1")
                } else {
                  uiOutput("textOut2")
                }
              )
            )
        )
      } else if(input$headerTab == "tab2header") {
        div(class = "graficos-accordion",
            accordion(
              id = "accordion2",
              accordionItem(
                title = "Visualização dos Dados",
                status = "primary",
                collapsed = FALSE,
                fluidRow(
                  column(6, plotOutput("graficoBarra")),
                  column(6, plotOutput("graficoRosca"))
                )
              )
            )
        )
      }
    })
    
    # Renderização do gráfico de barras
    output$graficoBarra <- renderPlot({
      dadostub <- dados_tubaroes()
      
      dadostub <- subset(dadostub, Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
      
      # Filtra os dados com base na escolha do sexo
      if (input$sexo_escolhido == "Macho") {
        dadostub <- subset(dadostub, Sexo == "M")
      } else if (input$sexo_escolhido == "Fêmea") {
        dadostub <- subset(dadostub, Sexo == "F")
      }
      
      # Cria o gráfico ggplot
      ggplot(dadostub, aes(x = Tamanho)) +
        geom_histogram(binwidth = 10, fill = "blue", color = "black") +
        labs(title = "Distribuição de Tamanhos de Tubarões",
             x = "Tamanho (cm)",
             y = "Frequência (%)") +
        scale_y_continuous(labels = scales::label_percent(scale = 0.01))
    })
    
    # Renderização do gráfico de rosca
    output$graficoRosca <- renderPlot({
      dadostub <- dados_tubaroes()
      
      dadostub <- subset(
        dadostub,
        Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2]
        )
      
      gender <- dadostub %>%
        count(Sexo)
      
      gender <- gender %>%
        mutate(porc = n / sum(n) * 100)
      
      gender$ymax <- cumsum(gender$porc)
      gender$ymin <- c(0, head(gender$ymax, n=1))
      
      ggplot(gender, aes(ymax=ymax, ymin=ymin, xmax = 4, xmin = 3, fill = Sexo)) +
        geom_rect() +
        coord_polar(theta = "y") +
        xlim(c(1,4)) +
        theme_void()
    })
    
    # Renderização do texto da aba 1
    output$textOut1 <- renderUI({
      if(input$headerTab == "tab1header" && input$bodyTab == "tab1body") {
        HTML(paste0("<pre>", pdf_content1$text, "</pre>"))
      }
    })
    
    # Renderização do texto da aba 2
    output$textOut2 <- renderUI({
      if(input$headerTab == "tab1header" && input$bodyTab == "tab2body") {
        HTML(paste0("<pre>", pdf_content2$text, "</pre>"))
      }
    })
  }
)
