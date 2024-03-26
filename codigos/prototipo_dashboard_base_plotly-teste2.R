# Pacotes necessários
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, shinythemes,
  readtext, dplyr, ggplot2, ggrepel, scales, plotly
)

senha_admin <- "senha123"

shinyApp(
  ui = dashboardPage(
    skin = "blue-light",
    # Definições de layout
    header = dashboardHeader(title = "Projeto Tubarão Azul"),
    sidebar =  dashboardSidebar(
      id = "sidebar",
      minified = FALSE,
      collapsed = FALSE,
      width = 250,
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
            width: 680px;
          }
          .graficos-accordion {
            margin: 0 auto;
            width: 80%;
          }
        '))
      ),
      sidebarMenu(
        id = "sidebarMenu",
        menuItem("Apresentação", tabName = "tab1header"),
        menuItem("Distribuição de comprimentos",tabName = "tab2header"),
        menuItem("Desembarques", tabName = "tab3header"),
        menuItem("Distribuição espacial das capturas", tabName = "tab4header"),
        menuItem("Admin", tabName = "tab5header"),
        mainPanel(imageOutput("creditos_img"))
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          "tab1header",
                tabsetPanel(
                  id = "bodyTab",
                  tabPanel("Projeto", value = "tab1body"),
                  tabPanel("Leia-me", value = "tab2body")
                ),
                column(
                  9,
                  uiOutput("accordiontxt_ui")
                )
        ),
        tabItem(
          "tab2header",
                div(class = "graficos-accordion",
                    accordion(
                      id = "accordion2",
                      accordionItem(
                        title = "Visualização dos Dados",
                        status = "primary",
                        collapsed = FALSE,
                        fluidRow(
                          column(6, plotlyOutput("graficoBarra")),
                          column(6, plotlyOutput("graficoRosca"))
                        )
                      )
                    )
                )
        ),
        tabItem("tab3header"),
        tabItem("tab4header"),
        tabItem(
          "tab5header",
                value = "tab5header",
                fluidRow(
                  column(
                    width = 4,
                    offset = 4,
                    passwordInput(
                      inputId = "senha",
                      label = "Senha", 
                      value = ""
                    ),
                    actionButton(
                      inputId = "entrar",
                      label = "Entrar"
                    ),
                    uiOutput("tabelaAdmin")
                  ),
                  uiOutput("mensagemSenha") # Mensagem dinâmica sobre a senha
                  )
                )
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
        )
      )
    ),
    title = "Teste ShinyDashboardPlus"
  ),
  
  server = function(input, output, session) {
    
    conteudo_admin <- reactiveVal(NULL)
    
    observeEvent(input$entrar, {
      if (input$senha == senha_correta) {
        conteudo_admin({
          dataTableOutput("tabela_tub")
        })
        
        output$tabela_tub <- renderDataTable({
          
          dadostub <- dados_tubaroes()
          
          dadostub <- subset(dadostub,
                             Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
          
          if (input$sexo_escolhido == "Macho") {
            dadostub <- subset(dadostub, Sexo == "M")
          } else if (input$sexo_escolhido == "Fêmea") {
            dadostub <- subset(dadostub, Sexo == "F")
          }
          
          if (!is.null(input$entrar) && input$entrar > 0) {
            if (input$senha == senha_admin) {
              dadostub
            }
          }
        })
      } else {
        showModal(modalDialog(
          title = "Erro de login",
          "Senha incorreta. Tente novamente.",
          easyClose = TRUE,
        ))
        conteudo_admin(NULL)
      }
    })
    
    output$tabelaAdmin <- renderUI({
      conteudo_admin()
    })
    
    
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
      # if(input$headerTab == "tab1header") {
        tabsetPanel(
          id = "bodyTab",
          tabPanel("Projeto", value = "tab1body"),
          tabPanel("Leia-me", value = "tab2body")
        )
      # }
    })
    
    # Renderização da imagem
    output$creditos_img <- renderImage({
      list(src = "dados_brutos/ImagemTeste.png",  # Substitua pelo caminho da sua imagem PNG
           contentType = "image/png",
           alt = "Créditos")  # Texto alternativo para acessibilidade
    }, deleteFile = FALSE)
    
    output$accordiontxt_ui <- renderUI({
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
    })
    
    # Renderização do gráfico de barras
    output$graficoBarra <- renderPlotly({
      dadostub <- dados_tubaroes()
      
      dadostub <- subset(dadostub, Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
      
      # Filtra os dados com base na escolha do sexo
      if (input$sexo_escolhido == "Macho") {
        dadostub <- subset(dadostub, Sexo == "M")
      } else if (input$sexo_escolhido == "Fêmea") {
        dadostub <- subset(dadostub, Sexo == "F")
      }
      
      plot_ly(
        data = dadostub,
        x = ~Tamanho,
        type = "histogram",
        histnorm = "percent",
        xbins = list(size = 5),
        marker = list(
          color = "#3C8DBC",
          line = list(
            color = "black",
            width = 1
          )
        ),
        hoverinfo = "text",
        hovertext = ~paste(
          "Intervalo: ",
          cut(Tamanho,
              breaks = seq(min(Tamanho),
                           max(Tamanho) + 5, by = 5),
              include.lowest = TRUE,
              right = FALSE),
          ", ",
          round(prop.table(table(cut(Tamanho,
                                     breaks = seq(min(Tamanho),
                                                  max(Tamanho) + 5, by = 5),
                                     include.lowest = TRUE,
                                     right = FALSE)
          )
          ) * 100,
          2),
          "%"
        )
      ) %>% 
        layout(
          yaxis = list(
            title = "Frequência Relativa (%)",
            tickwidth = 2,  # Espessura da linha
            showgrid = TRUE  # Exibir linhas de grade
          ),
          xaxis = list(
            title = "Comprimento total (cm)"
          )
        )
    })
    
    
    
    # Renderização do gráfico de rosca
    output$graficoRosca <- renderPlotly({
      dadostub <- dados_tubaroes()
      
      dadostub <- subset(
        dadostub,
        Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2]
      )
      
      gender <- dadostub %>%
        count(Sexo)
      
      gender_list <- as.list(gender) # Convertendo o vetor nomeado em uma lista
      
      plot_ly(
        data = gender, # Usando a lista nomeada como entrada
        labels = ~Sexo,
        values = ~n,
        type = "pie",
        hole = 0.6
      )
    })
    
    # Renderização do texto da aba 1
    output$textOut1 <- renderUI({
      # if(input$headerTab == "tab1header" && input$bodyTab == "tab1body") {
      if(input$bodyTab == "tab1body"){
        HTML(paste0("<pre>", pdf_content1$text, "</pre>"))
      }
    })
    
    # Renderização do texto da aba 2
    output$textOut2 <- renderUI({
      # if(input$headerTab == "tab1header" && input$bodyTab == "tab2body") {
      if(input$bodyTab == "tab2body"){
        HTML(paste0("<pre>", pdf_content2$text, "</pre>"))
      }
    })
  }
)
