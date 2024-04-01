# Pacotes necessários
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, htmltools,
  readtext, dplyr, ggplot2, ggrepel, scales, plotly
)

# separar_data <- function(dados) {
#   dados <- mutate(dados,
#                   Ano = year(as.Date(Data)),
#                   Mes = month(as.Date(Data)),
#                   Dia = day(as.Date(Data)))
#   return(dados)
# }

capturaMediaMes <- function(dados) {
  dados$Mes <- format(dados$Data, "%Y-%m")
  media_por_mes <- dados[, .(Captura_Media = mean(Tonelada)), by = Mes]
  return(media_por_mes)
}

senha_admin <- "senha123"

shinyApp(
  ui = dashboardPage(
    skin = "blue",
    header = dashboardHeader(
      title = tags$a(href = "https://lrpdc.shinyapps.io/proj_tubarao_azul/",
                     "Projeto Tubarão Azul", class = "logo"),
      # title = "Projeto Tubarão Azul",
      controlbarIcon = icon("sliders"),
      dropdownMenu(
        type = "notifications",
        icon = icon("bell"),
        notificationItem(
          text = "Nova embarcação ocorrerá dia 20/03.",
          href = "https://lrpdc.shinyapps.io/proj_tubarao_azul/",
          icon = icon("ship")
          ),
        notificationItem(
          text = "Nova embarcação ocorrerá dia 17/04.",
          icon = icon("ferry"),
          )
        )
      ),
    sidebar =  dashboardSidebar(
      id = "sidebar",
      minified = T,
      collapsed = FALSE,
      width = 250,
      tags$head(
        tags$style(HTML('
          /* Ajuste o tamanho dos títulos das abas dentro do sidebarPanel */
          .custom-sidebar .nav-tabs > li > a {
            width: 240px; /* Largura fixa para os títulos das abas */
            text-align: center; /* Centraliza o texto */
          }
          # /* Estilo para alterar a cor do texto do sidebar para preto */
          # #sidebar .sidebar-menu li a,
          # #sidebar .sidebar-menu li a:hover,
          # #sidebar .sidebar-menu label {
          #   color: #000000 !important; /* Cor preta */
          # }
          .texto-accordion {
            display: inline-block;
            # margin: auto auto;
            padding: 10px;
            # width: 1450px;
          }
          .texto-accordion .accordion-title {
            text-align: center; /* Centraliza o texto apenas nos títulos */
            width: 680px;
          }
          .graficos-accordion {
            margin: 0 auto;
            # width: 80%;
            width: 100%;
          }
        '))
      ),
      sidebarMenu(
        id = "sidebarMenu",
        menuItem("Apresentação",
                 icon = icon("house"),
                 menuSubItem(
                   "Projeto",
                   tabName = "tab1body",
                   icon = icon("r-project")
                   ),
                 menuSubItem(
                   "Leia-me",
                   tabName = "tab2body",
                   icon = icon("readme")
                   )
                 ),
        menuItem(
          "Distribuição de comprimentos",
          tabName = "tab2header",
          icon = icon("chart-simple")
          ),
        menuItem(
          "Desembarques",
          tabName = "tab3header",
          icon = icon("chart-area")
          ),
        menuItem(
          "Distribuição espacial das capturas",
          tabName = "tab4header",
          icon = icon("earth-americas")
          ),
        menuItem(
          "Admin",
          tabName = "tab5header",
          icon = icon("user-tie")
          # icon = icon("user-shield")
          ),
        imageOutput("creditos_img")
      )
    ),
    body = dashboardBody(
      tags$head(
        tags$style(HTML('
        /* Mantém a cor padrão do título */
        .logo {
          color: #555;
        }
      '))
      ),
      tabItems(
        tabItem(
          "tab1body",
          div(class = "texto-accordion",
              accordion(
                id = "accordion1",
                accordionItem(
                  title = "Visualização de Texto",
                  status = "primary",
                  collapsed = FALSE,
                  uiOutput("textOut1")
                )
              )
          )
        ),
        tabItem(
          "tab2body",
          div(class = "texto-accordion",
              accordion(
                id = "accordion2",
                accordionItem(
                  title = "Visualização de Texto",
                  status = "primary",
                  collapsed = FALSE,
                  uiOutput("textOut2")
                )
              )
          )
        ),
        tabItem(
          "tab2header",
          div(class = "graficos-accordion",
              box(
                solidHeader = T,
                title = "Histograma",
                status = "primary",
                plotlyOutput("graficoBarra")
                ),
              box(
                solidHeader = T,
                title = "Gráfico de Rosca",
                status = "primary",
                plotlyOutput("graficoRosca")
                )
              )
          ),
        tabItem(
          "tab3header",
          div(
            class = "graficos-accordion",
            fluidRow(
              box(
                solidHeader = T,
                title = "Captura Média por Viagem",
                status = "primary",
                plotlyOutput("graficoCaptura")
              )
            ),
            fluidRow(
              box(
                solidHeader = T,
                title = "Desembarque Total (ton)",
                status = "primary"#,
                # plotlyOutput("graficoDesembarque")
              )
            ),
            fluidRow(
              box(
                solidHeader = T,
                title = "Composição de espécies",
                status = "primary"#,
                # plotlyOutput("graficoEspecies")
                )
              )
            )
          ),
        tabItem("tab4header"),
        tabItem(
          "tab5header",
          value = "tab5header",
          fluidRow(
            column(
              width = 4,
              offset = 4,
              uiOutput("senhaAdm"),
              uiOutput("entrarAdm")
              
            )
          ),
          tags$head(
            tags$style(HTML("
            
                      .dataTables_wrapper {
                        height: 100vh !important;
                        width: 80vw !important;
                        padding: 10px;
                      }
                    "))
          ),
          fluidRow(
            column(
              width = 12,
              uiOutput("tabelaAdm")
              )
            )
          )
        )
      ),
    controlbar = dashboardControlbar(
      overlay = F,
      collapsed = F,
      skin = "dark",
      id = "controlbar",
      controlbarMenu(
        id = "controlbarMenu",
        controlbarItem(
          "Opções",
          icon = icon("gear"),
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
          ),
          actionButton("reset","Reiniciar Valores",icon = icon("repeat"))
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    
    dados_capturas <- reactive({
      read.table("dados_brutos/tabela_dados_ficticios.csv",
                 header = TRUE, sep = ";", dec = ",")
    })
    
    output$graficoCaptura <- renderPlotly({
      dadosSeparados <- dados_capturas()
      
      
      # dadosSeparados <- separar_data(dados_capturas_teste)
      
      dadosSeparados <- subset(
        dadosSeparados,
        Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
      
      # Filtra os dados com base na escolha do sexo
      if (input$sexo_escolhido == "Macho") {
        dadosSeparados <- subset(dadosSeparados, Sexo == "M")
      } else if (input$sexo_escolhido == "Fêmea") {
        dadosSeparados <- subset(dadosSeparados, Sexo == "F")
      }
      
      CapturasMediasPorMes <- dadosSeparados %>%
        group_by(Especie, Ano, Mes) %>%
        summarise(Media_Toneladas = mean(Toneladas)) %>%
        group_by(Especie, Mes) %>%
        summarise(Media_Toneladas_por_Mes = mean(Media_Toneladas)) %>%
        mutate(MediaTonMes = round(Media_Toneladas_por_Mes, 2)) %>%
        select(-Media_Toneladas_por_Mes)
      
      nomes_meses <- c(
        "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
        "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
      )
      
      CapturasMediasPorMes <- CapturasMediasPorMes %>% 
        mutate(Mes_Nome = case_when(
          Mes == 1 ~ "Janeiro",
          Mes == 2 ~ "Fevereiro",
          Mes == 3 ~ "Março",
          Mes == 4 ~ "Abril",
          Mes == 5 ~ "Maio",
          Mes == 6 ~ "Junho",
          Mes == 7 ~ "Julho",
          Mes == 8 ~ "Agosto",
          Mes == 9 ~ "Setembro",
          Mes == 10 ~ "Outubro",
          Mes == 11 ~ "Novembro",
          Mes == 12 ~ "Dezembro",
          TRUE ~ as.character(Mes)  # Se não for nenhum dos casos, mantém o número do mês
        ))
      cores <- c("Albacora bandolim" = "purple","Albacora branca" = "red",
                 "Albacora laje" = "green", "Meca" = "yellow",
                 "Outros" = "orange", "Tubarão Azul" = "blue")
      plot_ly(
        data = CapturasMediasPorMes,
        x = ~Mes,
        y = ~MediaTonMes,
        type = 'scatter',
        mode = 'lines+markers',
        color = ~Especie,
        colors = cores,
        marker = list(size = 10),
        hoverinfo = "text",
        text = ~paste(
          "Espécie: ", Especie, "<br>",
          "Mês: ", Mes_Nome, "<br>",
          "Média de Toneladas: ", MediaTonMes, "<br>"
        )
      ) %>% 
        layout(
          title = "Captura Média por Viagem por Mês",
          xaxis = list(title = "Mês"),
          yaxis = list(
            title = "Captura Média (ton) por Viagem"#,
            #range = c(0,5)
            )
        )
      })
    
    output$graficoDesembarque <- renderPlotly({
      
    })
    
    output$graficoEspecies <- renderPlotly({
      
    })
    
    conteudo_senha_adm({
      output$senhaOutput <- renderUI({
        passwordInput(
          inputId = "senha",
          label = "Senha",
          value = ""
        )
      })
    }) 
    
    conteudo_entrar_adm({
      output$entrarOutput <- renderUI({
        actionButton(
          inputId = "entrar",
          label = "Entrar"
        )
      })
    })
    
    observeEvent(input$reset, {
      updateSliderInput(session, "intervalo_anos", value = c(2018, 2023))
      updateRadioButtons(session, "sexo_escolhido", selected = "Todos")
      updateCheckboxGroupInput(
        session, 
        "species",
        selected = c("Albacora bandolim","Albacora branca","Albacora laje",
                     "Meca","Outros")
        )
    })
    
    observeEvent(input$entrar, {
      if (input$senha == senha_admin) {
        conteudo_tabela_adm({
          dataTableOutput("tabela_tub")
        })
        conteudo_senha_adm(NULL)
        conteudo_entrar_adm(NULL)
        output$tabela_tub <- renderDataTable({
          dadostub <- dados_tubaroes()
          dadostub <- subset(
            dadostub,
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
        },options = list( paging = T, searching = FALSE))
      } else{
        showModal(modalDialog(
          title = "Erro de login",
          "Senha incorreta. Tente novamente.",
          easyClose = TRUE,
          ))
        conteudo_tabela_adm(NULL)
      }
    })
      
    conteudo_tabela_adm <- reactiveVal(NULL)
    
    output$senhaAdm <- renderUI({
      conteudo_senha_adm()
    })
    
    output$entrarAdm <- renderUI({
      conteudo_entrar_adm()
    })
    
    output$tabelaAdm <- renderUI({
      conteudo_tabela_adm()
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
        tabsetPanel(
          id = "bodyTab",
          tabPanel("Projeto", value = "tab1body"),
          tabPanel("Leia-me", value = "tab2body")
        )
    })
    
    # Renderização da imagem
    output$creditos_img <- renderImage({
      list(src = "dados_brutos/ImagemTeste.png", 
           contentType = "image/png",
           alt = "Créditos")  # Texto alternativo para acessibilidade
    }, deleteFile = FALSE)
    # Renderização do gráfico de barras
    output$graficoBarra <- renderPlotly({
      dadostub <- dados_tubaroes()
      
      dadostub <- subset(
        dadostub,
        Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
      
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
      
      # gender_list <- as.list(gender)
      
      plot_ly(
        data = gender, 
        labels = ~Sexo,
        values = ~n,
        type = "pie",
        hole = 0.6
      )
    })
    
    # Renderização do texto da aba 1
    output$textOut1 <- renderUI({
        HTML(paste0("<pre>", pdf_content1$text, "</pre>"))
    })
    
    # Renderização do texto da aba 2
    output$textOut2 <- renderUI({
        HTML(paste0("<pre>", pdf_content2$text, "</pre>"))
    })
  }
)
