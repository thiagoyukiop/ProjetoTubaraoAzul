if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, htmltools,
  readtext, dplyr, ggplot2, ggrepel, scales, plotly, zoo
)

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
    sidebar = dashboardSidebar(
      width = 250,
      minified = T,
      collapsed = FALSE,
      tags$head(
        tags$style(HTML('
        /* Ajuste o tamanho dos títulos das abas dentro do sidebarPanel */
        .custom-sidebar .nav-tabs > li > a {
          width: 240px; /* Largura fixa para os títulos das abas */
          text-align: center; /* Centraliza o texto */
        }
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
          width: 100%;
        }
      '))
      ),
      sidebarMenu(
        id = "sidebarMenu",
        menuItem(
          text = "Apresentação",
          icon = icon("house"),
          menuSubItem(
           text = "Projeto",
           tabName = "tab1body",
           icon = icon("r-project")
          ),
          menuSubItem(
           text = "Leia-me",
           tabName = "tab2body",
           icon = icon("readme")
          )
        ),
        menuItem(
          text = "Distribuição de comprimentos",
          tabName = "tab2header",
          icon = icon("chart-simple")
        ),
        menuItem(
          text = "Desembarques",
          tabName = "tab3header",
          icon = icon("chart-area")
        ),
        menuItem(
          text = "Distribuição espacial das capturas",
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
          class = "box-desembarque",
          fluidRow(
            column(
              width = 12,
              box(
                solidHeader = T,
                title = "Captura Média por Viagem",
                status = "primary",
                plotlyOutput("graficoCaptura")
              ),
              box(
                solidHeader = T,
                title = "Desembarque Total (ton)",
                status = "primary",
                plotlyOutput("graficoDesembarque")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              offset = 3,
              box(
                solidHeader = T,
                title = "Composição de espécies",
                status = "primary",
                plotlyOutput("graficoEspecies")
              )
            )
          )
        ),
        tabItem(
          "tab4header",
          plotlyOutput("mapa_calor")
          ),
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
            choices = c("Albacora bandolim","Albacora branca","Albacora laje",
                        "Meca", "Outros","Tubarao Azul"),
            selected = c("Albacora bandolim","Albacora branca","Albacora laje",
                         "Meca","Outros", "Tubarao Azul")
          ),
          actionButton("reset","Reiniciar Valores",icon = icon("repeat"))
        )
      )
    )
  ),
  server <- function(input, output, session) {
    
    dados_coordenadas <- read.table("dados_brutos/dados_captura_coordenadas.csv",
                                    header = TRUE, sep = ",", dec = ".")
    
    dados_coordenadas_filtrados <- reactive({
      dados_filtrados <- subset(dados_coordenadas, Especie %in% input$species)
      if (input$sexo_escolhido == "Macho") {
        dados_filtrados <- subset(dados_filtrados, Sexo == "M")
      } else if (input$sexo_escolhido == "Fêmea") {
        dados_filtrados <- subset(dados_filtrados, Sexo == "F")
      }
      dados_filtrados <- subset(dados_filtrados,
                                Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
      # Convertendo o resultado para data frame
      data.frame(dados_filtrados)
    })
    
    output$mapa_calor <- renderPlotly({
      dados_filtrados <- dados_coordenadas_filtrados()
      plot_ly(
        data = dados_filtrados,
        type = 'scattergeo',
        mode = 'markers',
        color = ~Especie,
        colors = cores,
        hoverinfo = "text",
        text = ~paste(
          "Espécie: ", Especie, "<br>",
          "Coordenada: ", round(latitude,3), "º, ", round(longitude,3), "º<br>"
        ),
        lat = ~latitude,
        lon = ~longitude) %>%
        layout(title = "Coordenadas da captura das espécies marinhas",
               geo = list(projection = list(type = 'robinson')),
               showlegend = FALSE
               )
    })
    
    
    observeEvent(input$sidebarCollapsed, {
      if (input$sidebarCollapsed) {
        output$creditos_img <- renderImage({
          list(src = "dados_brutos/ImagemTeste.png",
               contentType = "image/png",
               width = "0%",
               height = "0%",
               alt = "Créditos")
        }, deleteFile = FALSE)
        
      } else {
        output$creditos_img <- renderImage({
          list(src = "dados_brutos/ImagemTeste.png",
               contentType = "image/png",
               width = "100%",
               height = "100%",
               alt = "Créditos")
        }, deleteFile = FALSE)
      }
    })
    
    cores <- c("Albacora bandolim" = "purple","Albacora branca" = "red",
               "Albacora laje" = "green", "Meca" = "yellow",
               "Outros" = "orange", "Tubarao Azul" = "blue")
    
    
    
    dados_capturas <- read.table("dados_brutos/tabela_dados_ficticios.csv",
                                 header = TRUE, sep = ";", dec = ",")
    
    dados_capturas_filtrados <- reactive({
      dados_capturas <- subset(dados_capturas, Especie %in% input$species)
      if (input$sexo_escolhido == "Macho") {
        dados_capturas <- subset(dados_capturas, Sexo == "M")
      } else if (input$sexo_escolhido == "Fêmea") {
        dados_capturas <- subset(dados_capturas, Sexo == "F")
      } else if (input$sexo_escolhido == "Todos"){
        
      }
      subset(dados_capturas,
             Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
    })
    
    CapturasPorMesDesembarque <- reactive({
      dados_capturas_filtrados() %>%
        group_by(Especie, Ano, Mes) %>% 
        summarise(Media_Toneladas = mean(Toneladas)) %>%
        mutate(Media_Toneladas = round(Media_Toneladas, 2)) %>%
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
          TRUE ~ as.character(Mes) 
        )) %>%
        mutate(mes_ano = as.yearmon(paste0(Ano, "-", sprintf("%02d", Mes))))
    })
    
    CapturasMediasPorMes <- reactive({
      dados_capturas_filtrados() %>%
        group_by(Especie, Ano, Mes) %>%
        summarise(Media_Toneladas = mean(Toneladas)) %>%
        group_by(Especie, Mes) %>%
        summarise(Media_Toneladas_por_Mes = mean(Media_Toneladas)) %>%
        mutate(MediaTonMes = round(Media_Toneladas_por_Mes, 2)) %>%
        select(-Media_Toneladas_por_Mes) %>%
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
          TRUE ~ as.character(Mes) 
        )) 
    })
    
    output$graficoCaptura <- renderPlotly({
      plot_ly(
        data = CapturasMediasPorMes(),
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
            title = "Captura Média (ton) por Viagem"
          ), showlegend = FALSE
        )
    })
    
    output$graficoDesembarque <- renderPlotly({
      plot_ly(
        data = CapturasPorMesDesembarque(),
        x = ~mes_ano,
        y = ~Media_Toneladas,
        type = 'scatter',
        mode = 'lines+markers',
        color = ~Especie,
        colors = cores,
        marker = list(size = 5),
        hoverinfo = "text",
        text = ~paste(
          "Espécie: ", Especie, "<br>",
          "Mês: ", Mes_Nome, "<br>",
          "Ano: ", Ano, "<br>",
          "Média de Toneladas: ", Media_Toneladas, "<br>"
        )
      ) %>% 
        layout(
          title = "Captura Média por Viagem por Mês em cada Ano",
          xaxis = list(title = "Mês"),
          yaxis = list(
            title = "Captura Média (ton) por Viagem"
          ), showlegend = FALSE
        )
    })
    
    output$graficoEspecies <- renderPlotly({
      tiposEspecies <- dados_capturas_filtrados() %>%
        group_by(Especie) %>%
        summarise(n = n(),
                  toneladas_totais = sum(Toneladas)) %>%
        mutate(porc = n/sum(n))
      
      plot_ly(
        data = tiposEspecies,
        labels = ~Especie,
        values = ~n,
        type = "pie",
        hole = 0.6,
        textinfo = 'label',
        hoverinfo = "text",
        text = ~paste(
          "Quantidade: ", n, "<br>",
          "Porcentagem: ", percent(porc, accuracy = 0.1), "<br>"
        ),
        marker = list(colors = cores)
      ) %>% 
        layout(showlegend = FALSE)
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
                     "Meca","Outros","Tubarao Azul")
      )
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
    
    output$creditos_img <- renderImage({
      list(src = "dados_brutos/ImagemTeste.png",
           contentType = "image/png",
           alt = "Créditos")
    }, deleteFile = FALSE)
    
    dados_tubaroes <- read.table("dados_brutos/dados_tubaroes_criados.csv",header = TRUE, sep = ";", dec = ",")
    
    dadostub_filtrados <- reactive({
      # dados <- dados_tubaroes
      if (input$sexo_escolhido == "Macho") {
        dados_tubaroes <- subset(dados_tubaroes, Sexo == "M")
      } else if (input$sexo_escolhido == "Fêmea") {
        dados_tubaroes <- subset(dados_tubaroes, Sexo == "F")
      } else if (input$sexo_escolhido == "Todos"){
        
      }
      subset(dados_tubaroes, Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
    })
    
    observeEvent(input$entrar, {
      if (input$senha == senha_admin) {
        conteudo_tabela_adm({
          dataTableOutput("tabela_tub")
        })
        conteudo_senha_adm(NULL)
        conteudo_entrar_adm(NULL)
        output$tabela_tub <- renderDataTable({
          if (!is.null(input$entrar) && input$entrar > 0) {
            if (input$senha == senha_admin) {
              dadostub_filtrados()
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
    
    dadostub_filtrados_Sexo <- reactive({
      subset(dados_tubaroes, Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
    })
    
    # Renderização do gráfico de barras
    output$graficoBarra <- renderPlotly({
      plot_ly(
        data = dadostub_filtrados(),
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
      gender <- dadostub_filtrados_Sexo() %>%
        count(Sexo)
      
      cores_sexo = c("F" = "red", "M" = "blue")
      
      plot_ly(
        data = gender, 
        labels = ~Sexo,
        values = ~n,
        type = "pie",
        hole = 0.6,
        textinfo = 'percent',
        hoverinfo = "text",
        text = ~paste(
          "Sexo: ", Sexo, "<br>",
          "Quantidade: ", n, "<br>"
        ),
        marker = list(colors = cores_sexo)
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

