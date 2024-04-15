if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, htmltools,
  dplyr, ggplot2, ggrepel, scales, plotly, zoo
)

senha_admin <- "senha123"

shinyApp(
  ui = dashboardPage(
    skin = "blue",
    header = dashboardHeader(
      title = tags$a(href = "https://lrpdc.shinyapps.io/proj_tubarao_azul/",
                     "Projeto Tubarão Azul", class = "logo"),
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
          href = NULL,
          icon = icon("ferry"),
        )
      )
    ),
    sidebar = dashboardSidebar(
      width = 250,
      minified = T,
      collapsed = FALSE,
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
          "Administrador",
          tabName = "tab5header",
          icon = icon("user-tie")
        ),
        imageOutput("creditos_img")
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "tab1body",
          fluidPage(
            fluidRow(
              column(
                width = 6,
                h2("Geração de subsídios e elaboração do Plano de gestão da
                   pesca do Tubarão Azul, e monitoramento da atividade no 
                   Estado do Rio Grande do Sul"),
                h3("Como surgiu o Projeto Tubarão Azul?"),
                p("O tubarão-azul ",tags$em("Prionace glauca", .noWS = "after"),
                " é um dos tubarões mais abundantes e de mais ampla 
                distribuição nos oceanos do planeta, sendo a espécie mais
                frequente nas capturas da frota de espinhel e superfície no
                Oceano Atlântico Sul."),
                p("Em 2014 o tubarão-azul foi classificado como Vulnerável à
                  extinção a nível estadual (Decreto Estadual 51.797/2014).
                  Tal classificação impõe, por meio de leis federais, 
                  restrição à captura, desembarque e comercialização da 
                  espécie no estado do Rio Grande do Sul."),
                p("Em 2016, 17 especialistas de diferentes instituições 
                  concordaram com a classificação da espécie como vulnerável. 
                  Avaliaram, no entanto, que a proibição pontual da pesca do 
                  tubarão-azul em águas gaúchas não seria uma medida adequada 
                  para a sua conservação, devido aos seguintes motivos:"),
                p(strong("1) Por ser fauna acompanhante de outras espécies
                         comerciais, tubarões azuis continuariam sendo 
                         capturados em quantidades expressivas.")),
                p(strong("2) O procedimento de liberação dos exemplares 
                         capturados poderia inviabilizar economicamente a 
                         frota de espinhel pelágico.")),
                p(strong("3) A pesca clandestina e a descarga em locais fora
                         do Rio Grande do Sul continuariam ocorrendo.")),
                p(strong("4) Perder-se-ia a pesca regularizada e 
                         sistematicamente acompanhada como fonte de dados 
                         para o monitoramento do estoque.")),
                p("Como parte do processo, foi criado um Grupo Técnico para a
                  elaboração de um Plano de Gestão da Pesca do tubarão-azul no
                  Rio Grande do Sul, surgindo daí o Projeto Tubarão Azul."),
                h3("Legal, mas o que é um Plano de Gestão da Pesca?"),
                p("Trata-se de um documento que estabelece as orientações para
                  o uso sustentável dos recursos pesqueiros e tem como objetivo 
                  assegurar a sustentabilidade tanto da pesca quanto do 
                  ambiente natural, levando em conta os aspectos sociais, 
                  econômicos e ecológicos das pescarias."),
                p("A equipe técnica vem realizando levantamento de informações 
                  através de ações de monitoramento e coleta de amostras 
                  biológicas na descarga das embarcações de espinhel e da 
                  realização de embarque de observadores de bordo - 
                  profissionais treinados que participam das viagens de pesca 
                  coletando amostra e informações, com vistas a avaliação do 
                  estoque e proposição/adoção de medidas para os órgãos de 
                  manejo responsáveis, conforme o fluxograma."),
                h3("Fluxograma do Plano de Gestão da pesca de Tubarão Azul 
                   no Rio Grande do Sul"),
                imageOutput("FluxogramaTubAzul")
              ),
              column(
                width = 6,
                infoBox(
                  fill = T,
                  title = "Tubarões Medidos",
                  width = 10,
                  color = "light-blue",
                  value = 28954,
                  icon = icon("fish")
                ),
                infoBox(
                  fill = T,
                  title = "Entrevista de desembarque",
                  width = 10,
                  color = "light-blue",
                  value = 731,
                  icon = icon("paste")
                ),
                infoBox(
                  fill = T,
                  title = "Cadernos de bordo",
                  width = 10,
                  color = "light-blue",
                  value = 465,
                  icon = icon("book-open")
                ),
                infoBox(
                  fill = T,
                  title = "Embarcações Monitoradas",
                  width = 10,
                  color = "light-blue",
                  value = 92,
                  icon = icon("ship")
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "tab2body",
          fluidPage(
            fluidRow(
              column(
                width = 4,
                offset = 2,
                p("Prezado Usuário,")
              ),
              column(
                width = 6,
                offset = 2,
                p("Esta plataforma foi desenvolvida para disponibilizar 
                  informações atualizadas sobre as capturas de tubarão azul e 
                  da frota de espinhel pelágico que vêm sendo coletadas pela 
                  equipe do projeto Tubarão Azul. Os dados são referentes à 
                  desembarques realizados pela frota no porto de Rio Grande,
                  RS."),
                p("Na aba “Distribuição de comprimentos” você encontrará as 
                  composições de comprimentos dos indivíduos amostrados em cada
                  ano para machos, fêmeas e sexos agrupados. Está disponível 
                  também a proporção de sexos dos indivíduos capturados."),
                p("Na aba “Desembarques” você encontrará as capturas mensais
                  médias por viagem para todas as espécies e também 
                  discriminado por espécie."),
                p("Na aba “Distribuição espacial das capturas” você encontrará 
                  os lances de pesca realizados distribuídos espacialmente, 
                  assim como as densidades especializadas de lances em que cada
                  uma das espécies foi capturada."),
                p("Para a construção dos gráficos apresentados nesta plataforma
                  são utilizados dados atualizados anualmente."),
                p("Para maiores informações, por favor, entre em contato através
                  do",strong("e-mail proj.tubaraoazul.furg@gmail.com."))
              )
            )
          )
        ),
        tabItem(
          "tab2header",
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
        ),
        tabItem(
          "tab3header",
          fluidPage(
            fluidRow(
             column(
               width = 6,
               box(
                 width = 12,
                 solidHeader = T,
                 title = "Gráfico de Linhas",
                 status = "primary",
                 plotlyOutput("graficoCaptura")
               )
             ),
             column(
               width = 6,
               box(
                 width = 12,
                 solidHeader = T,
                 title = "Gráfico de Rosca",
                 status = "primary",
                 plotlyOutput("graficoEspecies")
               )
             ) ,
             fluidRow(
               column(
                 width = 12,
                 box(
                   width = 12,
                   solidHeader = T,
                   title = "Gráfico de Linhas",
                   status = "primary",
                   plotlyOutput("graficoDesembarque")
                 )
               )
             )
            )
          )
        ),
        tabItem(
          "tab4header",
          fluidPage(
            fluidRow(
              column(
                width = 12,
                box(
                  width = 12,
                  solidHeader = T,
                  status = "primary",
                  title = "Gráfico de Dispersão",
                  plotlyOutput("mapa_calor")
                )
              )
            )
          )
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
    dados_gerais <- read.table("dados_brutos/dados_PTA.csv",
                               header = TRUE, sep = ",", dec = ".")
    
    dados_gerais_filtrados <- reactive({
      dados_filtrados <- subset(dados_gerais, Especie %in% input$species)
      if (input$sexo_escolhido == "Macho") {
        dados_filtrados <- subset(dados_filtrados, Sexo == "M")
      } else if (input$sexo_escolhido == "Fêmea") {
        dados_filtrados <- subset(dados_filtrados, Sexo == "F")
      }
      dados_filtrados <- subset(
        dados_filtrados,
        Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
      data.frame(dados_filtrados)
    })
    
    output$FluxogramaTubAzul <- renderImage({
      list(src = "dados_brutos/Fluxograma.png",
           width = "100%",
           contentType = "image/png",
           alt = "Créditos")
    }, deleteFile = FALSE)
    
    output$mapa_calor <- renderPlotly({
      dados_filtrados <- dados_gerais_filtrados()
      plot_ly(
        data = dados_filtrados,
        type = 'scattergeo',
        mode = 'markers',
        color = ~Especie,
        colors = cores,
        hoverinfo = "text",
        text = ~paste(
          "Espécie: ", Especie, "<br>",
          "Coordenada: ", round(Latitude,3), "º, ", round(Longitude,3), "ºa<br>"
        ),
        lat = ~Latitude,
        lon = ~Longitude,
        sizes = "0.1px"
        ) %>%
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
    
    CapturasPorMesDesembarque <- reactive({
      dados_gerais_filtrados() %>%
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
      dados_gerais_filtrados() %>%
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
          title = "Captura Média por Viagem",
          xaxis = list(
            title = "Mês"
            ),
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
          title = "Desembarque Total (ton)",
          xaxis = list(title = "Mês"),
          yaxis = list(
            title = "Captura Média (ton) por Viagem"
          ), showlegend = FALSE
        )
    })
    
    cores <- c("Albacora bandolim" = "purple","Albacora branca" = "red",
               "Albacora laje" = "green", "Meca" = "yellow",
               "Outros" = "orange", "Tubarao Azul" = "blue")
    
    output$graficoEspecies <- renderPlotly({
      tiposEspecies <- dados_gerais_filtrados() %>%
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
        layout(
          title = "Composição de espécies",
          showlegend = FALSE
          )
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
    
    conteudo_senha_adm <- reactiveVal(NULL)
    conteudo_entrar_adm <- reactiveVal(NULL)
    conteudo_tabela_adm <- reactiveVal(NULL)
    
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
              dados_gerais_filtrados()
            }
          }
        },options = list(paging = T, searching = FALSE))
      } else{
        showModal(modalDialog(
          title = "Erro de login",
          "Senha incorreta. Tente novamente.",
          easyClose = TRUE,
        ))
        conteudo_tabela_adm(NULL)
      }
    })
    
    output$senhaAdm <- renderUI({
      conteudo_senha_adm()
    })
    
    output$entrarAdm <- renderUI({
      conteudo_entrar_adm()
    })
    
    output$tabelaAdm <- renderUI({
      conteudo_tabela_adm()
    })
    
    dadostub_filtrados <- reactive({
      dados_auxiliar <- subset(dados_gerais, Especie == "Tubarao Azul")
      if (input$sexo_escolhido == "Macho") {
        dados_auxiliar <- subset(dados_gerais, Sexo == "M")
      } else if (input$sexo_escolhido == "Fêmea") {
        dados_auxiliar <- subset(dados_gerais, Sexo == "F")
      } else if (input$sexo_escolhido == "Todos"){

      }
      subset(
        dados_auxiliar,
        Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2]
        )
    })
    
    dadostub_filtrados_Sexo <- reactive({
      subset(
        dados_gerais,
        Especie == "Tubarao Azul" & 
        Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2]
        )
    })
    
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
          title = "Frequência Relativa do Comprimento de Tubarões Azul",
          yaxis = list(
            title = "Frequência Relativa (%)",
            tickwidth = 2,
            showgrid = TRUE,
            titlefont = list(size = 18)
          ),
          xaxis = list(
            title = "Comprimento total (cm)",
            titlefont = list(size = 18)
          )
        )
    })
    
    output$graficoRosca <- renderPlotly({
      gender <- dadostub_filtrados_Sexo() %>%
        count(Sexo)
      
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
        marker = list(colors = c("F" = "red", "M" = "blue")) 
      )%>% 
        layout(
          title = "Quantidade de Tubarões Azul por Sexo",
          showlegend = FALSE
        )
    })
  }
)