# Bibliotecas -------------------------------------------------------------

pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, htmltools, DT,apexcharter,
  dplyr, ggplot2, ggrepel, scales, plotly, zoo, rsconnect, vctrs
)

# Interface do Usuário ----------------------------------------------------

# Define a interface do usuário
ui = dashboardPage(
  skin = "blue", # Definindo a cor do tema do Painel
  scrollToTop = T,
  # Header ------------------------------------------------------------------
  # Definindo a Header do Painel
  header = dashboardHeader(
    # Definição do Título com Link da Header
    title = tags$a(
      href = "https://lrpdc.shinyapps.io/proj_tubarao_azul/", # Link URL
      "Projeto Tubarão Azul", # Título da Header
      class = "logo"
    ),
    controlbarIcon = icon("sliders"), # Definição do ícone da aba de Controle
    # Definição do Menu Suspenso
    dropdownMenu(
      type = "notifications", # Tipo de Menu Suspenso
      icon = icon("bell"),
      # Criação da Notificação
      notificationItem(
        text = "Nova embarcação ocorrerá dia 20/03.", 
        href = "https://lrpdc.shinyapps.io/proj_tubarao_azul/",
        icon = icon("ship")
      ),
      notificationItem(
        text = "Nova embarcação ocorrerá dia 17/04.",
        href = NULL,
        icon = icon("ferry")
      )
    )
  ),
  
  # Sidebar -----------------------------------------------------------------
  
  # Definindo o Sidebar do Painel
  sidebar = dashboardSidebar(
    width = 250,   # Definição da Largura em pixels
    minified = T,  # Se a aba lateral ao ser fechada deverá mostrar os ícones
    collapsed = F, # Se a aba lateral deve ser iniciada fechada
    # Definindo do Menu Sidebar
    sidebarMenu(
      id = "sidebarMenu",
      # Definindo o item do Menu da Tela Inicial
      menuItem(
        text = "Apresentação",
        icon = icon("house"),
        # Definindo o item do Sub-Menu do Projeto
        menuSubItem(
          text = "Projeto", 
          tabName = "tab1body", # Definição do nome do tab
          icon = icon("r-project")
        ),
        # Definindo o item do Sub-Menu do Leia-me
        menuSubItem(
          text = "Leia-me",
          tabName = "tab2body",
          icon = icon("readme")
        )
      ),
      # Definindo o item do Menu de Distribuição de Comprimentos
      menuItem(
        text = "Distribuição de comprimentos",
        tabName = "tab2header",
        icon = icon("chart-simple")
      ),
      # Definindo o item do Menu de Desembarques
      menuItem(
        text = "Desembarques",
        tabName = "tab3header",
        icon = icon("chart-area")
      ),
      # Definindo o item do Menu da Distribuição espacial das capturas
      menuItem(
        text = "Distribuição espacial das capturas",
        tabName = "tab4header",
        icon = icon("earth-americas")
      ),
      # Definindo o item do Menu do Administrador
      menuItem(
        text = "Administrador",
        tabName = "tab5header",
        icon = icon("user-tie")
      ),
      # Saída da Imagem, com os créditos dos financiadores do Projeto
      imageOutput("creditos_img")
    )
  ),
  
  # Body --------------------------------------------------------------------
  
  # Definindo o Body do Painel
  body = dashboardBody(
    tabItems(
      # Definindo o conteúdo do Projeto
      tabItem(
        tabName = "tab1body",
        fluidPage(
          fluidRow(
            column(
              width = 6, # Definindo a largura da coluna
              infoBox(
                title = "Tubarões Medidos",
                fill = T,                  # Se a infoBox deve ser preenchida
                width = 10,                # Definindo a largura da infoBox
                color = "light-blue",      # Definindo cor da infoBox
                # Definindo Configurações do conteúdo da infoBox
                value = tags$div(
                  style = "display: block; text-align: center;", 
                  h1(strong("28954"))
                ),
                icon = icon("fish")
              )
            ),
            column(
              width = 6,
              infoBox(
                title = "Entrevista de desembarque",
                fill = T,
                width = 10,
                color = "light-blue", 
                value = tags$div(
                  style = "display: block; text-align: center;", 
                  h1(strong("731"))
                ),
                icon = icon("paste")
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              infoBox(
                title = "Cadernos de bordo",
                fill = T,
                width = 10,
                color = "light-blue",
                value = tags$div(
                  style = "display: block; text-align: center;", 
                  h1(strong("465"))
                ),
                icon = icon("book-open")
              )
            ),
            column(
              width = 6,
              infoBox(
                title = "Embarcações Monitoradas",
                fill = T,
                width = 10,
                color = "light-blue",
                value = tags$div(
                  style = "display: block; text-align: center;",
                  h1(strong("92"))
                ),
                icon = icon("ship")
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              offset = 2, # Definindo Deslocamento da Coluna
              # Definindo Texto do Projeto
              h2("Geração de subsídios e elaboração do Plano de gestão da
                   pesca do Tubarão Azul, e monitoramento da atividade no 
                   Estado do Rio Grande do Sul"),
              br(),
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
              br(),
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
              br(),
              h3("Fluxograma do Plano de Gestão da pesca de Tubarão Azul 
                   no Rio Grande do Sul"),
              # Saída da Imagem do Fluxograma do Projeto
              imageOutput("FluxogramaTubAzul")
            )
          )
        )
      ),
      # Definindo o conteúdo do Leia-me
      tabItem(
        tabName = "tab2body",
        fluidPage(
          fluidRow(
            column(
              width = 8,
              offset = 2,
              # Definindo o texto do Leia-me
              p("Prezado Usuário,"),
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
                  do e-mail ",strong("proj.tubaraoazul.furg@gmail.com."))
            )
          )
        )
      ),
      # Definindo o conteúdo da Distribuição de Comprimentos
      tabItem(
        tabName = "tab2header",
        # Definindo Caixa com conteúdo da Distribuição de Comprimentos
        box(
          title = "Histograma",
          solidHeader = T, # Se a Header é sólida
          status = "primary",
          # Saída do Gráfico Histograma Plotly do Comprimento de Tubarões Azul
          plotlyOutput("graficoBarra")
        ),
        box(
          title = "Gráfico de Rosca",
          solidHeader = T,
          status = "primary",
          # Saída do Gráfico Rosca Plotly do Comprimento de Tubarões Azul
          plotlyOutput("graficoRosca")
        )
      ),
      # Definindo o conteúdo de Desembarques
      tabItem(
        tabName = "tab3header",
        fluidPage(
          fluidRow(
            column(
              width = 6,
              box(
                title = "Gráfico de Linhas",
                width = 12,
                solidHeader = T, 
                status = "primary",
                # Saída do Gráfico Plotly de Captura 
                plotlyOutput("graficoCaptura")
              )
            ),
            column(
              width = 6,
              box(
                title = "Gráfico de Rosca",
                width = 12,
                solidHeader = T,
                status = "primary",
                # Saída do Gráfico Plotly das Espécies
                plotlyOutput("graficoEspecies")
              )
            ) 
          ),
          fluidRow(
            column(
              width = 12,
              box(
                title = "Gráfico de Linhas",
                width = 12,
                solidHeader = T,
                status = "primary",
                # Saída do Gráfico Plotly do Desembarque
                apexchartOutput("graficoDesembarque")
                # plotlyOutput("graficoDesembarque")
              )
            )
          )
        )
      ),
      # Definindo o conteúdo da Distribuição Espacial das Capturas
      tabItem(
        tabName = "tab4header",
        fluidPage(
          fluidRow(
            column(
              width = 12,
              box(
                title = "Gráfico de Dispersão",
                width = 12,
                solidHeader = T,
                status = "primary",
                # Saída do Gráfico Plotly do Mapa de Calor
                plotlyOutput("mapa_calor")
              )
            )
          )
        )
      ),
      # Definindo o conteúdo do Administrador
      tabItem(
        tabName = "tab5header",
        value = "tab5header",
        fluidRow(
          column(
            width = 4,
            offset = 4,
            # Saída do PasswordInput para a Interface do Usuário
            uiOutput("senhaAdm"),
            # Saída do ActionButton para a Interface do Usuário
            uiOutput("entrarAdm")
          )
        ),
        fluidRow(
          column(
            width = 12,
            # Saída da Tabela com os dados para Interface do Usuário
            uiOutput("tabelaAdm")
          )
        )
      )
    )
  ),
  
  # ControlBar --------------------------------------------------------------
  
  # Definindo o Controlbar do Painel
  controlbar = dashboardControlbar(
    overlay = F, # Se vai sobrepor o conteúdo
    collapsed = F,
    skin = "dark",
    id = "controlbar",
    # Definindo controlbar Menu
    controlbarMenu(
      id = "controlbarMenu",
      # Definindo o Item Opções
      controlbarItem(
        "Opções",
        icon = icon("gear"),
        # Entrada do controle deslizante
        sliderInput(
          inputId = "intervalo_anos",   # Identificador do controle deslizante
          label = "Intervalo de Anos:", # Rótulo do controle deslizante
          min = 2018,                   # Valor Mínimo do controle deslizante
          max = 2023,                   # Valor Máximo do controle deslizante
          value = c(2018,2023),         # Valor Inicial do controle deslizante
          step = 1,        # Intervalo entre os valores do controle deslizante
          # Opções das animações
          animate = animationOptions(
            playButton = icon("play"),
            pauseButton = icon("pause")
          )
        ),
        # Botões de rádio
        radioButtons(
          inputId = "sexo_escolhido", 
          label = "Escolha o Sexo:",
          choices = c("Todos", "Macho", "Fêmea"), # Opções dos botões de rádio
          selected = "Todos"                      # Opção inicial selecionada
        ),
        # Entrada de grupo de caixas de seleção
        checkboxGroupInput(
          inputId = "species",
          label = "Seletor de Espécies:",
          choices = c("Albacora bandolim","Albacora branca","Albacora laje",
                      "Meca", "Outros","Tubarao Azul"),
          selected = c("Albacora bandolim","Albacora branca","Albacora laje",
                       "Meca","Outros", "Tubarao Azul")
        ),
        # Botão de ação
        actionButton(
          inputId = "reset",
          label = "Reiniciar Valores",
          icon = icon("repeat")
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

# Definindo Servidor do Painel
server <- function(input, output, session) {
  
  # Variáveis ---------------------------------------------------------------
  
  # FALAR COM SANTANA, POIS SENHA FUNCIONA NO LOCAL, MAS NO SITE NÃO
  # Definindo Senha da Tabela do Administrador
  senha_admin <- Sys.getenv("SENHA_ADMIN")
  
  # Carregando os Dados de um Arquivo csv
  dados_gerais <- read.table("dados_brutos/dados_PTA.csv",
                             header = TRUE, sep = ",", dec = ".")
  
  # Definindo as Cores de cada Espécie
  cores <- c("Albacora bandolim" = "purple","Albacora branca" = "red",
             "Albacora laje" = "green", "Meca" = "yellow",
             "Outros" = "orange", "Tubarao Azul" = "blue")
  
  coresAux <- as.list(cores)
  
  # Criando um Valores Reativos que vão Iniciar Nulo
  conteudo_senha_adm <- reactiveVal(NULL)
  conteudo_entrar_adm <- reactiveVal(NULL)
  conteudo_tabela_adm <- reactiveVal(NULL)
  
  # Filtro de Dados ---------------------------------------------------------
  
  # Filtrando os Dados Gerais Reativamente
  dados_gerais_filtrados <- reactive({
    # Filtrando as Espécies 
    dados_filtrados <- subset(dados_gerais, Especie %in% input$species)
    # Filtrando o Sexo
    if (input$sexo_escolhido == "Macho") {
      dados_filtrados <- subset(dados_filtrados, Sexo == "M")
    } else if (input$sexo_escolhido == "Fêmea") {
      dados_filtrados <- subset(dados_filtrados, Sexo == "F")
    }
    # Filtrando o Intervalo de Anos
    dados_filtrados <- subset(
      dados_filtrados,
      Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
    data.frame(dados_filtrados)
  })
  
  # Filtrando Dados Para o Gráfico de Desembarque
  CapturasPorMesDesembarque <- reactive({
    dados_gerais_filtrados() %>%
      # Agrupa os Dados por Colunas Selecionadas
      group_by(Especie, Ano, Mes) %>% 
      # Média das Toneladas de Captura de Cada Grupo
      summarise(Media_Toneladas = mean(Toneladas)) %>%
      # Arredondando a Média de Toneladas para Duas Casas Decimais
      mutate(Media_Toneladas = round(Media_Toneladas, 2)) %>%
      # Criação de Nome do Mês para Legenda
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
      #  Cria uma Variável Formato mes_ano
      mutate(mes_ano = as.yearmon(paste0(Ano, "-", sprintf("%02d", Mes)))) %>%
      # Formata a coluna mes_ano para exibir apenas o mês e o ano
      mutate(mes_ano_formatado = format(as.Date(
        mes_ano, format = "%Y-%m"),
        "%b %Y")) %>% 
      mutate(
        cores = case_when(
          Especie == "Albacora bandolim" ~ "purple",
          Especie == "Albacora branca" ~ "red",
          Especie == "Albacora laje" ~ "green",
          Especie == "Meca" ~ "yellow",
          Especie == "Outros" ~ "orange",
          Especie == "Tubarao Azul" ~ "blue",
          TRUE ~ NA_character_  # Caso Não haja Correspondência
        )
      )
  })
  
  # Filtrando Dados para o Gráfico de Captura
  CapturasMediasPorMes <- reactive({
    dados_gerais_filtrados() %>%
      group_by(Especie, Ano, Mes) %>%
      summarise(Media_Toneladas = mean(Toneladas)) %>%
      # Agrupando as Colunas Selecionadas
      group_by(Especie, Mes) %>% 
      # Média das Toneladas de Captura, de cada Mês, com Anos Agrupados
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
  
  # Filtrando os Dados para o Histograma de Comprimento dos Tubarões Azul
  dadostub_filtrados <- reactive({
    dados_auxiliar <- subset(dados_gerais, Especie == "Tubarao Azul")
    if (input$sexo_escolhido == "Macho") {
      dados_auxiliar <- subset(dados_gerais, Sexo == "M")
    } else if (input$sexo_escolhido == "Fêmea") {
      dados_auxiliar <- subset(dados_gerais, Sexo == "F")
    }
    subset(
      dados_auxiliar,
      Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2]
    )
  })
  
  # Filtrando os Dados para o Gráfico de Distribuição do Sexo dos Tubarões Azul
  dadostub_filtrados_Sexo <- reactive({
    subset(
      dados_gerais,
      Especie == "Tubarao Azul" & 
        Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2]
    )
  })
  
  # Sidebar -----------------------------------------------------------------
  
  # Verificação de se a Sidebar está Recolhida
  observeEvent(input$sidebarCollapsed, {
    if (input$sidebarCollapsed) {
      # Renderizando a Imagem Minimizada dos Créditos
      output$creditos_img <- renderImage({
        list(
          src = "dados_brutos/ImagemTeste.png",
          contentType = "image/png",
          width = "0%",
          height = "0%",
          alt = "Créditos"
        )
      }, deleteFile = FALSE)
    } else {
      # Renderizando a Imagem Maximizada dos Créditos
      output$creditos_img <- renderImage({
        list(
          src = "dados_brutos/ImagemTeste.png",
          contentType = "image/png",
          width = "100%",
          height = "100%",
          alt = "Créditos"
        )
      }, deleteFile = FALSE)
    }
  })
  
  # Projeto -----------------------------------------------------------------
  
  # Renderizando a Imagem do Fluxograma
  output$FluxogramaTubAzul <- renderImage({
    list(
      src = "dados_brutos/Fluxograma.png", # Local do arquivo da Imagem
      height = "100%",                     # Altura da Imagem
      width = "100%",                      # Largura da Imagem
      contentType = "image/png"            # Tipo do Conteúdo da Imagem
    )
  }, deleteFile = FALSE)                   # Não Deleta o Arquivo após o Uso
  
  # Distribuição de Comprimentos --------------------------------------------
  
  #Renderizando Histograma Plotly da Distribuição do Comprimento dos Tubarões Azul
  output$graficoBarra <- renderPlotly({
    # Definindo o Valor Inicial do Histograma no Eixo X
    start_value <- floor(min(dadostub_filtrados()$Tamanho) / 5) * 5
    
    plot_ly(
      data = dadostub_filtrados(),
      x = ~Tamanho,
      type = "histogram",
      histnorm = "percent",
      xbins = list(start = start_value ,size = 5),
      marker = list(
        color = "#3C8DBC",
        line = list(
          color = "black",
          width = 1
        )
      ),
      hoverinfo = "x"
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
  
  # Renderizando Gráfico de Rosca Plotly da Distribuição de Sexo dos Tubarões Azul
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
  
  # Desembarques ------------------------------------------------------------
  
  # Renderização do Gráfico Plotly da Média Mensal de Capturas (mes)
  output$graficoCaptura <- renderPlotly({
    plot_ly(
      data = CapturasMediasPorMes(),
      x = ~Mes,
      y = ~MediaTonMes,
      type = 'scatter',
      mode = 'lines+markers',
      color = ~Especie,
      colors = cores,
      marker = list(size = 10), # Tamanho do Marcador
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
  
  output$graficoDesembarque <- renderApexchart({
    apex(
      data = CapturasPorMesDesembarque(),
      type = "area",
      mapping = aes(
        x = mes_ano_formatado,
        y = Media_Toneladas,
        fill = Especie
      )
    ) %>%
      ax_colors_manual(coresAux)
  })
  
  
  # # Renderização do Gráfico Plotly da Média Mensal de Capturas de Todo Período
  # # (mes_ano)
  # output$graficoDesembarque <- renderPlotly({
  #   plot_ly(
  #     data = CapturasPorMesDesembarque(),
  #     x = ~mes_ano,
  #     y = ~Media_Toneladas,
  #     type = 'scatter',
  #     mode = 'lines+markers',
  #     color = ~Especie,
  #     colors = cores,
  #     marker = list(size = 5),
  #     hoverinfo = "text",
  #     text = ~paste(
  #       "Espécie: ", Especie, "<br>",
  #       "Mês: ", Mes_Nome, "<br>",
  #       # "Ano: ", Ano, "<br>",
  #       "Média de Toneladas: ", Media_Toneladas, "<br>"
  #     )
  #   ) %>% 
  #     layout(
  #       title = "Desembarque Total (ton)",
  #       xaxis = list(
  #         title = "Mês"
  #       ),
  #       yaxis = list(
  #         title = "Captura Média (ton) por Viagem"
  #       ), showlegend = FALSE
  #     )
  # })
  
  # Renderizando Gráfico de Rosca Plotly das Espécies Presentes nos Dados
  output$graficoEspecies <- renderPlotly({
    
    # Criando dataframe para ser Utilizado no Gráfico
    tiposEspecies <- dados_gerais_filtrados() %>%
      group_by(Especie) %>% # Agrupando os Dados por Espécie
      summarise(
        n = n(), # Quantidade de Capturas por Espécie
        toneladas_totais = sum(Toneladas) # Somando as Toneladas para cada Grupo
      ) %>%
      mutate(porc = n/sum(n),
             # Para que cada Espécie Mantenha a Mesma Cor
             cores = case_when(
               Especie == "Albacora bandolim" ~ "purple",
               Especie == "Albacora branca" ~ "red",
               Especie == "Albacora laje" ~ "green",
               Especie == "Meca" ~ "yellow",
               Especie == "Outros" ~ "orange",
               Especie == "Tubarao Azul" ~ "blue",
               TRUE ~ NA_character_  # Caso Não haja Correspondência
             ))
    
    plot_ly(
      data = tiposEspecies,
      labels = ~Especie,
      values = ~n,
      type = "pie",
      hole = 0.6,
      textinfo = 'label', # Cada Fatia terá o Nome de sua Espécie nela
      hoverinfo = "text",
      text = ~paste(
        "Quantidade: ", n, "<br>",
        "Porcentagem: ", percent(porc, accuracy = 0.1), "<br>"
      ),
      marker = list(colors = tiposEspecies$cores)
    ) %>%
      layout(
        title = "Composição de espécies",
        showlegend = FALSE
      )
  })
  
  # Distribuição Espacial das Capturas --------------------------------------
  
  # Renderizando o Gráfico Plotly do Mapa de Calor
  output$mapa_calor <- renderPlotly({
    dados_filtrados <- dados_gerais_filtrados()
    # Criando um gráfico Plotly
    plot_ly(
      data = dados_filtrados,
      type = 'scattergeo',  # Define o tipo de Gráfico como Dispersão Geográfica
      mode = 'markers',     # Define o modo nos Pontos Específicados
      color = ~Especie, 
      colors = cores,
      hoverinfo = "text",   # Define as Informações do Hover como Texto
      text = ~paste(
        "Espécie: ", Especie, "<br>",
        "Coordenada: ", round(Latitude,3), "º, ", round(Longitude,3), "º<br>"
      ),
      lat = ~Latitude,      # Define Latitude
      lon = ~Longitude,     # Define Longitude
      sizes = "0.1px"       # Define o Tamanho dos Marcadores
    ) %>%
      layout(
        title = "Coordenadas da captura das espécies marinhas",      # Título
        geo=list(projection=list(type='robinson')), # Define Projeção Gráfica
        showlegend = FALSE
      )
  })
  
  # Administrador -----------------------------------------------------------
  
  # Valor Reativo Recebe a Entrada de Senha
  conteudo_senha_adm({
    output$senhaOutput <- renderUI({
      passwordInput(
        inputId = "senha",
        label = "Senha:",
        value = ""
      )
    })
  })
  
  # Valor Reativo Recebe um Botão
  conteudo_entrar_adm({
    output$entrarOutput <- renderUI({
      actionButton(
        inputId = "entrar",
        label = "Entrar"
      )
    })
  })
  
  # Verificação do Pressionamento do Botão Entrar
  observeEvent(input$entrar, {
    if (input$senha == senha_admin) {
      conteudo_tabela_adm({
        # Saída da Data Table, que Possuí a Tabela com os Dados
        DTOutput("tabela_tub")
      })
      # Valores Reativos Recebem o Valor de Nulo
      conteudo_senha_adm(NULL)
      conteudo_entrar_adm(NULL)
      # Renderizando a DataTable com os Dados Filtrados
      output$tabela_tub <- renderDT({
        if (!is.null(input$entrar) && input$entrar > 0) {
          if (input$senha == senha_admin) {
            dados_gerais_filtrados()
          }
        }
        # Opções da Data Table
      },options = list(paging = T, searching = FALSE))
    } else{
      # Mensagem no Caso de Senha Incorreta
      showModal(modalDialog(
        title = "Erro de login",
        "Senha incorreta. Tente novamente.", 
        easyClose = TRUE
      ))
      # Valor Reativo Recebe Valor Nulo
      conteudo_tabela_adm(NULL)
    }
  })
  
  # Renderizando UI com Conteúdo dos Valores Reativos
  output$senhaAdm <- renderUI({
    conteudo_senha_adm()
  })
  
  output$entrarAdm <- renderUI({
    conteudo_entrar_adm()
  })
  
  output$tabelaAdm <- renderUI({
    conteudo_tabela_adm()
  })
  
  # ControlBar --------------------------------------------------------------
  
  # Verificação do Botão Reset, ao ser Pressionado Atualizará os Controles
  observeEvent(input$reset, {
    # Atualização no Controle Deslizante 
    updateSliderInput(
      session,
      inputId = "intervalo_anos",
      value = c(2018, 2023)
    )
    # Atualização nos Botões de Rádio
    updateRadioButtons(
      session, 
      inputId = "sexo_escolhido",
      selected = "Todos"
    )
    # Atualização no Grupo de Caixas de Seleção
    updateCheckboxGroupInput(
      session, 
      inputId = "species",
      selected = c("Albacora bandolim","Albacora branca","Albacora laje",
                   "Meca","Outros","Tubarao Azul")
    )
  })
  
}

shinyApp(ui,server)
