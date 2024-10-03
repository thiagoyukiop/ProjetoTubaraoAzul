# Bibliotecas -------------------------------------------------------------

# Carregando os Pacotes que serão utilizados no dashboard
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, shiny.i18n,
  leaflet, leaflet.extras,
  dplyr, tidyverse, scales, zoo, DT, tibble,
  plotly, shinyjs,
  raster, digest, sodium,
  DBI, RSQLite,
  RColorBrewer
)

# Habilita o recarregamento automático do aplicativo ao detectar mudanças.
options(shiny.autoreload = TRUE)

# Conecta ao banco de dados SQLite, que contém as tabelas
db <- dbConnect(SQLite(), "dados_brutos/database.sqlite")
# Fecha a conexão ao final da execução
on.exit(dbDisconnect(db))

# Lê as tabelas do banco de dados
dados <- dbReadTable(db, "Dados")
dados_falsos <- dbReadTable(db, "Dados_falsos")

notificacoes <- read.csv(
  "dados_brutos/NotificacoesTabela.csv",
  fileEncoding = "UTF-8"
)

notificacoes$AvisoDeDesembarque <- as.Date(notificacoes$AvisoDeDesembarque)
notificacoes$DataDoDesembarque <- as.Date(notificacoes$DataDoDesembarque)
notificacoes$Saída <- as.Date(notificacoes$Saída)
notificacoes$Chegada <- as.Date(notificacoes$Chegada)

novas_linhas <- data.frame(
  Embarcação = c("Nova Embarcação 1", "Nova Embarcação 2", "Nova Embarcação 3"),
  AvisoDeDesembarque = as.Date(c(Sys.Date(), "2024-09-19", "2024-09-26")),
  DataDoDesembarque = as.Date(c(Sys.Date(), "2024-09-22", "2024-10-03")),
  Saída = as.Date(c(Sys.Date(), "2024-09-19", "2024-09-27")),
  Chegada = as.Date(c(Sys.Date(), "2024-09-18","2024-10-03")),
  IndivíduosMedidosDeTubarãoAzul = c(150, 75, 97),
  IndivíduosMedidosDeTubarãoAnequim = c(0, 0, 0)
)

# TESTE
# Adicionar as novas linhas à tabela existente
notificacoes <- rbind(notificacoes, novas_linhas)


data_atual <- Sys.Date()

NovaColuna <- ifelse(
  notificacoes$Saída > data_atual, "Futuro", 
  ifelse(
    notificacoes$Saída < data_atual, "Passado",
    "Hoje"
  )
)

notificacoesTabela <- cbind(Status = NovaColuna, notificacoes)

notificacoesTabela$DiasRestantes <- ifelse(
  notificacoes$Saída > data_atual, as.integer(notificacoes$Saída - data_atual), 
  ifelse(
    notificacoes$Saída == data_atual, "Hoje",
    "Passou"
  )
)

# Le arquivos rds
user_base <- readRDS("dados_brutos/user_base.rds")

# Dicionário para substituição de categorias
categoria_substituicoes <- c(
  "Albacora-bandolim" = "Albacora_bandolim",
  "Albacora-branca" = "Albacora_branca",
  "Albacora-lage" = "Albacora_lage",
  "Cacao-anequim" = "Cacao_anequim",
  "Cacao-azul" = "Cacao_azul"
)

# Ajusta a coluna CATEGORIA em 'dados' com base em 'categoria_substituicoes'
dados_ajustados <- dados %>%
  mutate(CATEGORIA = recode(CATEGORIA, !!!categoria_substituicoes))

# i18n <- Translator$new(translation_json_path = "traducoes/translation.json")
i18n <- Translator$new(
  translation_json_path = "traducoes/translation_es_complete_v2.json"
)
i18n$set_translation_language("pt")
i18n$use_js()

# Interface do Usuário ----------------------------------------------------

# Define a interface do usuário 
ui <- dashboardPage(
  # Cria uma tag que contém metadados sobre HTML
  tags$head(
    # Cria uma tag que é usada para definir estilos CSS dentro da HTML
    tags$style(
      # Marca caracteres como HTML
      HTML('
      .main-header .logo {
      padding: 0 1px;
      }
      
      .main-footer {
      height: 15vh;
      z-index: 1000;
      }
           ')
    )
  ),
  skin = "blue", # Definindo a cor do tema do Painel
  scrollToTop = TRUE,
  
  # Header ------------------------------------------------------------------
  
  # Definindo a Header do Painel
  header = dashboardHeader(
    titleWidth = 230,
    title = uiOutput("textoHeader"),
    controlbarIcon = icon("sliders"), # Definição do ícone da aba de Controle
    # Definição do Menu Suspenso
    dropdownMenuOutput("notification_menu")
  ),
  
  # Sidebar -----------------------------------------------------------------
  
  # Definindo o Sidebar do Painel
  sidebar = dashboardSidebar(
    useShinyjs(),  # Necessário para usar shinyjs
    # Define um script JavaScript dentro da tag script
    tags$script(HTML("
      Shiny.addCustomMessageHandler('sidebarState', function(collapsed) {
        if (collapsed) {
          // Sidebar is closed
          $('.main-footer').css('height', '16vh');
        } else {
          // Sidebar is open
          $('.main-footer').css('height', '15vh');
        }
      });
    ")),
    tags$head(tags$style(HTML('
      .sidebar-mini:not(.sidebar-mini-expand-feature).sidebar-collapse
      .sidebar-menu>li:hover>a>span:not(.pull-right) {
        width: 181px !important;
        padding-right: 2rem;
        padding-left: 2rem;
      }
      .main-footer {
        z-index: 1000;
        padding-bottom: 0px;
      }
    '))),
    width = 230, # Definição da Largura em pixels
    minified = TRUE,  # Se a aba lateral ao ser fechada deverá mostrar os ícones
    collapsed = FALSE, # Se a aba lateral deve ser iniciada fechada
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(
        text = tagList(i18n$t("apresentacao")), 
        icon = icon("house"),
        menuSubItem(
          text = tagList(i18n$t("projeto")),
          tabName = "projeto",
          icon = icon("r-project")
        ),
        menuSubItem(
          text = tagList(i18n$t("leiame")),
          tabName = "leia_me",
          icon = icon("readme")
        ),
        menuSubItem(
          text = tagList(i18n$t("sobre")),
          tabName = "sobre",
          icon = icon("circle-info")
        )
      ),
      menuItem(
        text = tagList(i18n$t("distribuicao")),
        icon = icon("chart-bar"),
        menuSubItem(
          text = tagList(i18n$t("captura")),
          tabName = "captura",
          icon = icon("chart-pie")
        ),
        menuSubItem(
          text = tagList(i18n$t("comprimento")),
          tabName = "comprimento",
          icon = icon("chart-simple")
        )
      ),
      menuItem(
        text = tagList(i18n$t("desembarque")),
        tabName = "desembarque",
        icon = icon("chart-area")
      ),
      menuItem(
        text = tagList(i18n$t("distribuicao_espacial")),
        icon = icon("globe"),
        menuSubItem(
          text = tagList(i18n$t("captura")),
          tabName = "captura_espacial",
          icon = icon("earth-americas")
        ),
        menuSubItem(
          text = tagList(i18n$t("comprimento")),
          tabName = "comprimento_espacial"
        )
      ),
      menuItem(
        text = tagList(i18n$t("tabela_embarcacoes")),
        tabName = "tabela_embarcacoes",
        icon = icon("table"),
        badgeLabel = nrow(notificacoes),
        badgeColor = "red"
      )
    )
  ),
  
  # Body --------------------------------------------------------------------
  
  # Definindo o Body do Painel
  body = dashboardBody(
    shiny::useBusyIndicators(),
    busyIndicatorOptions(
      spinner_type = 'bars3',
      spinner_delay = 0,
      spinner_size = 100
    ),
    # Ajustando Visualização de Mapa para que sempre fique com a altura ideal
    tags$head(tags$style(HTML(' 
    body {
      overflow-y: hidden;
    }
    
    .mapa {
    display: flex;
    width: 100%;
    height: 60vh;
    visibility: inherit;
    position: relative;
    z-index: 100;
    }
    
    .graficos {
    display: flex;
    width: 100%;
    # height: 30vh;
    height: 40vh;
    visibility: inherit;
    position: relative;
    z-index: 100;
    }
    
    .graficosMaiores {
    display: flex;
    width: 100%;
    # height: calc(70vh - 120px);
    height: 70vh;
    visibility: inherit;
    position: relative;
    z-index: 100;
    }

    .direct-chat-contacts {
      z-index: 100 !important;
    }
    
    .direct-chat-contacts p {
      text-align: justify;
      margin-right: 8px;
    }
    
    .content-wrapper {
      background-color: #FFFFFF; /* cor de fundo branca */
    }
    
    .box-header {
      padding-right: 25px;
      text-align: center;
    }
    .box {
      overflow-x: auto; /* Impede que o conteúdo transborde horizontalmente */
    }
    
    #LogoPTA img {
      width: 60%;
      height: auto;
    }
    
    #FluxogramaTubAzul img {
      width: 70%;
      height: auto;
    }
    
    #Logo_Instituicoes img {
      width: 70%;
      height: auto;
    }
    
    #Logo_MAPA img {
      # width: 80%;        /* Define a largura como 80% */
      # height: auto;      /* Mantém a proporção */
    }
    
    .content {
      overflow: auto;
      height: 76vh;
    }
                              ')
    )
    ),
    tabItems(
      # Definindo o conteúdo do Projeto
      tabItem(
        tabName = "projeto",
        # Cria uma página com layout fluido
        fluidPage(
          # Cria uma página com layout fixo
          fluidRow(
            # Cria uma coluna dentro de uma definição da Interface do Usuário
            column(
              offset = 1,
              width = 5,
              infoBox(
                title = tags$div(
                  h6(
                    tagList(
                      shiny.i18n::usei18n(i18n),
                      i18n$t("tubaroes_medidos")
                    )
                  ),
                  style = "display: block; text-align: center;"
                ),
                fill = TRUE,          # Se a infoBox deve ser preenchida
                width = 12,           # Definindo a largura da infoBox
                color = "light-blue", # Definindo cor da infoBox
                # Definindo Configurações do conteúdo da infoBox
                value = tags$div(
                  style = "display: block; text-align: center;",
                  h1(strong("28954"), style = "margin: 0px;")
                ),
                icon = icon("fish")
              )
            ),
            column(
              width = 5,
              infoBox(
                title = tags$div(
                  h6(tagList(i18n$t("entrevista_desembarque"))),
                  style = "display: block; text-align: center;"
                ),
                fill = TRUE,
                width = 12,
                color = "light-blue",
                value = tags$div(
                  style = "display: block; text-align: center;",
                  h1(strong("731"), style = "margin: 0px;")
                ),
                icon = icon("paste")
              )
            )
          ),
          fluidRow(
            column(
              offset = 1,
              width = 5,
              infoBox(
                title = tags$div(
                  h6(tagList(i18n$t("cadernos_bordo"))),
                  style = "display: block; text-align: center;"
                ),
                fill = TRUE,
                width = 12,
                color = "light-blue",
                value = tags$div(
                  style = "display: block; text-align: center;",
                  h1(strong("465"), style = "margin: 0px;")
                ),
                icon = icon("book-open")
              )
            ),
            column(
              width = 5,
              infoBox(
                title = tags$div(
                  h6(tagList(i18n$t("embarcacoes_monitoradas"))),
                  style = "display: block; text-align: center;"
                ),
                fill = TRUE,
                width = 12,
                color = "light-blue",
                value = tags$div(
                  style = "display: block; text-align: center;",
                  h1(strong("92"), style = "margin: 0px;")
                ),
                icon = icon("ship")
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              div(
                style = "text-align: center;",
                imageOutput("LogoPTA", height = "100%")
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              # Definindo Texto do Projeto
              tags$div(
                h3(tagList(i18n$t("projeto_texto_1"))),
                style = "text-align:center;"
              ),
              br(),
              tags$div(
                style = "text-align:center;",
                h4(tagList(i18n$t("projeto_texto_2")))
              ),
              tags$div(
                style = "text-align:justify;",
                p(
                  tagList(i18n$t("projeto_texto_3")),
                  tags$em("Prionace glauca", .noWS = "after"),
                  tagList(i18n$t("projeto_texto_4"))
                ),
                p(tagList(i18n$t("projeto_texto_5"))),
                p(tagList(i18n$t("projeto_texto_6"))),
                p(strong(tagList(i18n$t("projeto_texto_7")))),
                p(strong(tagList(i18n$t("projeto_texto_8")))),
                p(strong(tagList(i18n$t("projeto_texto_9")))),
                p(strong(tagList(i18n$t("projeto_texto_10")))),
                p(tagList(i18n$t("projeto_texto_11"))),
                br()
              ),
              tags$div(
                style = "text-align:center;",
                h4(tagList(i18n$t("projeto_texto_12")))
              ),
              tags$div(
                style = "text-align:justify;",
                p(tagList(i18n$t("projeto_texto_13")))
              ),
              tags$div(
                style = "text-align:justify;",
                p(tagList(i18n$t("projeto_texto_14"))),
                br()
              ),
              tags$div(
                style = "text-align:center;",
                h4(tagList(i18n$t("projeto_texto_15")))
              ),
              div(
                style = "text-align: center;",
                imageOutput("FluxogramaTubAzul", height = "100%")
              )
            )
          )
        )
      ),
      # Definindo o conteúdo do Leia-me
      tabItem(
        tabName = "leia_me",
        fluidPage(
          fluidRow(
            column(
              width = 10,
              offset = 1,
              tags$head(
                tags$style(HTML("
                  #boxWithoutHeader .box-header {
                    display: none;
                    # width: auto;
                    # height: calc(100vh-98px);
                  }
                "))
              ),
              box(
                id = "boxWithoutHeader",
                title = NULL,
                width = 12,
                # Se deve exibir uma borda abaixo do cabeçalho
                headerBorder = FALSE, 
                background = "gray", # Define a cor do fundo
                # Definindo o texto do Leia-me
                tags$div(
                  style = "text-align:justify;",
                  p(tagList(i18n$t("leiame_texto_1"))),
                  p(tagList(i18n$t("leiame_texto_2"))),
                  p(tagList(i18n$t("leiame_texto_3"))),
                  p(tagList(i18n$t("leiame_texto_4"))),
                  p(tagList(i18n$t("leiame_texto_5"))),
                  p(tagList(i18n$t("leiame_texto_6"))),
                  p(tagList(i18n$t("leiame_texto_7"))),
                  p(tagList(i18n$t("leiame_texto_8"))),
                  p(tagList(i18n$t("leiame_texto_9"))),
                  p(
                    tagList(i18n$t("leiame_texto_10")),
                    strong(tagList(i18n$t("leiame_texto_11")))
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "sobre",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            box(
              id = "boxWithoutHeader",
              title = NULL,
              width = 12,
              headerBorder = FALSE,
              background = "gray",
              h4("Equipe"),
              p("Este projeto foi desenvolvido pela equipe do Projeto Tubarão 
                Azul, onde este dashboard foi desenvolvido pelo bolsista e 
                estudante de Engenharia da Computação, na Univali.")
              
            )
          )
        )
      ),
      # Definindo o conteúdo da Distribuição de Captura
      tabItem(
        tabName = "captura",
        fluidRow(
          column(
            width = 12,
            # Definindo Caixa com conteúdo da Distribuição de Captura
            box(
              title = tagList(i18n$t("captura_texto_1")),
              width = 12,
              solidHeader = TRUE, # Se a Header é sólida
              status = "primary",
              div(
                class = "graficos",
                # Saída do Gráfico de Barras Empilhadas de dados Registrados
                plotlyOutput("TubMesAno", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebar2",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                width = 30,
                p(tagList(i18n$t("captura_texto_2")))
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = tagList(i18n$t("captura_texto_3")),
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              div(
                class = "graficos",
                plotlyOutput("BarraTubOutros", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebar3",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("captura_texto_4")))
              )
            )
          ),
          column(
            width = 6,
            box(
              title = tagList(i18n$t("captura_texto_5")),
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              div(
                class = "graficos",
                # Saída do Mapa de Calor que compara os dados por mês
                plotlyOutput("ComparaDadosTub", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebar4",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("captura_texto_6")))
              )
            )
          )
        )
      ),
      # Definindo o conteúdo de Desembarques
      tabItem(
        tabName = "desembarque",
        fluidRow(
          column(
            width = 6,
            box(
              title = tagList(i18n$t("desembarque_texto_1")),
              width = 12,
              solidHeader = TRUE, 
              collapsible = TRUE,
              status = "primary",
              div(
                class = "graficos",
                # Saída do Gráfico de Linha de Captura 
                plotlyOutput("graficoCaptura", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebar6",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("desembarque_texto_2")))
              )
            )
          ),
          column(
            width = 6,
            box(
              title = tagList(i18n$t("desembarque_texto_1")),
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              div(
                class = "graficos",
                # Saída do Mapa de Calor do Peso das Espécies
                plotlyOutput("pesoMes", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebar7",
                icon = icon("circle-info"),
                background = "#A6ACAFEF",
                p(tagList(i18n$t("desembarque_texto_3")))
              )
            )
          ) 
        ),
        fluidRow(
          column(
            width = 12,
            box(
              title = tagList(i18n$t("desembarque_texto_4")),
              width = 12,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              div(
                class = "graficosMaiores",
                # Saída do Gráfico Plotly do Desembarque
                plotlyOutput("graficoAreaDesembarque", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebar82",
                icon = icon("circle-info"),
                width = 30,
                background = "#A6ACAFEF",
                p(tagList(i18n$t("desembarque_texto_5")))
              )
            )
          )
        )
      ),
      # Definindo o conteúdo da Distribuição Espacial das Capturas
      tabItem(
        tabName = "captura_espacial",
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              title = tagList(i18n$t("captura_espacial_texto_1")),
              solidHeader = TRUE,
              status = "primary",
              div(
                class = "mapa",
                leafletOutput("Mapas", height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebar9",
                icon = icon("circle-info"),
                width = 30,
                background = "#A6ACAFEF",
                uiOutput("textBoxSidebar")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "comprimento",
        fluidRow(
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              title = tagList(i18n$t("comprimento_texto_1")),
              status = "primary",
              plotlyOutput("histograma_comprimento"),
              sidebar = boxSidebar(
                id = "boxsidebar10",
                icon = icon("circle-info"),
                width = 50,
                background = "#A6ACAFEF",
                p(tagList(i18n$t("comprimento_texto_2")))
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              title = tagList(i18n$t("comprimento_texto_3")),
              status = "primary",
              plotlyOutput("boxplot_comprimento"),
              sidebar = boxSidebar(
                id = "boxsidebar11",
                icon = icon("circle-info"),
                width = 50,
                background = "#A6ACAFEF",
                p(tagList(i18n$t("comprimento_texto_4")))
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tabela_embarcacoes",
        DTOutput("tabela_embarcacoes")
      )
    )
  ),
  footer = dashboardFooter(
    left = list(
      fluidRow(
        tags$div(
          style = "margin-left: 20px;
          margin-top: -15px;
          margin-bottom: -20px;", 
          h4(tagList(i18n$t("instituicoes_executoras")))
        ),
        column(
          width = 5,
          tags$img(
            imageOutput("Logo_Instituicoes",height = "100%", width = "100%")
          )
        ),
        column(
          offset = 4,
          width = 3,
          tags$div(
            style = "margin-left: 50px; margin-top: -15px;
            margin-bottom: -30px; padding-right: 0px;",
            h4(tagList(i18n$t("apoio"))),
            br()
          ),
          imageOutput("Logo_MAPA",height = "100%", width = "100%")
        )
      )
    )
  ),
  
  # ControlBar --------------------------------------------------------------
  
  # Definindo o Controlbar do Painel
  controlbar = dashboardControlbar(
    overlay = FALSE, # Se vai sobrepor o conteúdo
    collapsed = FALSE,
    skin = "dark",
    id = "controlbar",
    width = 230,
    # Definindo controlbar Menu
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        title = tagList(i18n$t("filtros")),
        icon = icon("filter"),
        selectInput(
          inputId = "selected_language",
          label = tagList(
            shiny.i18n::usei18n(i18n),
            i18n$t("trocar_linguagem")
          ),
          choices = setNames(
            i18n$get_languages()[-1],
            c("🇵🇹 - Português","🇬🇧 - English", "🇪🇸 - Español")
          ),
          selected = i18n$get_key_translation()
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'captura'",
          sliderInput(
            inputId = "anos_captura",
            label = tagList(i18n$t("intervalo_anos")),
            min = min(dados_ajustados$ANO),
            max = max(dados_ajustados$ANO),
            value = c(min(dados_ajustados$ANO), max(dados_ajustados$ANO)),
            step = 1,
            animate = animationOptions(
              interval = 1700,
              playButton = icon("play"),
              pauseButton = icon("pause")
            ),
            sep = ""
          ),
          checkboxGroupInput(
            inputId = "especies_captura",
            label = tagList(i18n$t("seletor_especies")),
            choiceValues = c(
              "Albacora_bandolim", "Albacora_branca", "Albacora_lage",
              "Cacao_anequim", "Meca", "Outros", "Prego"
            ),
            choiceNames = c(
              tagList(i18n$t("albacora_bandolim")),
              tagList(i18n$t("albacora_branca")),
              tagList(i18n$t("albacora_lage")),
              tagList(i18n$t("cacao_anequim")),
              tagList(i18n$t("meca")),
              tagList(i18n$t("outros")),
              tagList(i18n$t("prego"))
            ),
            selected = dados_ajustados$CATEGORIA
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento'",
          radioButtons(
            inputId = "sexo_comprimento",
            label = tagList(i18n$t("seletor_sexo")),
            choiceValues = c("Todos", "M", "F"),
            choiceNames = c(
              tagList(i18n$t("todos")),
              tagList(i18n$t("macho")),
              tagList(i18n$t("femea"))
            ),
            selected = "Todos"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'desembarque'",
          sliderInput(
            inputId = "anos_desembarque",
            label = tagList(i18n$t("intervalo_anos")),
            min = min(dados_ajustados$ANO),
            max = max(dados_ajustados$ANO),
            value = c(min(dados_ajustados$ANO), max(dados_ajustados$ANO)),
            step = 1,
            animate = animationOptions(
              interval = 1700,
              playButton = icon("play"),
              pauseButton = icon("pause")
            ),
            sep = ""
          ),
          checkboxGroupInput(
            inputId = "especies_desembarque",
            label = tagList(i18n$t("seletor_especies")),
            choiceValues = c(
              "Albacora_bandolim", "Albacora_branca", "Albacora_lage",
              "Cacao_anequim", "Meca", "Outros", "Prego"
            ),
            choiceNames = c(
              tagList(i18n$t("albacora_bandolim")),
              tagList(i18n$t("albacora_branca")),
              tagList(i18n$t("albacora_lage")),
              tagList(i18n$t("cacao_anequim")),
              tagList(i18n$t("meca")),
              tagList(i18n$t("outros")),
              tagList(i18n$t("prego"))
            ),
            selected = dados_ajustados$CATEGORIA
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'captura_espacial'",
          sliderInput(
            inputId = "anos_cap_esp",
            label = tagList(i18n$t("intervalo_anos")),
            min = min(dados_ajustados$ANO),
            max = max(dados_ajustados$ANO),
            value = c(min(dados_ajustados$ANO), max(dados_ajustados$ANO)),
            step = 1,
            animate = animationOptions(
              interval = 1700,
              playButton = icon("play"),
              pauseButton = icon("pause")
            ),
            sep = ""
          ),
          checkboxGroupInput(
            inputId = "especies_cap_esp",
            label = tagList(i18n$t("seletor_especies")),
            choiceValues = c(
              "Albacora_bandolim", "Albacora_branca", "Albacora_lage",
              "Cacao_anequim", "Meca", "Outros", "Prego"
            ),
            choiceNames = c(
              tagList(i18n$t("albacora_bandolim")),
              tagList(i18n$t("albacora_branca")),
              tagList(i18n$t("albacora_lage")),
              tagList(i18n$t("cacao_anequim")),
              tagList(i18n$t("meca")),
              tagList(i18n$t("outros")),
              tagList(i18n$t("prego"))
            ),
            selected = dados_ajustados$CATEGORIA
          ),
          radioButtons(
            inputId = "mapa_cap_esp",
            label = tagList(i18n$t("seletor_mapa")),
            choiceValues = c("KilosTotais", "KiloPorViagem", "Viagens"),
            choiceNames = c(
              tagList(i18n$t("kilostotais")),
              tagList(i18n$t("kiloporviagem")),
              tagList(i18n$t("viagens"))
            ),
            selected = "KilosTotais"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento_espacial'",
          
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'tabela_embarcacoes'",
          radioButtons(
            inputId = "status_tabela",
            label = tagList(i18n$t("status_embarcacao")),
            choiceValues = c("Todos", "Hoje", "Passado", "Futuro"),
            choiceNames = c(
              tagList(i18n$t("todos")),
              tagList(i18n$t("hoje")),
              tagList(i18n$t("passado")),
              tagList(i18n$t("futuro"))
            ),
            selected = "Todos"
          )
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

# Definindo Servidor do Painel
server <- function(input, output, session) {
  
  # Variáveis ---------------------------------------------------------------
  
  # Completando dados que não estão presentes em algumas datas com NA
  dados_completos <- dados_ajustados %>%
    complete(CATEGORIA, ANO, MES = 1:12, fill = list(VALOR = NA))%>% 
    filter(!(ANO == 2024 & MES >= 5))
  
  # Trocando dados com NA para 0, em KG, que é o peso Capturado
  dados_gerais <- dados_completos %>%
    mutate(KG = replace_na(KG, 0))
  
  nomes_meses_reativo <- reactive({
    c(
      i18n$t("mes_1"), i18n$t("mes_2"), i18n$t("mes_3"), i18n$t("mes_4"), 
      i18n$t("mes_5"), i18n$t("mes_6"), i18n$t("mes_7"), i18n$t("mes_8"), 
      i18n$t("mes_9"), i18n$t("mes_10"), i18n$t("mes_11"), i18n$t("mes_12")
    )
  })
  
  categorias <- unique(dados_ajustados$CATEGORIA)
  
  cores <- brewer.pal(n = 8, name = "Set1")
  
  cores <- cores[cores != "#377EB8"]
  
  cores_categoria <- setNames(c(rep(NA, length(categorias))), categorias)
  
  cores_categoria["Cacao_azul"] <- "#377EB8"
  
  outras_categorias <- categorias[categorias != "Cacao_azul"]
  cores_categoria[outras_categorias] <- cores[1:length(outras_categorias)]
  
  # Header ------------------------------------------------------------------
  
  observeEvent(input$selected_language, {
    output$notification_menu <- renderMenu({
      notification_items <- lapply(1:nrow(notificacoesTabela), function(i) {
        current_date <- Sys.Date()
        
        notification_status <- "primary"
        
        if (notificacoesTabela$Saída[i] == current_date) {
          notification_status <- "danger"
        }
        
        if (notificacoesTabela$Saída[i] >= current_date) {
          notification_time <- format(
            as.POSIXct(
              paste(
                notificacoesTabela$Saída[i]
              )
            ),
            "%d/%m/%Y"
          )
          notificationItem(
            icon = icon("bell"),
            status = notification_status,
            tags$div(
              tags$span(
                paste(
                  notificacoesTabela$Embarcação[i]
                ),
                style = "font-weight: bold;"
              ),
              br(),
              if (notificacoesTabela$Saída[i] == current_date) {
                tags$span(
                  paste(
                    tagList(i18n$t("notificacao_texto_1"))
                  ), style = "color: #a94442;"
                )
              }
              else{
                tags$span(
                  paste(
                    tagList(i18n$t("notificacao_texto_2")),
                    notificacoesTabela$DiasRestantes[i],
                    tagList(i18n$t("notificacao_texto_3"))
                  ), style = "color: #337ab7;"
                )
              }
            )
          )
        }
        else {
          return(NULL)
        }
      })
      
      # Remover itens NULL da lista
      notification_items <- notification_items[!sapply(
        notification_items,is.null)]
      
      dropdownMenu(
        type = "notifications",
        headerText = paste(
          tagList(i18n$t("notificacao_texto_4")),
          length(notification_items),
          tagList(i18n$t("notificacao_texto_5"))
        ),
        icon = icon("bell"),
        .list = notification_items
      )
    })
  })
  
  # Sidebar -----------------------------------------------------------------
  
  # Verificação de se a Sidebar está Recolhida
  observeEvent(input$sidebarCollapsed, {
    
    session$sendCustomMessage('sidebarState', input$sidebarCollapsed)
    
    if (input$sidebarCollapsed) {
      output$textoHeader <- renderUI({
        tags$img(
          src = "icone_tubarao_preto.png",
          height = "30px",
          width = "30px"
        )
      })
    } else {
      output$textoHeader <- renderUI({
        tags$span(i18n$t("projeto_tubarao_azul"))
      })
    }
  })
  
  # Projeto -----------------------------------------------------------------
  
  output$LogoPTA <- renderImage({
    list(
      src = "dados_brutos/logo_tuba_azul_3.png", # Local do arquivo da Imagem
      height = "auto",
      width = "100%",                      # Largura da Imagem
      contentType = "image/png",            # Tipo do Conteúdo da Imagem
      id = "LogoPTA"
    )
  }, deleteFile = FALSE)
  
  # Renderizando a Imagem do Fluxograma
  output$FluxogramaTubAzul <- renderImage({
    list(
      src = "dados_brutos/Fluxograma_ajustado.png", # Local do arquivo da Imagem
      height = "auto",                     # Altura da Imagem
      width = "100%",                      # Largura da Imagem
      contentType = "image/png"            # Tipo do Conteúdo da Imagem
    )
  }, deleteFile = FALSE)                   # Não Deleta o Arquivo após o Uso
  
  output$Logo_Instituicoes <- renderImage({
    list(
      src = "dados_brutos/instituicoes_executoras.png",
      height = "auto",
      width = "100%",
      contentType = "image/jpg"
    )
  }, deleteFile = FALSE)
  
  output$Logo_MAPA <- renderImage({
    list(
      src = "dados_brutos/logo_MAPA2.png",
      height = "auto",
      width = "100%",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  # Distribuição de Captura --------------------------------------------
  
  # Filtrando os Dados da Tabela Inicial
  dados_aux_filtrados <- reactive({
    # Filtrando as Espécies 
    dados_aux <- subset(
      dados_ajustados,
      CATEGORIA %in% union(input$especies_captura, "Cacao_azul")
    )
    # Filtrando o Intervalo de Anos
    dados_aux <- subset(
      dados_aux,
      ANO >= input$anos_captura[1] & ANO <= input$anos_captura[2]
    )
    data.frame(dados_aux)
  })
  
  # Dividindo os dados Registrados de Cacao-azul em Comparação ao Resto e 
  # Completando os Mês/Ano sem registros, completando com Zero
  dados_TubMesAno <- reactive({
    dados_aux_filtrados() %>%
      mutate(CATEGORIA=if_else(CATEGORIA!="Cacao_azul","Outros",CATEGORIA)) %>%
      group_by(CATEGORIA, ANO, MES) %>%
      summarise(Quantidade = n()) %>%
      ungroup() %>%
      complete(CATEGORIA, ANO, MES = 1:12, fill = list(Quantidade = 0)) %>%
      filter(!(ANO == 2024 & MES >= 5)) %>%
      mutate(mes_ano_formatado = make_date(ANO, MES)) %>%
      mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES)))) %>%
      mutate(mes_ano_formatado = format(mes_ano_formatado, "%Y-%m")) %>% 
      group_by(mes_ano_formatado) %>%
      mutate(total = sum(Quantidade)) %>%
      ungroup() %>%
      # Calcula a porcentagem para cada categoria
      mutate(percentage = Quantidade / total*100)
  }) 
  
  output$TubMesAno <- renderPlotly({
    plot_ly(
      data = dados_TubMesAno(),
      x = ~mes_ano_formatado,
      y = ~percentage,
      color = ~CATEGORIA,
      colors = cores_categoria,
      type = 'scatter',
      stackgroup = 'one',
      groupnorm = 'percent',
      mode = 'none',
      hoverinfo = "text",
      text = ~paste(
        " ",i18n$t("legenda_data"), mes_ano, "<br>",
        i18n$t("legenda_categoria"),case_when(
          CATEGORIA == "Cacao_azul" ~ i18n$t("tubarao_azul"),
          CATEGORIA == "Outros" ~ i18n$t("outros"),
          TRUE ~ CATEGORIA
        ),
        "<br>",
        i18n$t("legenda_quantidade"), Quantidade, "<br>",
        i18n$t("legenda_porcentagem"), round(percentage, 2), "%"
      )
    ) %>%
      layout(
        xaxis = list(
          title = "",
          tickvals = dados_TubMesAno()$mes_ano_formatado[seq(
            1, length(dados_TubMesAno()$mes_ano_formatado), by = 4)], # era 2
          ticktext = dados_TubMesAno()$mes_ano_formatado[seq(
            1, length(dados_TubMesAno()$mes_ano_formatado), by = 4)], # era 2
          showgrid = FALSE
        ),
        yaxis = list(
          title = " ",
          tickformat = ".0f",
          ticksuffix = '%',
          showgrid = FALSE
        ),
        barmode = "stack",
        showlegend = FALSE,
        hovermode = "x",
        xaxis = list(categoryorder = "category ascending"),
        margin = list(t = 10, b = 40, l = 20, r = 20)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  dados_aux_filtrados <- reactive({
    # Filtrando as Espécies 
    dados_aux <- subset(
      dados_ajustados, 
      CATEGORIA %in% union(input$especies_captura, "Cacao_azul")
    )
    # Filtrando o Intervalo de Anos
    dados_aux <- subset(
      dados_aux,
      ANO >= input$anos_captura[1] & ANO <= input$anos_captura[2]
    )
    data.frame(dados_aux)
  })
  
  # Dividindo os dados em duas categorias, e fazendo a proporção de dados
  dados_BarraTubOutros <- reactive({
    dados_aux_filtrados() %>%
      group_by(MES) %>% 
      mutate(
        CATEGORIA = if_else(
          CATEGORIA != "Cacao_azul", "Outros", CATEGORIA
        )
      ) %>%
      count(CATEGORIA) %>% 
      mutate(prop = (n / sum(n)) * 100) %>% 
      mutate(media = round(n / 12, 2))
  })
  
  output$BarraTubOutros <- renderPlotly({
    plot_ly(
      data = dados_BarraTubOutros(),
      x = ~MES,
      y = ~prop,
      color = ~CATEGORIA,
      colors = cores_categoria,
      type = 'bar',
      text = ~paste(
        " ", i18n$t("legenda_categoria"), case_when(
          CATEGORIA == "Cacao_azul" ~ i18n$t("tubarao_azul"),
          CATEGORIA == "Outros" ~ i18n$t("outros"),
          TRUE ~ CATEGORIA
        ),
        "<br>",
        i18n$t("legenda_quantidade_total"), n, "<br>",
        i18n$t("legenda_quantidade_media"), media, "<br>",
        i18n$t("legenda_porcentagem"), round(prop,2), "%"
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = NULL,
        showlegend = FALSE,
        yaxis = list(
          title = " ",
          tickformat = ".0f",
          ticksuffix = '%',
          showgrid = FALSE
        ),
        barmode = "stack",
        showlegend = FALSE,
        hovermode = "x",
        xaxis = list(
          title = i18n$t("mes"),
          categoryorder = "category descending",
          tickvals = unique(dados_BarraTubOutros()$MES),
          ticktext = unique(dados_BarraTubOutros()$MES)
        ),
        margin = list(t = 10, b = 40, l = 20, r = 20)
      )
  })
  
  # Filtrando os Dados da Tabela Inicial com somente a CATEGORIA Cacao-azul
  dadostub_aux_filtrados <- reactive({
    dados_auxiliar <- subset(dados_ajustados, CATEGORIA == "Cacao_azul")
    subset(
      dados_auxiliar,
      ANO >= input$anos_captura[1] & ANO <= input$anos_captura[2]
    )
  })
  
  dados_ComparaDadosTub <- reactive({
    dadostub_aux_filtrados() %>%
      group_by(CATEGORIA, MES, ANO) %>%
      summarise(Quantidade = n()) %>%
      ungroup() %>%
      complete(CATEGORIA, MES = 1:12, fill = list(Quantidade = 0))
  }) 
  
  output$ComparaDadosTub <- renderPlotly({
    nomes_meses <- nomes_meses_reativo()
    plot_ly(
      dados_ComparaDadosTub(),
      x = ~MES,
      y = ~ANO,
      z = ~Quantidade,
      type = "heatmap",
      colorscale = "Plasma",
      hoverinfo = "text",
      text = ~paste(
        " ", i18n$t("legenda_mes"), nomes_meses[MES], "<br>",
        i18n$t("legenda_ano"), ANO, "<br>",
        i18n$t("legenda_dados_registrados"), Quantidade, "<br>"
      ),
      colorbar = list(
        title = i18n$t("quantidade")
      )
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = i18n$t("mes"),
          tickvals = unique(dados_ComparaDadosTub()$MES),
          ticktext = unique(dados_ComparaDadosTub()$MES),
          showgrid = F
        ),
        yaxis = list(
          title = i18n$t("ano"),
          tickformat = ".0f",
          tickvals = unique(floor(dados_ComparaDadosTub()$ANO)),
          ticktext = unique(floor(dados_ComparaDadosTub()$ANO)),
          showgrid = F
        ),
        legend = list(
          orientation = "h",
          y = 0.9,
          x = 0.1,
          font = list(
            size = 10
          )
        ),
        showlegend = F,  # Desativa a legenda
        margin = list(t = 10, b = 40, l = 20, r = 20)
      )
  })
  
  # Desembarques ------------------------------------------------------------
  
  # Filtrando os Dados Gerais Completos Reativamente
  dados_desembarque <- reactive({
    # Filtrando as Espécies
    dados_aux <- subset(
      dados_gerais, 
      CATEGORIA %in% union(input$especies_desembarque, "Cacao_azul"))
    # Filtrando o Intervalo de Anos
    dados_aux <- subset(
      dados_aux,
      ANO >= input$anos_desembarque[1] & ANO <= input$anos_desembarque[2]
    )
    data.frame(dados_aux)
  })
  
  # Filtrando Dados para o Gráfico de Captura
  dados_graficoCaptura <- reactive({
    dados_desembarque() %>%
      mutate(KG_por_Viagem = (KG/DESCARGA)) %>%
      group_by(CATEGORIA, ANO, MES) %>%
      summarise(Media_KG_por_Viagem = mean(KG_por_Viagem)) %>%
      # Substituindo NAs por Zero
      mutate(Media_KG_por_Viagem = replace_na(Media_KG_por_Viagem, 0)) %>%
      group_by(CATEGORIA, MES) %>%
      # Média das Toneladas de Captura, de cada Mês, com Anos Agrupados
      summarise(MediaKG_Mes_Viagem = mean(Media_KG_por_Viagem)) %>%
      mutate(MediaKGMesViagem = round(MediaKG_Mes_Viagem, 2)) %>%
      dplyr::select(-MediaKG_Mes_Viagem)
  })
  
  # Renderização do Gráfico Plotly da Média Mensal de Capturas (mes)
  output$graficoCaptura <- renderPlotly({
    plot_ly(
      data = dados_graficoCaptura(),
      x = ~MES,
      y = ~MediaKGMesViagem,
      type = 'scatter',
      mode = 'lines+markers',
      color = ~CATEGORIA,
      colors = cores_categoria,
      marker = list(
        size = 10
      ), 
      hoverinfo = "text",
      text = ~paste(
        " ", i18n$t("legenda_especie"),
        case_when(
          CATEGORIA == "Albacora_bandolim" ~ i18n$t("albacora_bandolim"),
          CATEGORIA == "Albacora_branca" ~ i18n$t("albacora_branca"),
          CATEGORIA == "Albacora_lage" ~ i18n$t("albacora_lage"),
          CATEGORIA == "Cacao_anequim" ~ i18n$t("cacao_anequim"),
          CATEGORIA == "Cacao_azul" ~ i18n$t("tubarao_azul"),
          CATEGORIA == "Meca" ~ i18n$t("meca"),
          CATEGORIA == "Outros" ~ i18n$t("outros"),
          CATEGORIA == "Prego" ~ i18n$t("prego"),
          TRUE ~ CATEGORIA
        ),
        "<br>", 
        i18n$t("legenda_MCV"), MediaKGMesViagem, "kg <br>"
      ),
      hoverlabel = list(
        font = list(
          size = 11 # Tamanho da fonte do hover
        )
      )
    ) %>%
      layout(
        xaxis = list(
          title = i18n$t("mes"),
          tickvals = unique(dados_graficoCaptura()$MES),
          ticktext = unique(dados_graficoCaptura()$MES),
          showgrid = F
        ),
        yaxis = list(
          title = i18n$t("legenda_CMV"),
          showgrid = F
        ),
        showlegend = FALSE,
        hovermode = "x",
        margin = list(t = 10, b = 40, l = 20, r = 20)
      )
  })
  
  dados_cap_desembarque <- reactive({
    dados_aux <- subset(
      dados_gerais,
      ANO >= input$anos_desembarque[1] & ANO <= input$anos_desembarque[2]
    )
    data.frame(dados_aux)
  })
  
  # Fazendo o cálculo da Média de Captura por Kg, por Viagem, por Mês/Ano
  dados_graficoAreaDesembarque <- reactive({
    dados_cap_desembarque() %>%
      mutate(KG_por_Viagem = (KG/DESCARGA)) %>% 
      # Agrupa os Dados por Colunas Selecionadas
      group_by(CATEGORIA, ANO, MES) %>% 
      # Média das Toneladas de Captura de Cada Grupo
      summarise(Media_KG_por_Viagem = mean(KG_por_Viagem)) %>%
      # Substituindo NAs por Zero
      mutate(Media_KG_por_Viagem = replace_na(Media_KG_por_Viagem, 0)) %>% 
      # Arredondando a Média de Toneladas para Duas Casas Decimais
      mutate(Media_KG_por_Viagem = round(Media_KG_por_Viagem, 2)) %>%
      mutate(mes_ano_formatado = make_date(ANO, MES)) %>%
      mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES)))) %>%
      mutate(mes_ano_formatado = format(mes_ano_formatado, "%Y-%m")) 
  })
  
  output$graficoAreaDesembarque <- renderPlotly({
    
    dados_captura <- dados_graficoAreaDesembarque()
    
    data <- data.frame(
      "mes_ano" = dados_captura$mes_ano,
      "mes_ano_formatado" = dados_captura$mes_ano_formatado,
      dados_captura)
    
    data_wide <- pivot_wider(
      data, names_from = CATEGORIA, values_from = Media_KG_por_Viagem)
    
    data_wide_filtrado <- data_wide %>% 
      dplyr::select(
        c(
          "mes_ano",
          "mes_ano_formatado",
          "Cacao_azul",
          input$especies_desembarque
        )
      )
    
    req(ncol(data_wide_filtrado) > 1)
    plot_data <- plot_ly(
      data = data_wide_filtrado,
      x = ~mes_ano_formatado,
      type = 'scatter',
      mode = 'none',
      stackgroup = 'one',
      groupnorm = 'percent',
      hoverinfo = "text",
      y = ~Cacao_azul,
      name = i18n$t("tubarao_azul"),
      fillcolor = "#377EB8",
      text = ~paste(
        " ", i18n$t("legenda_especie"), i18n$t("tubarao_azul"), "<br>",
        i18n$t("legenda_data"), mes_ano, "<br>",
        i18n$t("legenda_MCV"), Cacao_azul, "kg <br>"
      )
    )
    if(any(names(data_wide_filtrado) == "Albacora_bandolim")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_bandolim,
          name = i18n$t("albacora_bandolim"),
          fillcolor = "#F781BF",
          text = ~paste(
            " ", i18n$t("legenda_especie"), i18n$t("albacora_bandolim"), "<br>",
            i18n$t("legenda_data"), mes_ano, "<br>",
            i18n$t("legenda_MCV"), Albacora_bandolim, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Albacora_branca")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_branca,
          name = i18n$t("albacora_branca"),
          fillcolor = '#E41A1C',
          text = ~paste(
            " ", i18n$t("legenda_especie"), i18n$t("albacora_branca"), "<br>",
            i18n$t("legenda_data"), mes_ano, "<br>",
            i18n$t("legenda_MCV"), Albacora_branca, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Albacora_lage")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_lage,
          name = i18n$t("albacora_lage"),
          fillcolor = '#4DAF4A',
          text = ~paste(
            " ", i18n$t("legenda_especie"), i18n$t("albacora_lage"), "<br>",
            i18n$t("legenda_data"), mes_ano, "<br>",
            i18n$t("legenda_MCV"), Albacora_lage, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Cacao_anequim")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Cacao_anequim,
          name = i18n$t("cacao_anequim"),
          fillcolor = '#984EA3',
          text = ~paste(
            " ", i18n$t("legenda_especie"), i18n$t("cacao_anequim"), "<br>",
            i18n$t("legenda_data"), mes_ano, "<br>",
            i18n$t("legenda_MCV"), Cacao_anequim, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Meca")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Meca,
          name = i18n$t("meca"),
          fillcolor = '#FFFF33',
          text = ~paste(
            " ", i18n$t("legenda_especie"), i18n$t("meca"), "<br>",
            i18n$t("legenda_data"), mes_ano, "<br>",
            i18n$t("legenda_MCV"), Meca, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Outros")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Outros,
          name = i18n$t("outros"),
          fillcolor = '#FF7F00',
          text = ~paste(
            " ", i18n$t("legenda_especie"), i18n$t("outros"), "<br>",
            i18n$t("legenda_data"), mes_ano, "<br>",
            i18n$t("legenda_MCV"), Outros, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Prego")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Prego,
          name = i18n$t("prego"),
          fillcolor = '#A65628',
          text = ~paste(
            " ", i18n$t("legenda_especie"), i18n$t("prego"), "<br>",
            i18n$t("legenda_data"), mes_ano, "<br>",
            i18n$t("legenda_MCV"), Prego, "kg <br>"
          )
        )
    }
    plot_data <- plot_data %>% 
      layout(
        xaxis = list(
          title = "",
          tickvals = data_wide_filtrado$mes_ano_formatado[seq(
            1, length(data_wide_filtrado$mes_ano_formatado), by = 2)],
          ticktext = data_wide_filtrado$mes_ano_formatado[seq(
            1, length(data_wide_filtrado$mes_ano_formatado), by = 2)], 
          showgrid = FALSE
        ),
        yaxis = list(
          title = " ",
          showgrid = FALSE,
          ticksuffix = '%'
        ),
        showlegend = FALSE,
        hovermode = "x",
        margin = list(t = 0, b = 20, l = 20, r = 20)
      )
    plot_data
  })
  
  dados_desembarque_calor <- reactive({
    # Filtrando as Espécies 
    dados_aux <- subset(
      dados_ajustados, CATEGORIA %in% union(
        input$especies_desembarque, "Cacao_azul")
    )
    # Filtrando o Intervalo de Anos
    dados_aux <- subset(
      dados_aux,
      ANO >= input$anos_desembarque[1] & ANO <= input$anos_desembarque[2]
    )
    data.frame(dados_aux)
  })
  
  # Fazendo o cálculo da captura por mês
  dados_PesoMes_desembarque <- reactive({ 
    dados_desembarque_calor() %>%
      mutate(KG_por_Viagem = (KG/DESCARGA)) %>%
      complete(CATEGORIA, ANO, MES, fill = list(KG_por_Viagem = 0)) %>% 
      group_by(CATEGORIA, ANO, MES) %>%
      summarise(MedKGPorViagemMesAno = mean(KG_por_Viagem)) %>%
      mutate(MedKGPorViagemMesAno = replace_na(MedKGPorViagemMesAno, 0)) %>% 
      group_by(CATEGORIA, MES) %>%
      summarise(Media_KG_por_Viagem = mean(MedKGPorViagemMesAno)) %>%
      mutate(Media_KG = round(Media_KG_por_Viagem, 2)) %>%
      mutate(
        CATEGORIA = case_when(
          CATEGORIA == "Albacora_bandolim" ~ i18n$t("albacora_bandolim"),
          CATEGORIA == "Albacora_branca" ~ i18n$t("albacora_branca"),
          CATEGORIA == "Albacora_lage" ~ i18n$t("albacora_lage_quebra_linha"),
          CATEGORIA == "Cacao_anequim" ~ i18n$t("cacao_anequim_quebra_linha"),
          CATEGORIA == "Cacao_azul" ~ i18n$t("tubarao_azul"),
          CATEGORIA == "Meca" ~ i18n$t("meca"),
          CATEGORIA == "Outros" ~ i18n$t("outros"),
          CATEGORIA == "Prego" ~ i18n$t("prego"),
          TRUE ~ CATEGORIA
        )
      )
  })
  
  output$pesoMes <- renderPlotly({
    nomes_meses <- nomes_meses_reativo()
    plot_ly(
      data = dados_PesoMes_desembarque(),
      x = ~MES,
      y = ~CATEGORIA,
      z = ~Media_KG,
      type = "heatmap",
      colorscale = "Plasma",
      hoverinfo = "text",
      text = ~paste(
        " ", i18n$t("legenda_mes"), nomes_meses[MES], "<br>",
        i18n$t("legenda_categoria"), CATEGORIA, "<br>",
        i18n$t("legenda_MCV"), Media_KG, "kg <br>"
      ),
      colorbar = list(
        title = list(
          text = i18n$t("captura_media")
        )
      )
    ) %>%
      layout(
        xaxis = list(
          title = i18n$t("mes"),
          tickvals = unique(dados_PesoMes_desembarque()$MES), 
          ticktext = unique(dados_PesoMes_desembarque()$MES)
        ),
        yaxis = list(title = ""),
        legend = list(
          orientation = "h",
          y = 0.9,
          x = 0.1,
          font = list(
            size = 10
          )
        ),
        showlegend = F, 
        margin = list(t = 10, b = 40, l = 20, r = 20)
      )
  })
  
  # Distribuição Espacial das Capturas --------------------------------------
  
  db_filtrado <- reactive({
    dados_aux <- subset(
      dados_ajustados,
      CATEGORIA %in% union(input$especies_cap_esp, "Cacao_azul")
    )
    dados_aux <- subset(
      dados_aux,
      ANO >= input$anos_cap_esp[1] & ANO <= input$anos_cap_esp[2]
    )
    tab01 <- dados_aux %>%
      group_by(LON, LAT) %>%
      summarise(
        prod = sum(KG),
        prod2 = sum(KG)/sum(DESCARGA),
        viagem = sum(DESCARGA)
      ) %>%
      ungroup()
    list(dados = dados_aux, tab01 = tab01)
  })
  
  output$Mapas <- renderLeaflet({
    tab01 <- db_filtrado()$tab01
    
    # Cálculo dos quantis para categorizar os dados do mapa de calor
    breaksTotal <- quantile(tab01$prod, probs = seq(0, 1, 0.1), na.rm = TRUE)
    
    # Verificando se há breaks duplicados
    if (any(duplicated(breaksTotal))) {
      # Jitter é usado para variar um pouco o valor dos duplicados
      tab01$prod <- jitter(tab01$prod, factor = 0.1)
    }
    
    # Criar a paleta de cores com base nos intervalos
    pal <- colorQuantile(
      palette = colorRampPalette(brewer.pal(9, "Blues"))(10),
      domain = tab01$prod,
      probs = seq(0, 1, 0.1)
    )
    
    Mapa_Capturas_Totais <- leaflet() %>%
      # Definindo a primeira opção do estilo do Mapa (Claro)
      addProviderTiles(
        providers$CartoDB.Positron,
        group = i18n$t("mapa_claro")
      ) %>%
      # Definindo a segunda opção do estilo do Mapa (Escuro)
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = i18n$t("mapa_escuro")
      ) %>%
      # Definindo a Posição Inicial da visão sobre o Mapa
      setView(
        lng = -40,
        lat = -28,
        zoom = 4
      ) %>%
      # Definindo a adição dos Marcadores no Mapa
      addCircleMarkers(
        group = tab01$prod,      # Define os marcadores com base na soma dos KG
        radius = 7,
        lng = tab01$LON,         # Define tab01$LON como longitude
        lat = tab01$LAT,         # Define tab01$LAT como latitude
        stroke = FALSE,          # Define que não haverá borda dos marcadores
        color = pal(tab01$prod), # Define a paleta de cores dos marcadores
        fillOpacity = 0.7,       # Define a opacidade dos marcadores como 70%
        label = lapply(paste0(
          i18n$t("mapa_legenda_1"),
          round(tab01$prod, 0), 
          i18n$t("mapa_legenda_2"),
          tab01$viagem
        ), HTML)
      ) %>%
      # Definindo a legenda com a paleta de cores e suas Porcentagens
      addLegend(
        pal = pal,
        values = tab01$prod,
        group = tab01$prod,
        position = "bottomright",
        title = i18n$t("mapa_legenda_3")
      ) %>%
      # Controle de Estilo de Mapa
      addLayersControl(
        position = "topleft",
        baseGroups = c(
          i18n$t("mapa_escuro"),
          i18n$t("mapa_claro")
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      # Adicionando Mini Mapa
      addMiniMap(
        position = "bottomleft",
        toggleDisplay = T
      ) %>%
      # Adicionando um Medidor
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        secondaryAreaUnit = "hectares",
        localization = "pt_br"
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      ) %>%
      addFullscreenControl(
        position = "topright"
      ) %>%
      addResetMapButton() %>%
      setMaxBounds(
        lng1 = -180, lat1 = -90,  # Limite inferior esquerdo
        lng2 = 180, lat2 = 90     # Limite superior direito
      )
    
    breaksPorViagem <- quantile(
      tab01$prod2, 
      probs = seq(0, 1, 0.1), 
      na.rm = TRUE
    )
    
    if (any(duplicated(breaksPorViagem))) {
      tab01$prod2 <- jitter(tab01$prod2, factor = 0.1)
    }
    
    pal <- colorQuantile(
      palette = colorRampPalette(brewer.pal(9, "Blues"))(10),
      domain = tab01$prod2,
      probs = seq(0, 1, 0.1)
    )
    
    Mapa_kg_por_viagem <- leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        i18n$t("mapa_claro")
      ) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        i18n$t("mapa_escuro")
      ) %>%
      setView(
        lng = -40, lat = -28, zoom = 4
      ) %>%
      addCircleMarkers(
        group = "Marcadores Circulares",
        radius = 7,
        lng = tab01$LON,
        lat = tab01$LAT, 
        stroke = FALSE,
        color = pal(tab01$prod2),
        fillOpacity = 0.7,
        label = paste0(
          i18n$t("mapa_legenda_1"),
          round(tab01$prod2, 0), " kg"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = tab01$prod2,
        group = tab01$prod,
        position = "bottomright", 
        title = i18n$t("mapa_legenda_3")
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c(
          i18n$t("mapa_escuro"),
          i18n$t("mapa_claro")
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addMiniMap(
        position = "bottomleft",
        toggleDisplay = T
      ) %>%
      # Adicionando um Medidor
      addMeasure(
        position = "bottomleft"
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      ) %>%
      addFullscreenControl(
        position = "topright"
      ) %>%
      addResetMapButton() %>%
      setMaxBounds(
        lng1 = -180, lat1 = -90,
        lng2 = 180, lat2 = 90
      )
    
    breaksViagem <- quantile(
      tab01$viagem, 
      probs = seq(0, 1, 0.1), 
      na.rm = TRUE
    )
    
    if (any(duplicated(breaksViagem))) {
      tab01$viagem <- jitter(tab01$viagem, factor = 0.1)
    }
    
    pal <- colorQuantile(
      palette = colorRampPalette(brewer.pal(9, "Blues"))(10),
      domain = tab01$viagem,
      probs = seq(0, 1, 0.1)
    )
    
    Mapa_Viagem <- leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        group = i18n$t("mapa_claro")
      ) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = i18n$t("mapa_escuro")
      ) %>%
      setView(
        lng = -40, lat = -28, zoom = 4
      ) %>%
      addCircleMarkers(
        group = tab01$viagem,
        radius = 7,
        lng = tab01$LON,
        lat = tab01$LAT,
        stroke = FALSE,
        color = pal(tab01$viagem), 
        fillOpacity = 0.7,
        label = paste0(
          i18n$t("mapa_legenda_5"),
          round(tab01$viagem),0)
      ) %>%
      addLegend(
        pal = pal, 
        values = tab01$viagem,
        group = tab01$viagem,
        position = "bottomright", 
        title = i18n$t("mapa_legenda_4")
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c(
          i18n$t("mapa_escuro"),
          i18n$t("mapa_claro")
        ),
        options =
          layersControlOptions(collapsed = FALSE)
      ) %>%
      addMiniMap(
        position = "bottomleft",
        toggleDisplay = T
      ) %>%
      addMeasure(
        position = "bottomleft"
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      ) %>%
      addFullscreenControl(
        position = "topright"
      ) %>%
      addResetMapButton() %>%
      setMaxBounds(
        lng1 = -180, lat1 = -90,
        lng2 = 180, lat2 = 90
      )
    
    if (input$mapa_cap_esp == "KilosTotais") {
      Mapa_Capturas_Totais
    } else if (input$mapa_cap_esp == "KiloPorViagem") {
      Mapa_kg_por_viagem
    } else if (input$mapa_cap_esp == "Viagens") {
      Mapa_Viagem
    }
    
  })
  
  output$textBoxSidebar <- renderUI({
    if (input$mapa_cap_esp == "KilosTotais") {
      p(tagList(i18n$t("captura_espacial_texto_2")))
    } else if (input$mapa_cap_esp == "KiloPorViagem") {
      p(tagList(i18n$t("captura_espacial_texto_3")))
    } else if (input$mapa_cap_esp == "Viagens") {
      p(tagList(i18n$t("captura_espacial_texto_4")))
    }
  })
  
  output$MapaComprimento <- renderLeaflet({
    leaflet() %>%
      # Definindo a primeira opção do estilo do Mapa (Claro)
      addProviderTiles(
        providers$CartoDB.Positron,
        group = i18n$t("mapa_claro")
      ) %>%
      # Definindo a segunda opção do estilo do Mapa (Escuro)
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = i18n$t("mapa_escuro")
      ) %>%
      # Definindo a Posição Inicial da visão sobre o Mapa
      setView(
        lng = -40, lat = -28, zoom = 5
      )%>%
      # Controle de Estilo de Mapa
      addLayersControl(
        position = "topleft",
        baseGroups = c(
          i18n$t("mapa_escuro"),
          i18n$t("mapa_claro")
        ),
        options =
          layersControlOptions(collapsed = FALSE)
      ) %>%
      # Adicionando Mini Mapa
      addMiniMap(
        position = "bottomleft"
      ) %>%
      # Adicionando um Medidor 
      addMeasure(
        position = "bottomleft"
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      ) %>%
      addFullscreenControl(
        position = "topright"
      ) %>%
      addResetMapButton() %>%
      setMaxBounds(
        lng1 = -180, lat1 = -90,  # Limite inferior esquerdo
        lng2 = 180, lat2 = 90     # Limite superior direito
      )
  })
  
  # Distribuição de Comprimentos --------------------------------------------
  
  dados_falsos_filtro <- reactive({
    if (input$sexo_comprimento == "Todos") {
      dados_falsos
    } else {
      dados_falsos[dados_falsos$Sexo == input$sexo_comprimento, ]
    }
  })
  
  output$histograma_comprimento <- renderPlotly({
    plot_ly(
      data = dados_falsos_filtro(),
      x = ~IDL,
      type = 'histogram',
      marker = list(
        line = list(
          color = 'black',  # Cor do contorno
          width = 1         # Espessura do contorno
        )
      )
    ) %>% 
      layout(
        title = NULL,
        yaxis = list(
          title = i18n$t("frequencia_relativa"),
          showgrid = FALSE,
          ticksuffix = '%'
        ),
        showlegend = FALSE
      )
  })
  
  output$boxplot_comprimento <- renderPlotly({
    plot_ly(
      data = dados_falsos_filtro(),
      y = ~IDL,
      type = "box",
      marker = list(color = "primary")
    ) %>% 
      layout(
        title = NULL,
        hovermode = "x",
        xaxis = list(
          title = NULL
        ),
        yaxis = list(
          title = i18n$t("legenda_comprimento")
        )
      )
  })
  
  # Tabela de Embarcações ---------------------------------------------------
  
  output$tabela_embarcacoes <- renderDT({
    
    colnames(notificacoesTabela) <- c(
      "Status",
      tagList(i18n$t("tabela_embarcacoes_nome_1")), 
      tagList(i18n$t("tabela_embarcacoes_nome_2")),
      tagList(i18n$t("tabela_embarcacoes_nome_3")),
      tagList(i18n$t("tabela_embarcacoes_nome_4")),
      tagList(i18n$t("tabela_embarcacoes_nome_5")),
      tagList(i18n$t("tabela_embarcacoes_nome_6")),
      tagList(i18n$t("tabela_embarcacoes_nome_7")),
      "DiasRestantes"
    )
    
    tabela_filtrada <- if (input$status_tabela == "Todos") {
      notificacoesTabela
    } else {
      notificacoesTabela[notificacoesTabela$Status == input$status_tabela, ]
    }
    
    datatable(
      tabela_filtrada[, !names(tabela_filtrada) %in% c(
        "DiasRestantes", "Status"
      )],
      rownames = FALSE,
      filter = "none",
      options = list(
        paging = T,
        searching = FALSE,
        pageLength = 8,
        lengthMenu = list(c(5, 8, 10, 15, -1),c('5','8', '10', '15', 'all')),
        columnDefs = list(
          list(
            className = 'dt-center',
            targets = "_all"
          ),
          list(
            targets = 1:4,
            render = JS(
              "function(data, type, row) {",
              "  return data ? new Date(data).toLocaleDateString('pt-BR'):'';",
              "}"
            )
          )
        ),
        order = list(list(3, 'desc'))
      ),
      class = "cell-border stripe hover",
      selection = "single"
    )
  })
  
  # ControlBar --------------------------------------------------------------
  
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(
      session, 
      inputId = "species",
      selected = unique(dados_ajustados$CATEGORIA)
    )
  })
  
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(
      session, 
      inputId = "species",
      selected = character(0)
    )
  })
  
  observeEvent(input$selected_language, {
    update_lang(input$selected_language)
  })
}

# Inicialização da aplicação Shiny
shinyApp(ui, server)
