# Bibliotecas -------------------------------------------------------------

# Carregando os Pacotes que serão utilizados no dashboard
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus,
  leaflet, leaflet.extras,
  dplyr, tidyverse, scales, zoo, DT,
  plotly, shinyjs,
  raster, readxl, digest,
  DBI, RSQLite
)

# # Ativa o modo de desenvolvimento do Shiny, desativando cache e 
# # facilitando o debug.
# options(shiny.devmode = TRUE)
# 
# # Habilita o recarregamento automático do aplicativo ao detectar mudanças.
# options(shiny.autoreload = TRUE)
# 
# # Define o intervalo de 0,5 segundos entre as verificações de mudanças 
# # no código para o recarregamento automático.
# options(shiny.autoreload.interval = 0.5)
# 
# # Desativa a minificação de arquivos CSS e JavaScript, útil para depuração.
# options(shiny.minified = FALSE)
# 
# # Define que, ao ocorrer um erro, o R entra no modo de navegação
# # de erro (browser) para facilitar o debug.
# # options(shiny.error = browser)
# 
# # Desativa a sanitização das mensagens de erro, permitindo que detalhes 
# # completos sejam exibidos.
# options(shiny.sanitize.errors = FALSE)
# 
# # Exibe a stack trace completa quando um erro ocorre, ajudando na 
# # identificação do ponto exato do problema.
# options(shiny.fullstacktrace = TRUE)
# 
# # Habilita o log de reatividade, permitindo a visualização das 
# # interações entre valores reativos no Shiny.
# options(shiny.reactlog = TRUE)
# 
# # Ativa o modo de teste do Shiny, que pode desabilitar certas 
# # funcionalidades não necessárias para desenvolvimento ou testes.
# options(shiny.testmode = TRUE)

db <- dbConnect(SQLite(), "dados_brutos/database.sqlite")
on.exit(dbDisconnect(db))

notificacoes <- dbReadTable(db, "Notificacoes")
dados <- dbReadTable(db, "Dados")
dados_falsos <- dbReadTable(db, "Dados_falsos")

# dbWriteTable(
#   db,
#   "Notificacoes",
#   data.frame(
#     id = c(10, 11, 12),
#     Titulo = c("Embarcação de Prego", "Embarcação de Tubarão Azul", "Embarcação de Peixes"),
#     Local = c("Itajaí","Itajaí","Itajaí"),
#     Data = format(as.Date(c("2024-08-27", "2024-08-28", "2024-08-29")), "%Y-%m-%d"),
#     Hora = c("14", "15", "15"),
#     Minuto = c("30", "30", "45"),
#     Link = c(NA, NA, NA),
#     TextoOpcional = c(NA, NA, NA)
#   ),
#   append = TRUE,
#   row.names = FALSE
#   )
# 
# dbExecute(db, "DELETE FROM Notificacoes WHERE id IN (10, 11, 12)")

# # Carregando os Dados de um Arquivo csv
# dados <- read.table(
#   "dados_brutos/dados_17_24_Outros.csv", header = TRUE, sep = ",", dec = "."
# )
# Ajuste nos nomes das CATEGORIAS
# dados_ajustados <- dados %>% 
#   mutate(
#     CATEGORIA = case_when(
#       CATEGORIA == "Albacora-bandolim" ~ "Albacora_bandolim",
#       CATEGORIA == "Albacora-branca" ~ "Albacora_branca",
#       CATEGORIA == "Albacora-lage" ~ "Albacora_lage",
#       CATEGORIA == "Cacao-anequim" ~ "Cacao_anequim",
#       CATEGORIA == "Cacao-azul" ~ "Cacao_azul",
#       CATEGORIA == "Meca" ~ CATEGORIA,
#       CATEGORIA == "Outros" ~ CATEGORIA,
#       CATEGORIA == "Prego" ~ CATEGORIA, 
#       TRUE ~ CATEGORIA
#     )
#   )

# Dicionário para substituição de categorias
categoria_substituicoes <- c(
  "Albacora-bandolim" = "Albacora_bandolim",
  "Albacora-branca" = "Albacora_branca",
  "Albacora-lage" = "Albacora_lage",
  "Cacao-anequim" = "Cacao_anequim",
  "Cacao-azul" = "Cacao_azul"
)

dados_ajustados <- dados %>%
  mutate(CATEGORIA = recode(CATEGORIA, !!!categoria_substituicoes))

# dados_falsos <- read.table(
#   "dados_falsos.csv", header = TRUE, sep = ",", dec = "."
# )

# notificacoes <- read_excel("dados_brutos/Notificacoes.xlsx")

notificacoes <- notificacoes %>%
  # mutate(Data = format(as.Date(Data), "%Y-%m-%d")) %>%
  mutate(Horário = sprintf("%02d:%02d", Hora, Minuto)) %>%
  plotly::select(-c(Hora, Minuto))

verifica_coluna <- function(df, coluna) {
  if (!any(names(df) == coluna)) {
    message(paste("Coluna", coluna, "não encontrada."))
  }
  return(any(names(df) == coluna))
}

check_password <- function(input_password, filename) {
  if (!file.exists(filename)) {
    stop("Arquivo de senha não encontrado.")
  }

  hashed_password <- readLines(filename)
  input_hash <- digest(input_password, algo = "sha256", serialize = FALSE)

  identical(input_hash, hashed_password)
}


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
           ')
    )
  ),
  skin = "blue", # Definindo a cor do tema do Painel
  scrollToTop = TRUE,
  # Header ------------------------------------------------------------------
  
  # Definindo a Header do Painel
  header = dashboardHeader(
    titleWidth = 300,
    # Definição do Título com Link da Header
    title = tags$a( # Cria uma tag que define um hyperlink
      href = "https://lrpdc.shinyapps.io/proj_tubarao_azul/", # Link URL
      target = "_blank", # Abre o link em uma nova aba
      # Cria uma tag que é um contêiner em linha usado para aplicar estilos
      tags$span(
        # Saída de Icon ou Título depende da situação do sidebar
        uiOutput("textoHeader")
      ),
      class = "logo"
    ),
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
          $('.treeview-menu').css('width', '126px');
        } else {
          $('.treeview-menu').css('width', 'auto');
        }
      });
                     ")
    ),
    tags$head(tags$style(HTML(' 
    .sidebar-mini:not(.sidebar-mini-expand-feature).sidebar-collapse 
    .sidebar-menu>li:hover>a>span:not(.pull-right) {
      width: auto !important; 
      padding-right: 2rem;
      padding-left: 2rem;
    }
                              ')
                         )
              ),
    width = 300, # Definição da Largura em pixels
    minified = TRUE,  # Se a aba lateral ao ser fechada deverá mostrar os ícones
    collapsed = TRUE, # Se a aba lateral deve ser iniciada fechada
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
      # Definindo o item do Menu de Distribuição de Captura
      menuItem(
        text = "Distribuição de captura",
        tabName = "tab2header",
        icon = icon("chart-pie")
      ),
      # Definindo o item do Menu de Desembarques
      menuItem(
        text = "Desembarques",
        tabName = "tab3header",
        icon = icon("chart-area")
        # icon = icon("ship")
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
      menuItem(
        text = "Distribuição de comprimentos",
        tabName = "tab6header",
        icon = icon("chart-simple")
      ),
      # menuItem(
      #   text = "Mapa de distribuição de comprimentos",
      #   tabName = "tab7header",
      #   icon = icon("map")
      # ),
      menuItem(
        text = "Tabela de embarcações",
        tabName = "tab8header",
        icon = icon("ship"),
        badgeLabel = nrow(notificacoes),
        badgeColor = "red"
      ),
      menuItem(
        text = "Perguntas",
        icon = icon("circle-question"),
        badgeLabel = "new",
        badgeColor = "aqua",
        tabName = "tab9header"
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
    .mapa {
    display: flex;
    width: 100%;
    # height: calc(86vh - 155px);
    height: calc(100vh - 145px);
    visibility: inherit;
    position: relative;
    z-index: 100;
    }
    
    .graficos {
    display: flex;
    width: 100%;
    # height: calc(43vh - 119px);
    height: calc(50vh - 120px);
    # height: calc(100vh - 120px);
    visibility: inherit;
    position: relative;
    z-index: 100;
    }
    
    .graficosMaiores {
    display: flex;
    width: 100%;
    # height: calc(43vh - 119px);
    # height: calc(50vh - 120px);
    height: calc(90vh - 120px);
    visibility: inherit;
    position: relative;
    z-index: 100;
    }
    
    .direct-chat-contacts {
      z-index: 100 !important;
    }
    
    .content-wrapper {
      background-color: #FFFFFF; /* cor de fundo branca */
    }
    
    #LogoPTA img {
      min-width: 200px;   /* Defina a largura mínima desejada */
      max-width: 400px;   /* Defina a largura máxima desejada */
    }
    
    # .box-header .box-title{
    #   font-size: 18px;
    #   # font-size: 15px;
    #   # Tentar alinhar o título, mas ver o que é melhor
    # }
                              ')
                         )
              ),
    tabItems(
      # Definindo o conteúdo do Projeto
      tabItem(
        tabName = "tab1body",
        # Cria uma página com layout fluido
        fluidPage(
          # Cria uma página com layout fixo
          fluidRow(
            # Cria uma coluna dentro de uma definição da Interface do Usuário
            column(
              offset = 2, # Define o deslocamento de 2 colunas à esquerda
              width = 9,  # Define a largura como 9 unidades de largura
              # Criando um carrossel de infoBox
              carousel(
                width = 12,
                id = "mycarousel",
                indicators = FALSE,       # Se haverá setas para troca de item
                # Item do Carrossel
                carouselItem(
                  # Caixa de Informações
                  infoBox(
                    title = "Tubarões Medidos",
                    fill = TRUE,          # Se a infoBox deve ser preenchida
                    width = 10,           # Definindo a largura da infoBox
                    color = "light-blue", # Definindo cor da infoBox
                    # Definindo Configurações do conteúdo da infoBox
                    value = tags$div(
                      style = "display: block; text-align: center;", 
                      # Cria uma tag de cabeçalho HTML
                      h1(
                        # Cria uma tag que deixa o texto em negrito
                        strong("28954")
                      )
                    ),
                    icon = icon("fish"),
                    # Cria uma tag que insere uma quebra de linha
                    br()
                  )
                ),
                carouselItem(
                  infoBox(
                    title = "Entrevista de Desembarque",
                    fill = TRUE,
                    width = 10,
                    color = "light-blue", 
                    value = tags$div(
                      style = "display: block; text-align: center;", 
                      h1(strong("731"))
                    ),
                    icon = icon("paste"),
                    br()
                  )
                ),
                carouselItem(
                  infoBox(
                    title = "Cadernos de Bordo",
                    fill = TRUE,
                    width = 10,
                    color = "light-blue",
                    value = tags$div(
                      style = "display: block; text-align: center;", 
                      h1(strong("465"))
                    ),
                    icon = icon("book-open"),
                    br()
                  )
                ),
                carouselItem(
                  infoBox(
                    title = "Embarcações Monitoradas",
                    fill = TRUE,
                    width = 10,
                    color = "light-blue",
                    value = tags$div(
                      style = "display: block; text-align: center;",
                      h1(strong("92"))
                    ),
                    icon = icon("sailboat"),
                    br()
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              div(
                style = "text-align: center;",
                tags$a(
                  href = "https://demersais.furg.br/projeto-tubarão-azul.html",
                  target = "_blank",
                  imageOutput("LogoPTA")
                )
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
              # Cria uma tag que define um parágrafo de texto 
              p("O tubarão-azul ",
                # Cria uma tag que enfatiza o texto
                tags$em("Prionace glauca", .noWS = "after"),
                " é um dos Tubarões mais abundantes e de mais ampla 
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
                         comerciais, Tubarões azuis continuariam sendo 
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
                  elaboração de um Plano de Gestão da Pesca do Tubarão-azul no
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
              h3("Fluxograma do Plano de Gestão da pesca de Tubarão azul 
                   no Rio Grande do Sul"),
              div(
                style = "text-align: center;",
                # Saída da Imagem do Fluxograma do Projeto
                imageOutput("FluxogramaTubAzul")
              )
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
              width = 10,
              offset = 1,
              tags$head(
                tags$style(HTML("
                  #boxWithoutHeader .box-header {
                    display: none;
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
                p("Prezado Usuário,"),
                p("Esta plataforma foi desenvolvida para disponibilizar 
                  informações atualizadas sobre as capturas de Tubarão azul e 
                  da frota de espinhel pelágico que vêm sendo coletadas pela 
                  equipe do projeto Tubarão Azul. Os dados são referentes à 
                  desembarques realizados pela frota no porto de Rio Grande,
                  RS."),
                p("A espécie Tubarão azul, por ser o foco do dashboard, estará
                  sempre selecionada."),
                p("Na aba “Distribuição de captura”, você encontrará gráficos
                  que mostram a quantidade de dados registrados de Tubarão azul
                  em comparação com outras espécies e a distribuição de dados
                  de Tubarão azul por mês."),
                p("Na aba “Desembarques”, serão visualizadas as capturas
                  mensais médias por viagem para todas as espécies,
                  discriminadas também por espécie, e a distribuição do peso 
                  total capturado por mês."),
                p("Na aba “Distribuição espacial das capturas”, você encontrará 
                  os lances de pesca realizados distribuídos espacialmente para 
                  todas as espécies, discriminados também por espécie."),
                p("Na aba “Administrador”, encontra-se uma tabela contendo os
                  dados utilizados nas visualizações de dados. O acesso a essa
                  tabela é restrito aos administradores, os quais devem 
                  fornecer uma senha para visualizar essas informações."),
                p("Para a construção dos gráficos apresentados nesta plataforma
                  são utilizados dados atualizados anualmente."),
                p("Para maiores informações, por favor, entre em contato através
                  do e-mail ",strong("proj.tubaraoazul.furg@gmail.com."))
              )
            )
          )
        )
      ),
      # Definindo o conteúdo da Distribuição de Captura
      tabItem(
        tabName = "tab2header",
        fluidPage(
          fluidRow(
            column(
              width = 12,
              # Definindo Caixa com conteúdo da Distribuição de Captura
              box(
                # title = "Gráfico de Área Relativa",
                title = "Dados Registrados por Mês, Ano e Categoria",
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
                  p("Este gráfico de área relativa, apresenta a quantidade 
                    de dados registrados por mês/ano, divididos por categoria 
                    de pesca. Cada barra representa um mês/ano, com segmentos 
                    empilhados que correspondem às diferentes categorias de 
                    pesca. Isso permite uma comparação direta entre as 
                    categorias ao longo do tempo, destacando as variações 
                    mensais/ano na distribuição dos dados de pesca.")
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              box(
                # title = "Gráfico de Barras",
                title = "Comparação de Dados Registrados por Mês",
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
                  p("Este gráfico de barra, compara a presença de Tubarão azul
                    com a categoria 'Outros', que representa dados de todas as
                    outras espécies de pesca. Ele mostra a proporção de dados de
                    Tubarão azul comparada com as demais categorias, por mês")
                )
              )
            ),
            column(
              width = 6,
              box(
                title = "Comparação de Dados Registrados por Mês/Ano",
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
                  p("Este mapa de calor compara os dados de Tubarão Azul obtidos
                    em cada mês e ano. Cada quadrado representa um mês de um ano
                    específico, mostrando a distribuição proporcional dos dados
                    ao longo do período analisado, permitindo visualizar 
                    variações sazionais ou tendências.")
                )
              )
            )
          )
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
                # title = "Gráfico de Linhas",
                title = "Média Mensal de Captura por Viagem",
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
                  p("Este gráfico de linha mostra a captura média em quilos por
                    viagem, distribuída por mês e categorizada por tipo de 
                    peixe. Cada barra representa a média mensal de capturas,
                    destacando a variação ao longo do tempo e entre diferentes 
                    categorias de pesca.")
                )
              )
            ),
            column(
              width = 6,
              box(
                title = "Média Mensal de Captura por Viagem",
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
                  p("Este mapa de calor ilustra a composição de espécies 
                    presente nos dados de pesca, indicando a porcentagem de 
                    cada espécie em relação ao total. Cada linha do mapa 
                    representa uma espécie, facilitando a visualização das 
                    diferenças da captura média por espécie.")
                )
              )
            ) 
          ),
          fluidRow(
            column(
              width = 12,
              box(
                # title = "Gráfico de Área Relativa",
                title = 'Média Mensal de Captura por Viagem ao Longo do Período',
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
                  p("Este gráfico de área relativa, apresenta a captura média 
                    em quilos por viagem, categorizada por tipo de peixe, para
                    cada mês/ano no período analisado. As diferentes cores
                    representam distintas categorias de pesca, permitindo uma 
                    comparação clara e imediata entre os meses e anos, bem como 
                    entre as categorias de peixe.")
                )
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
                width = 12,
                title = "Mapa de Capturas (KG Total)",
                solidHeader = TRUE,
                status = "primary",
                closable = T,
                div(
                  class = "mapa",
                  # Saída do Gráfico do Mapa de Calor
                  leafletOutput("MapaCaptura",height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar9",
                  icon = icon("circle-info"),
                  width = 30,
                  background = "#A6ACAFEF",
                  p("Este mapa de calor mostra a localização das capturas, com o
                    valor total de Quilos capturados, onde a cor dos círculos
                    varia de verde a roxo, indicando a porcentagem de capturas
                    em cada área. As áreas com uma porcentagem menor de capturas
                    são representadas em tons mais claros de verde, enquanto
                    áreas com uma porcentagem maior são exibidas em tons mais
                    escuros de roxo. Isso permite visualizar facilmente as
                    áreas com maior e menor concentração de capturas.")
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                title = "Mapa de Capturas (Kg por Viagem)",
                solidHeader = TRUE,
                status = "primary",
                closable = T,
                div(
                  class = "mapa",
                  # Saída do Gráfico do Mapa de Calor
                  leafletOutput("MapaCapturaPorViagem",height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar91",
                  icon = icon("circle-info"),
                  width = 30,
                  background = "#A6ACAFEF",
                  p("Este mapa de calor mostra a localização das capturas,com o
                    valor em Quilos por Viagem, onde a cor dos círculos varia de
                    verde a roxo, indicando a porcentagem de capturas em cada 
                    área. As áreas com uma porcentagem menor de capturas são 
                    representadas em tons mais claros de verde, enquanto áreas 
                    com uma porcentagem maior são exibidas em tons mais escuros 
                    de roxo. Isso permite visualizar facilmente as áreas com 
                    maior e menor concentração de capturas.")
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                title = "Mapa de Capturas (Kg por Viagem)",
                solidHeader = TRUE,
                status = "primary",
                closable = T,
                div(
                  class = "mapa",
                  # Saída do Gráfico do Mapa de Calor
                  leafletOutput("MapaCapturaPorViagem2", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar92",
                  icon = icon("circle-info"),
                  width = 30,
                  background = "#A6ACAFEF"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                title = "Mapa de Capturas (Kg por Viagem)",
                solidHeader = TRUE,
                status = "primary",
                closable = T,
                div(
                  class = "mapa",
                  # Saída do Gráfico do Mapa de Calor
                  leafletOutput("MapaCapturaPorViagem3", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar93",
                  icon = icon("circle-info"),
                  width = 30,
                  background = "#A6ACAFEF"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                width = 12,
                title = "Mapa de Viagens",
                solidHeader = TRUE,
                status = "primary",
                div(
                  class = "mapa",
                  # Saída do Gráfico do Mapa de Calor
                  leafletOutput("MapaViagens", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar94",
                  icon = icon("circle-info"),
                  width = 30,
                  background = "#A6ACAFEF"
                )
              )
            )
          )
        )
      ),
      # Definindo o conteúdo do Administrador
      tabItem(
        tabName = "tab5header",
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
      ),
      tabItem(
        tabName = "tab6header",
        fluidRow(
          box(
            title = "Dados Falsos"
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              title = "Histograma de Comprimento",
              status = "primary",
              flipBox(
                id = "teste",
                width = 12,
                front = plotlyOutput("histograma_comprimentoM"),
                back = plotlyOutput("histograma_comprimentoF"),
                trigger = "click"
              ),
              sidebar = boxSidebar(
                id = "boxsidebar10",
                icon = icon("circle-info"),
                width = 50,
                background = "#A6ACAFEF",
                p("Esta é uma flipBox, que contém os histogramas do comprimento
                  de Tubarões azul machos e fêmeas. Que indica a distribuição de
                  comprimento por intervalos específicos, que estão em 
                  centímetros. Para ver o outro histograma é necessário clicar 
                  no gráfico.")
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              title = "Diagrama de Caixa",
              status = "primary",
              plotlyOutput("boxplot_comprimento"),
              sidebar = boxSidebar(
                id = "boxsidebar11",
                icon = icon("circle-info"),
                width = 50,
                background = "#A6ACAFEF",
                p("Esta é uma boxplot do comprimento de Tubarões azul machos e
                  fêmeas. Ela indica 5 dados, o mínimo, o primeiro quartil (Q1),
                  a mediana (Q2), o terceiro quartil (Q3), e o máximo. Os 
                  círculos fora da linha que se estendem a partir da caixa, são
                  os outliers")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tab7header",
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              solidHeader = T,
              title = "Mapa de Capturas",
              status = "primary",
              div(
                class = "mapa",
                # Saída do Gráfico do Mapa de Calor
                leafletOutput("MapaComprimento",height = "100%")
              ),
              sidebar = boxSidebar(
                id = "boxsidebar12",
                icon = icon("circle-info"),
                width = 30,
                background = "#A6ACAFEF",
                p("")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tab8header",
        fluidRow(
          column(
            width = 12,
            # uiOutput("lista_embarcacoes")
            DTOutput("tabela_embarcacoes")
          )
        )
      ),
      tabItem(
        tabName = "tab9header",
        box(
          width = 6,
          solidHeader = T,
          title = "Qual é o Mês com que tem mais capturas Totais (KG)?",
          status = "primary",
          plotlyOutput("BarrasKGMes")
        ),
        box(
          width = 6,
          solidHeader = T,
          title = "Qual é o Mês com que tem mais capturas Médias por Viagem (KG)?",
          status = "primary",
          plotlyOutput("BarrasKGMediaMes")
        ),
        # box(
        #   width = 6,
        #   solidHeader = T,
        #   # title = "Qual é o Mês com que tem mais capturas Totais (KG)?",
        #   status = "primary"
        # ),
        box(
          width = 12,
          solidHeader = T,
          title = "Quilos Totais por Mês",
          status = "primary",
          plotlyOutput("BarrasKGMesAno")
        ),
        box(
          width = 12,
          solidHeader = T,
          title = "Quilos por Viagem, por Mês",
          status = "primary",
          plotlyOutput("BarrasKGMesAnoMedia")
        ),
        box(
          width = 12,
          solidHeader = T,
          title = "Média de Quilos por Espécie, por Mês",
          status = "primary",
          plotlyOutput("BarrasKGMediaPorEspecie")
        ),
        box(
          width = 12,
          solidHeader = T,
          title = "Viagens por Mês",
          status = "primary",
          plotlyOutput("BarrasViagemPorMes")
        )
      )
    )
  ),
  footer = dashboardFooter(
    left = list(
      fluidRow(
        tags$div(
          style = "margin-left: 20px;
          margin-top: -15px;
          margin-bottom: -10px;", 
          h3(strong("Instituições Executoras"))
        ),
        column(
          # width = 3,
          width = 2,
          tags$a(
            href = "http://www.univali.br", target = "_blank",
            tags$img(
              # Saída do Logo da UNIVALI 
              imageOutput("Logo_UNIVALI", height = "100%", width = "100%")
            )
          )
        ),
        column(
          width = 2,
          tags$img(
            # Saída do Logo do LEMA
            imageOutput("Logo_LEMA", height = "100%", width = "100%")
          )
        ),
        column(
          width = 1,
          offset = 1,
          tags$a(
            href = "http://www.furg.br", target = "_blank",
            tags$img(
              # Saída do Logo da FURG
              imageOutput("Logo_FURG",height = "100%",width = "100%")
            )
          )
        )
      )
    ),
    right = list(
      fluidRow(
        tags$div(
          style = "margin-left: 50px; margin-top: -15px; margin-bottom: -10px;",
          h3(strong("Apoio"))
        ),
        column(
          offset = 1,
          width = 3,
          tags$div(
            style = "margin-right: 20px;",
            tags$a(
              href = "https://www.gov.br/mpa/pt-br", target = "_blank",
              # Saída do Logo do MAPA
              imageOutput("Logo_MAPA",height = "100%",width = "100%")
            )
          )
        )
      )
    )
  ),
  
  # ControlBar --------------------------------------------------------------
  
  # Definindo o Controlbar do Painel
  controlbar = dashboardControlbar(
    overlay = FALSE, # Se vai sobrepor o conteúdo
    collapsed = TRUE,
    skin = "dark",
    id = "controlbar",
    width = 300,
    # Definindo controlbar Menu
    controlbarMenu(
      id = "controlbarMenu",
      # Definindo o Item Opções
      controlbarItem(
        "Opções",
        icon = icon("gear"),
        # Entrada do controle deslizante
        sliderInput(
          inputId = "intervalo_anos",    # Identificador do controle deslizante
          label = "Intervalo de Anos:",  # Rótulo do controle deslizante
          min = min(dados_ajustados$ANO),# Valor Mínimo do controle deslizante
          max = max(dados_ajustados$ANO),# Valor Máximo do controle deslizante
          value = c(                     # Valor Inicial do controle deslizante
            min(dados_ajustados$ANO),
            max(dados_ajustados$ANO)
          ), 
          step = 1,        # Intervalo entre os valores do controle deslizante
          # Opções das animações
          animate = animationOptions(
            interval = 1700,
            playButton = icon("play"),
            pauseButton = icon("pause")
          )
        ),
        checkboxGroupInput(
          inputId = "species",
          label = "Seletor de Espécies:",
          choiceValues = c("Albacora_bandolim", "Albacora_branca",
                           "Albacora_lage", "Cacao_anequim", "Meca",
                           "Outros", "Prego"),
          choiceNames = c("Albacora bandolim", "Albacora branca", 
                          "Albacora lage", "Cação Anequim", "Meca",
                          "Outros", "Prego"),
          selected = dados_ajustados$CATEGORIA,
        ),
        actionButton(
          inputId = "selectAll",
          label = "Todos",
          icon = icon("square-check")
        ),
        actionButton(
          inputId = "deselectAll",
          label = "Nenhum",
          icon = icon("square")
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
  
  # Definindo nome dos meses
  nomes_meses <- c(
    "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho","Julho",
    "Agosto", "Setembro", "Outubro", "Novembro","Dezembro"
  )
  
  # Cores amigáveis para pessoas com daltonismo
  cores <- c(
    "Albacora_bandolim" = "#9467bd","Albacora_branca" = "#E6194B",
    "Albacora_lage" = "#3CB44B", "Meca" = "#911EB4","Outros" = "#F58231", 
    "Cacao_azul" = "#4363D8","Cacao_anequim" = "#FFE119", "Prego" = "#42D4F4"
  )

  # Definindo as Cores de cada Mês (cores amigáveis para pessoas com daltonismo)
  cores_mes <- c(
    "Janeiro"="#42D4F4","Fevereiro"="#FABED4","Março"="#BFEF45",
    "Abril"="#FFE119","Maio"="#F032E6","Junho"="#9A6324",
    "Julho"="#4363D8","Agosto"="#911EB4","Setembro"="#3CB44B",
    "Outubro"="#A9A9A9","Novembro"="#F58231","Dezembro"="#E6194B"
  )
  
  
  
  # Criando um Valores Reativos que vão Iniciar Nulo
  conteudo_senha_adm <- reactiveVal(NULL)
  conteudo_entrar_adm <- reactiveVal(NULL)
  conteudo_tabela_adm <- reactiveVal(NULL)
  
  # Filtro de Dados ---------------------------------------------------------
  
  # Filtrando os Dados Gerais Completos Reativamente
  dados_gerais_filtrados <- reactive({
    # Filtrando as Espécies
    # dados_aux <- subset(dados_gerais, CATEGORIA %in% input$species)
    dados_aux <- subset(
      dados_gerais, CATEGORIA %in% union(input$species, "Cacao_azul"))
    # Filtrando o Intervalo de Anos
    dados_aux <- subset(
      dados_aux,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2])
    data.frame(dados_aux)
  })
  
  dados_captura_filtrada <- reactive({
    dados_aux <- subset(
      dados_gerais,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2])
    data.frame(dados_aux)
  })
  
  # Filtrando os Dados da Tabela Inicial
  dados_aux_filtrados <- reactive({
    # Filtrando as Espécies 
    # dados_aux <- subset(dados_ajustados, CATEGORIA %in% input$species)
    dados_aux <- subset(
      dados_ajustados, CATEGORIA %in% union(input$species, "Cacao_azul"))
    # Filtrando o Intervalo de Anos
    dados_aux <- subset(
      dados_aux,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2])
    data.frame(dados_aux)
  })
  
  # Fazendo o cálculo da Média de Captura por Kg, por Viagem, por Mês/Ano
  dados_graficoAreaDesembarque <- reactive({
    dados_captura_filtrada() %>%
      mutate(KG_por_Viagem = (KG/DESCARGA)) %>% 
      # Agrupa os Dados por Colunas Selecionadas
      group_by(CATEGORIA, ANO, MES) %>% 
      # Média das Toneladas de Captura de Cada Grupo
      summarise(Media_KG_por_Viagem = mean(KG_por_Viagem)) %>%
      # Substituindo NAs por Zero
      mutate(Media_KG_por_Viagem = replace_na(Media_KG_por_Viagem, 0)) %>% 
      # Arredondando a Média de Toneladas para Duas Casas Decimais
      mutate(Media_KG_por_Viagem = round(Media_KG_por_Viagem, 2)) %>%
      # Criação de Nome do Mês para Legenda
      # mutate(mes_nome = nomes_meses[MES]) %>%
      # # Formata a coluna mes_ano para o modelo aceitado pelo pacote Plotly
      # # mutate(mes_ano = format(as.Date(paste0(ANO, "-", MES, "-01")), "%Y-%m"))
      # mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES))))
      mutate(mes_ano_formatado = make_date(ANO, MES)) %>%
      mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES)))) %>%
      mutate(mes_ano_formatado = format(mes_ano_formatado, "%Y-%m")) 
  })
  
  # Filtrando Dados para o Gráfico de Captura
  dados_graficoCaptura <- reactive({
    dados_gerais_filtrados() %>%
      mutate(KG_por_Viagem = (KG/DESCARGA)) %>%
      group_by(CATEGORIA, ANO, MES) %>%
      summarise(Media_KG_por_Viagem = mean(KG_por_Viagem)) %>%
      # Substituindo NAs por Zero
      mutate(Media_KG_por_Viagem = replace_na(Media_KG_por_Viagem, 0)) %>%
      group_by(CATEGORIA, MES) %>%
      # Média das Toneladas de Captura, de cada Mês, com Anos Agrupados
      summarise(MediaKG_Mes_Viagem = mean(Media_KG_por_Viagem)) %>%
      mutate(MediaKGMesViagem = round(MediaKG_Mes_Viagem, 2)) %>%
      plotly::select(-MediaKG_Mes_Viagem) %>%
      mutate(mes_nome = nomes_meses[MES])
  })
  
  # Filtrando os Dados da Tabela Inicial com somente a CATEGORIA Cacao-azul
  dadostub_aux_filtrados <- reactive({
    dados_auxiliar <- subset(dados_ajustados, CATEGORIA == "Cacao_azul")
    subset(
      dados_auxiliar,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
    )
  })
  
  # Filtrando dados Para o Mapa
  db_filtrado <- reactive({
    dados_aux <- subset(dados_ajustados,
                        CATEGORIA %in% union(input$species, "Cacao_azul"))
    dados_aux <- subset(
      dados_aux,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2])
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

  dados_PesoMes <- reactive({ 
    dados_aux_filtrados() %>%
      mutate(KG_por_Viagem = (KG/DESCARGA)) %>%
      complete(CATEGORIA, ANO, MES, fill = list(KG_por_Viagem = 0)) %>% 
      group_by(CATEGORIA, ANO, MES) %>%
      summarise(MedKGPorViagemMesAno = mean(KG_por_Viagem)) %>%
      mutate(MedKGPorViagemMesAno = replace_na(MedKGPorViagemMesAno, 0)) %>% 
      group_by(CATEGORIA, MES) %>%
      summarise(Media_KG_por_Viagem = mean(MedKGPorViagemMesAno)) %>%
      mutate(Media_KG = round(Media_KG_por_Viagem, 2)) %>%
      mutate(mes_nome = nomes_meses[MES]) %>% 
      mutate(
        CATEGORIA = case_when(
          CATEGORIA == "Albacora_bandolim" ~ "Albacora bandolim",
          CATEGORIA == "Albacora_branca" ~ "Albacora branca",
          CATEGORIA == "Albacora_lage" ~ "Albacora lage",
          CATEGORIA == "Cacao_anequim" ~ "Cação anequim",
          CATEGORIA == "Cacao_azul" ~ "Tubarão azul",
          TRUE ~ CATEGORIA
        )
      )
  })
  
  # Fazendo o cálculo de Dados Totais Registrados por Mes de Cacao-azul
  dados_ComparaDadosTub <- reactive({
    dadostub_aux_filtrados() %>%
      group_by(CATEGORIA, MES, ANO) %>%
      summarise(Quantidade = n()) %>%
      ungroup() %>%
      complete(CATEGORIA, MES = 1:12, fill = list(Quantidade = 0)) %>% 
      mutate(mes_nome = nomes_meses[MES])
  }) 
  
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
  
  # Header ------------------------------------------------------------------
  
  output$notification_menu <- renderMenu({
    notification_items <- lapply(1:nrow(notificacoes), function(i) {
      current_date <- Sys.Date()
      current_time <- format(Sys.time(), "%H:%M")

      notification_status <- "primary"

      if (notificacoes$Data[i] == current_date) {
        if (notificacoes$Horário[i] <= current_time) {
          notification_status <- "danger"
        } else {
          notification_status <- "warning"
        }
      }

      if (notificacoes$Data[i] >= current_date) {
        notification_time <- format(
          as.POSIXct(
            paste(
              notificacoes$Data[i], 
              notificacoes$Horário[i]
              )
            ),
          "%d/%m/%Y %H:%M:%S"
          # "%Y-%m-%d %H:%M:%S"
          )
        notificationItem(
          icon = icon("bell"),
          status = notification_status,
          href = notificacoes$Link[i],
          tags$div(
            tags$span(
              paste(
                notificacoes$Titulo[i]
              ),
              style = "font-weight: bold;"
            ),
            br(),
            tags$span(
              paste(
                "Local:",
                notificacoes$Local[i]
              ),
              style = "font-weight: bold;"
            ),
            br(),
            tags$span(
              paste(
                "Hora exata:",
                notification_time
              ),
              style = "font-weight: bold;"
            ),
            br()
            # tags$span(
            #   paste(
            #     "Data:",
            #     notificacoes$Data[i]
            #   ),
            #   style = "font-weight: bold;"
            # ),
            # br(),
            # tags$span(
            #   paste(
            #     "Hora:",
            #     notificacoes$Horário[i]
            #   ),
            #   style = "font-weight: bold;"
            # )
          )
        )
      }
    })

    # Remover itens NULL da lista
    notification_items <- notification_items[!sapply(notification_items, 
                                                     is.null)]

    dropdownMenu(
      type = "notifications",
      headerText = paste(
        "Você tem ", length(notification_items), "notificações"
      ),
      icon = icon("bell"),
      .list = notification_items
    )
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
        tags$span("Projeto Tubarão Azul")
      })
    }
  })
  
  
  
  # Projeto -----------------------------------------------------------------
  
  output$LogoPTA <- renderImage({
    # Sys.sleep(1)
    list(
      src = "dados_brutos/logo_tuba_azul_2.png", # Local do arquivo da Imagem
      height = "100%",                     # Altura da Imagem
      width = "100%",                      # Largura da Imagem
      contentType = "image/png",            # Tipo do Conteúdo da Imagem
      id = "LogoPTA"
    )
  }, deleteFile = FALSE)
  
  # Renderizando a Imagem do Fluxograma
  output$FluxogramaTubAzul <- renderImage({
    list(
      src = "dados_brutos/Fluxograma_ajustado.png", # Local do arquivo da Imagem
      height = "100%",                     # Altura da Imagem
      # width = "100%",                      # Largura da Imagem
      contentType = "image/png"            # Tipo do Conteúdo da Imagem
    )
  }, deleteFile = FALSE)                   # Não Deleta o Arquivo após o Uso
  
  output$Logo_FURG <- renderImage({
    list(
      src = "dados_brutos/FURG_fundo.png",
      height = "80px",
      width = "55px",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  output$Logo_UNIVALI <- renderImage({
    list(
      src = "dados_brutos/Logo_univali2.png",
      height = "80px",
      width = "140px",
      contentType = "image/jpg"
    )
  }, deleteFile = FALSE)
  
  output$Logo_LEMA <- renderImage({
    list(
      src = "dados_brutos/Logo_Lema3.png",
      height = "80px",
      width = "175px",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  output$Logo_MAPA <- renderImage({
    list(
      src = "dados_brutos/logo_MAPA2.png",
      height = "80px",
      width = "315px",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  # Distribuição de Captura --------------------------------------------
  
  output$TubMesAno <- renderPlotly({
    plot_ly(
      data = dados_TubMesAno(),
      x = ~mes_ano_formatado,
      y = ~percentage,
      color = ~CATEGORIA,
      # colors = cores,
      # type = "bar",
      type = 'scatter',
      stackgroup = 'one',
      groupnorm = 'percent',
      mode = 'none',
      hoverinfo = "text",
      text = ~paste(
        " Data: ", mes_ano, "<br>",
        "Categoria: ",case_when(
          CATEGORIA == "Albacora_bandolim" ~ "Albacora bandolim",
          CATEGORIA == "Albacora_branca" ~ "Albacora branca",
          CATEGORIA == "Albacora_lage" ~ "Albacora lage",
          CATEGORIA == "Cacao_anequim" ~ "Cação anequim",
          CATEGORIA == "Cacao_azul" ~ "Tubarão azul",
          CATEGORIA == "Meca" ~ CATEGORIA,
          CATEGORIA == "Outros" ~ CATEGORIA,
          CATEGORIA == "Prego" ~ CATEGORIA,
          TRUE ~ CATEGORIA
        ),
        "<br>",
        "Quantidade: ", Quantidade, "<br>",
        "Porcentagem: ", round(percentage, 2), "%"
      )
    ) %>%
      layout(
        # title = "Dados Registrados por Mês, Ano e Categoria",
        xaxis = list(
          title = "",
          tickvals = dados_TubMesAno()$mes_ano_formatado[seq(
            1, length(dados_TubMesAno()$mes_ano_formatado), by = 2)],
          ticktext = dados_TubMesAno()$mes_ano_formatado[seq(
            1, length(dados_TubMesAno()$mes_ano_formatado), by = 2)], 
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
        xaxis = list(
          categoryorder = "category ascending"  
        ),
        margin = list(t = 10, b = 40, l = 20, r = 20)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$BarraTubOutros <- renderPlotly({
    plot_ly(
      data = dados_BarraTubOutros(),
      x = ~MES,
      y = ~prop,
      color = ~CATEGORIA,
      colors = cores,
      type = 'bar',
      text = ~paste(
        " Categoria: ",case_when(
          CATEGORIA == "Cacao_azul" ~ "Tubarão azul",
          CATEGORIA == "Outros" ~ CATEGORIA,
          TRUE ~ CATEGORIA
        ),
        "<br>",
        "Quantidade Total: ", n, "<br>",
        "Quantidade Média: ", media, "<br>",
        "Porcentagem: ", round(prop,2), "%"
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        # title = "Comparação de Dados da Tubarão Azul para Outros",
        # title = "Comparação de Dados Registrados",
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
          title = "Mês",
          categoryorder = "category descending",
          tickvals = unique(dados_BarraTubOutros()$MES), 
          ticktext = unique(dados_BarraTubOutros()$MES)
        ),
        margin = list(t = 10, b = 40, l = 20, r = 20)
      )
  })
  
  output$ComparaDadosTub <- renderPlotly({
    plot_ly(
      dados_ComparaDadosTub(),
      x = ~MES,
      y = ~ANO,
      z = ~Quantidade,
      type = "heatmap",
      colorscale = "Viridis",
      hoverinfo = "text",
      text = ~paste(
        " Mês: ", mes_nome, "<br>",
        "Ano: ", ANO, "<br>",
        "Dados Registrados: ", Quantidade, "<br>"
      )
    ) %>%
      layout(
        # title = "Comparação de Dados Registrados",
        title = NULL,
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_ComparaDadosTub()$MES),
          ticktext = unique(dados_ComparaDadosTub()$MES)
          ),
        yaxis = list(
          title = "Ano",
          tickformat = ".0f",
          tickvals = unique(floor(dados_ComparaDadosTub()$ANO)),
          ticktext = unique(floor(dados_ComparaDadosTub()$ANO))),
        legend = list(
          orientation = "h",
          y = 0.9,
          x = 0.1,
          font = list(
            size = 10
          )
        ),
        showlegend = F,  # Desativa a legenda
        # margin = list(t = 10, b = 40)
        margin = list(t = 10, b = 40, l = 20, r = 20)
      )
  })
  
  # Desembarques ------------------------------------------------------------
  
  # Renderização do Gráfico Plotly da Média Mensal de Capturas (mes)
  output$graficoCaptura <- renderPlotly({
    plot_ly(
      data = dados_graficoCaptura(),
      x = ~MES,
      y = ~MediaKGMesViagem,
      type = 'scatter',
      mode = 'lines+markers',
      color = ~CATEGORIA,
      colors = cores,
      marker = list(size = 10), # Tamanho do Marcador
      hoverinfo = "text",
      text = ~paste(
        " Espécie: ",
        case_when(
          CATEGORIA == "Albacora_bandolim" ~ "Albacora bandolim",
          CATEGORIA == "Albacora_branca" ~ "Albacora branca",
          CATEGORIA == "Albacora_lage" ~ "Albacora lage",
          CATEGORIA == "Cacao_anequim" ~ "Cação anequim",
          CATEGORIA == "Cacao_azul" ~ "Tubarão azul",
          CATEGORIA == "Meca" ~ CATEGORIA,
          CATEGORIA == "Outros" ~ CATEGORIA,
          CATEGORIA == "Prego" ~ CATEGORIA,
          TRUE ~ CATEGORIA
        ),
        "<br>",
        "Média de KG: ", MediaKGMesViagem, "<br>"
      ),
      hoverlabel = list(
        font = list(
          size = 11 # Tamanho da fonte do hover
        )
      )
    ) %>%
      layout(
        # title = "Média Mensal de Captura por Viagem",
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_graficoCaptura()$MES),
          ticktext = unique(dados_graficoCaptura()$MES)
        ),
        yaxis = list(
          title = "Captura Média (KG) por Viagem"
        ),
        showlegend = FALSE,
        hovermode = "x",
        margin = list(t = 10, b = 40, l = 20, r = 20)
      )
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
          input$species
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
      name = "Tubarão Azul",
      fillcolor = "#4363D8",
      text = ~paste(
        " Espécie: ", 'Tubarão azul', "<br>",
        "Data: ", mes_ano, "<br>",
        "Média de KG: ", Cacao_azul, "<br>"
      )
    )
    if (verifica_coluna(data_wide_filtrado, "Albacora_bandolim")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_bandolim,
          name = "Albacora bandolim",
          fillcolor = "#F032E6",
          text = ~paste(
            " Espécie: ", 'Albacora bandolim', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de KG: ", Albacora_bandolim, "<br>"
          )
        )
    }
    if (verifica_coluna(data_wide_filtrado, "Albacora_branca")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_branca,
          name = 'Albacora branca',
          fillcolor = '#E6194B',
          text = ~paste(
            " Espécie: ", 'Albacora branca', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de KG: ", Albacora_branca, "<br>"
          )
        )
    }
    if(verifica_coluna(data_wide_filtrado, "Albacora_lage")){
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_lage,
          name = 'Albacora lage',
          fillcolor = '#3CB44B',
          text = ~paste(
            " Espécie: ", 'Albacora lage', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de KG: ", Albacora_lage, "<br>"
          )
        )
    }
    if(verifica_coluna(data_wide_filtrado, "Cacao_anequim")){
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Cacao_anequim,
          name = 'Cação anequim',
          fillcolor = '#FFE119',
          text = ~paste(
            " Espécie: ", 'Cação anequim', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de KG: ", Cacao_anequim, "<br>"
          )
        )
    }
    if(verifica_coluna(data_wide_filtrado, "Meca")){
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Meca,
          name = 'Meca',
          fillcolor = '#911EB4',
          text = ~paste(
            " Espécie: ", 'Meca', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de KG: ", Meca, "<br>"
          )
        )
    }
    if(verifica_coluna(data_wide_filtrado, "Outros")){
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Outros,
          name = 'Outros',
          fillcolor = '#F58231',
          text = ~paste(
            " Espécie: ", 'Outros', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de KG: ", Outros, "<br>"
          )
        )
    }
    if(verifica_coluna(data_wide_filtrado, "Prego")){
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Prego,
          name = 'Prego',
          fillcolor = '#42D4F4',
          text = ~paste(
            " Espécie: ", 'Prego', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de KG: ", Prego, "<br>"
          )
        )
    }
    plot_data <- plot_data %>% 
      layout(
        # title = 'Média Mensal de Captura por Viagem ao Longo do Período',
        xaxis = list(
          title = "",
          tickvals = data_wide_filtrado$mes_ano_formatado[seq(
            1, length(data_wide_filtrado$mes_ano_formatado), by = 2)],
          ticktext = data_wide_filtrado$mes_ano_formatado[seq(
            1, length(data_wide_filtrado$mes_ano_formatado), by = 2)], 
          showgrid = FALSE
        ),
        yaxis = list(
          # title = "Porcentagem da Captura Média por Viagem",
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
  
  output$pesoMes <- renderPlotly({
    plot_ly(
      data = dados_PesoMes(),
      x = ~MES,
      y = ~CATEGORIA,
      z = ~Media_KG,
      type = "heatmap",
      colorscale = "Viridis",
      hoverinfo = "text",
      text = ~paste(
        " Mês: ", mes_nome, "<br>",
        "Categoria: ", CATEGORIA, "<br>",
        "Média de KG por Viagem: ", Media_KG, "<br>"
      )
    ) %>%
      layout(
        # title = "Média Mensal de Captura por Viagem",
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_PesoMes()$MES), 
          ticktext = unique(dados_PesoMes()$MES)
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
  
  # Renderização do Mapa de Calor das Capturas de Todas as Categorias
  output$MapaCaptura <- renderLeaflet({
    tab01 <- db_filtrado()$tab01
    
    # Cálculo dos quantis para categorizar os dados do mapa de calor
    breaks <- quantile(tab01$prod, probs = seq(0, 1, 0.1), na.rm = TRUE)
    
    # Verificando se há breaks duplicados 
    if (any(duplicated(breaks))) {
      # Jitter é usado para variar um pouco o valor dos duplicados
      tab01$prod <- jitter(tab01$prod, factor = 0.1)
    }
    
    # Criar a paleta de cores com base nos intervalos
    pal <- colorQuantile(
      palette = "viridis",
      domain = tab01$prod,
      probs = seq(0, 1, 0.1)
    )
    
    leaflet() %>%
      # Definindo a primeira opção do estilo do Mapa (Claro)
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Light Map"
      ) %>%
      # Definindo a segunda opção do estilo do Mapa (Escuro)
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = "Dark Map"
      ) %>%
      # Definindo a Posição Inicial da visão sobre o Mapa
      setView(
        lng = -40, lat = -28, zoom = 4
      ) %>%
      # Definindo a adição dos Marcadores no Mapa
      addCircleMarkers(
        group = tab01$prod,      # Define os marcadores com base na soma dos KG
        # radius = 12,             # Define o raio dos marcadores como 12 pixels
        radius = 7,
        lng = tab01$LON,         # Define tab01$LON como longitude
        lat = tab01$LAT,         # Define tab01$LAT como latitude
        stroke = FALSE,          # Define que não haverá borda dos marcadores
        color = pal(tab01$prod), # Define a paleta de cores dos marcadores
        fillOpacity = 0.7,       # Define a opacidade dos marcadores como 70%
        label = lapply(paste0(
          "Captura: ", round(tab01$prod, 0), " kg <br> Viagens: ", tab01$viagem
        ), HTML)
      ) %>%
      # Definindo a legenda com a paleta de cores e suas Porcentagens
      addLegend(
        pal = pal, values = tab01$prod, group = tab01$prod,
        position = "bottomright", title = "Percentual da Captura"
      ) %>%
      # Controle de Estilo de Mapa
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
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
  
  output$MapaCapturaPorViagem <- renderLeaflet({
    tab01 <- db_filtrado()$tab01

    # Cálculo dos quantis para categorizar os dados do mapa de calor
    breaks <- quantile(tab01$prod2, probs = seq(0, 1, 0.1), na.rm = TRUE)

    # Verificando se há breaks duplicados
    if (any(duplicated(breaks))) {
      # Jitter é usado para variar um pouco o valor dos duplicados
      tab01$prod2 <- jitter(tab01$prod2, factor = 0.1)
    }

    # Criar a paleta de cores com base nos intervalos
    pal <- colorQuantile(
      palette = "viridis",
      domain = tab01$prod2,
      probs = seq(0, 1, 0.1)
    )

    leaflet() %>%
      # Definindo a primeira opção do estilo do Mapa (Claro)
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Light Map"
      ) %>%
      # Definindo a segunda opção do estilo do Mapa (Escuro)
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = "Dark Map"
      ) %>%
      # Definindo a Posição Inicial da visão sobre o Mapa
      setView(
        lng = -40, lat = -28, zoom = 4
      ) %>%
      # Definindo a adição dos Marcadores no Mapa
      addCircleMarkers(
        group = "Marcadores Circulares",
        # group = tab01$prod2,      # Define os marcadores com base na soma dos KG
        # radius = 12,             # Define o raio dos marcadores como 12 pixels
        radius = 7,
        lng = tab01$LON,         # Define tab01$LON como longitude
        lat = tab01$LAT,         # Define tab01$LAT como latitude
        stroke = FALSE,          # Define que não haverá borda dos marcadores
        color = pal(tab01$prod2), # Define a paleta de cores dos marcadores
        fillOpacity = 0.7,       # Define a opacidade dos marcadores como 70%
        label = paste0(
          "Captura: ", round(tab01$prod2, 0), " kg"
        )
      ) %>%
      # Definindo a legenda com a paleta de cores e suas Porcentagens
      addLegend(
        pal = pal, values = tab01$prod2, group = tab01$prod,
        position = "bottomright", title = "Percentual da Captura"
      ) %>%
      # Controle de Estilo de Mapa
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      # addLayersControl(
      #   position = "bottomleft",
      #   baseGroups = c("Marcadores Circulares", "Mapa de Calor"),
      #   options = layersControlOptions(collapsed = FALSE)
      # ) %>%
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
      # addHeatmap(
      #   group = "Mapa de Calor",
      #   lng = tab01$LON,
      #   lat = tab01$LAT,
      #   intensity = tab01$prod2,
      #   blur = 20,
      #   max = 0.05,
      #   radius = 12
      # ) %>% 
      setMaxBounds(
        lng1 = -180, lat1 = -90,  # Limite inferior esquerdo
        lng2 = 180, lat2 = 90     # Limite superior direito
      )
  })
  
  output$MapaCapturaPorViagem2 <- renderLeaflet({
    tab01 <- db_filtrado()$tab01
    
    # Criar a paleta de cores com base nos intervalos
    pal <- colorQuantile(
      palette = "viridis",
      domain = tab01$prod2,
      probs = seq(0, 1, 0.1)
    )
    
    tab01 <- tab01 %>% 
      mutate(
        radii = (tab01$prod2 - min(tab01$prod2)) / 
          (max(tab01$prod2) - min(tab01$prod2)) * 30 + 6 
        )
    
    leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Light Map"
      ) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = "Dark Map"
      ) %>%
      setView(
        lng = -40, lat = -28, zoom = 4
      ) %>%
      addCircleMarkers(
        lng = tab01$LON,
        lat = tab01$LAT,
        radius = tab01$radii,
        stroke = FALSE,
        color = pal(tab01$prod2),
        fillOpacity = 0.7,
        label = paste0(
          "Captura: ", round(tab01$prod2, 0), " kg"
        )
      ) %>%
      addLegend(
        pal = pal, values = tab01$prod2,
        position = "bottomright", title = "Percentual da Captura"
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addMiniMap(
        position = "bottomleft"
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
        lng1 = -180, lat1 = -90,  # Limite inferior esquerdo
        lng2 = 180, lat2 = 90     # Limite superior direito
      )
  })
  
  output$MapaCapturaPorViagem3 <- renderLeaflet({
    tab01 <- db_filtrado()$tab01

    # Definindo uma cor fixa para todos os círculos
    fixedColor <- "#FF5733" # Escolha uma cor fixa, por exemplo, vermelho

# Normalizando os valores de prod2 para o intervalo [0.1, 1] para usar na opacidade
    minProd2 <- min(tab01$prod2, na.rm = TRUE)
    maxProd2 <- max(tab01$prod2, na.rm = TRUE)
    tab01 <- tab01 %>%
      # Intervalo [0.1, 1]
      mutate(opct = (prod2 - minProd2) / (maxProd2 - minProd2) * 0.9 + 0.1)
    leaflet() %>%
      # Definindo a primeira opção do estilo do Mapa (Claro)
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Light Map"
      ) %>%
      # Definindo a segunda opção do estilo do Mapa (Escuro)
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = "Dark Map"
      ) %>%
      # Definindo a Posição Inicial da visão sobre o Mapa
      setView(
        lng = -40, lat = -28, zoom = 4
      ) %>%
      # Definindo a adição dos Marcadores no Mapa
      addCircleMarkers(
        group = "Capturas",      # Define o grupo dos marcadores
        radius = 7,             # Define o raio dos marcadores como 7 pixels
        lng = tab01$LON,         # Define tab01$LON como longitude
        lat = tab01$LAT,         # Define tab01$LAT como latitude
        stroke = FALSE,          # Define que não haverá borda dos marcadores
        color = fixedColor,      # Define uma cor fixa para todos os marcadores
        fillOpacity = tab01$opct, # Define a opacidade dos marcadores com base em prod2
        label = paste0(
          "Captura: ", round(tab01$prod2, 0), " kg"
        )
      ) %>%
      # Controle de Estilo de Mapa
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
        options = layersControlOptions(collapsed = FALSE)
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
  
  output$MapaViagens <- renderLeaflet({
    tab01 <- db_filtrado()$tab01
    
    # Cálculo dos quantis para categorizar os dados do mapa de calor
    breaks <- quantile(tab01$viagem, probs = seq(0, 1, 0.1), na.rm = TRUE)
    
    # Verificando se há breaks duplicados 
    if (any(duplicated(breaks))) {
      # Jitter é usado para variar um pouco o valor dos duplicados
      tab01$viagem <- jitter(tab01$viagem, factor = 0.1)
    }
    
    # Criar a paleta de cores com base nos intervalos
    pal <- colorQuantile(
      palette = "viridis",
      domain = tab01$viagem,
      probs = seq(0, 1, 0.1)
    )
    
    leaflet() %>%
      # Definindo a primeira opção do estilo do Mapa (Claro)
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Light Map"
      ) %>%
      # Definindo a segunda opção do estilo do Mapa (Escuro)
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = "Dark Map"
      ) %>%
      # Definindo a Posição Inicial da visão sobre o Mapa
      setView(
        lng = -40, lat = -28, zoom = 4
      ) %>%
      # Definindo a adição dos Marcadores no Mapa
      addCircleMarkers(
        group = tab01$viagem,      # Define os marcadores com base na soma dos KG
        # radius = 12,             # Define o raio dos marcadores como 12 pixels
        radius = 7,
        lng = tab01$LON,         # Define tab01$LON como longitude
        lat = tab01$LAT,         # Define tab01$LAT como latitude
        stroke = FALSE,          # Define que não haverá borda dos marcadores
        color = pal(tab01$viagem), # Define a paleta de cores dos marcadores
        fillOpacity = 0.7,       # Define a opacidade dos marcadores como 70%
        label = paste0("Viagens: ", round(tab01$viagem),0)
      ) %>%
      # Definindo a legenda com a paleta de cores e suas Porcentagens
      addLegend(
        pal = pal, values = tab01$viagem, group = tab01$viagem,
        position = "bottomright", title = "Percentual da Viagens"
      ) %>%
      # Controle de Estilo de Mapa
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
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
  
  output$MapaComprimento <- renderLeaflet({
    leaflet() %>%
      # Definindo a primeira opção do estilo do Mapa (Claro)
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Light Map"
      ) %>%
      # Definindo a segunda opção do estilo do Mapa (Escuro)
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = "Dark Map"
      ) %>%
      # Definindo a Posição Inicial da visão sobre o Mapa
      setView(
        lng = -40, lat = -28, zoom = 5
      )%>%
      # Controle de Estilo de Mapa
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
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
    senha_correta <- check_password(input$senha, "senha_hash.txt")
    if (senha_correta) {
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
          if (senha_correta) {
            dados_aux_filtrados()
          }
        }
        # Opções da Data Table
      },options = list(
        paging = TRUE,
        searching = FALSE,
        rownames = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
          )
        ),
      class = "cell-border stripe hover",
      selection = "single"
      )
    } else{
      # Mensagem no Caso de Senha Incorreta
      showModal(
        modalDialog(
          title = "Erro de login",
          "Senha incorreta. Tente novamente.",
          easyClose = TRUE
        )
      )
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

# Distribuição de Comprimentos --------------------------------------------
  
  output$histograma_comprimento <- renderPlotly({
    histogramaM <- plot_ly(
      data = dados_falsos %>% filter(Sexo == "M"),
      x = ~IDL,
      name = "Masculino",
      type = 'histogram'
      ) %>% 
      layout(
        xaxis = list(
          title = ""
        )
      )
    histogramaF <- plot_ly(
      data = dados_falsos %>% filter(Sexo == "F"),
      x = ~IDL,
      name = "Feminino",
      type = 'histogram'
    ) %>% layout(
      xaxis = list(
        title = ""
      )
    )
    subplot(
      histogramaM, 
      histogramaF,
      nrows = 1,
      shareX = TRUE,
      shareY = TRUE
      ) %>% 
      layout(
        title = "Histograma de Comprimento por Sexo",
        yaxis = list(
          title = "Frequência Relativa",
          showgrid = FALSE,
          ticksuffix = '%'
        ),
        showlegend = FALSE
        )
  })
  
  output$histograma_comprimentoM <- renderPlotly({
    plot_ly(
      data = dados_falsos %>% filter(Sexo == "M"),
      x = ~IDL,
      name = "Masculino",
      type = 'histogram',
    marker = list(
      line = list(
        color = 'black',  # cor da borda
        width = 1        # espessura da borda
      )
    )
    ) %>% 
      layout(
        title = "Macho",
        hovermode = "x",
        xaxis = list(
          title = "Comprimento (cm)",
          showgrid = TRUE
        ),
        yaxis = list(
          showgrid = TRUE,
          ticksuffix = '%'
        )
      )
  })
  
  output$histograma_comprimentoF <- renderPlotly({
    plot_ly(
      data = dados_falsos %>% filter(Sexo == "F"),
      x = ~IDL,
      name = "Masculino",
      type = 'histogram',
      marker = list(
        line = list(
          color = 'black',  # cor da borda
          width = 1        # espessura da borda
        )
      )
    ) %>% 
      layout(
        title = "Fêmea",
        hovermode = "x",
        xaxis = list(
          title = "Comprimento (cm)",
          showgrid = TRUE
        ),
        yaxis = list(
          showgrid = TRUE,
          ticksuffix = '%'
        )
      )
  })
  
  output$boxplot_comprimento <- renderPlotly({
    plot_ly(
      data = dados_falsos,
      y = ~IDL,
      x = ~Sexo,
      type = "box",
      marker = list(color = "primary")
    ) %>% 
      layout(
        title = "Distribuição de Comprimento de Tubarões azul",
        hovermode = "x",
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = "Comprimento (cm)"
        )
      )
  })
  
# Tabela de Embarcações ---------------------------------------------------
  
  output$tabela_embarcacoes <- renderDT({
    datatable(
      notificacoes[, !names(notificacoes) %in% c("TextoOpcional", "Link")],
      rownames = FALSE,
      filter = "none",
      options = list(
        paging = FALSE,
        searching = FALSE,
        pageLength = 10,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")  # Centraliza o texto
        )
      ),
      class = "cell-border stripe hover",
      selection = "single"
    )
  })
  
  observeEvent(input$tabela_embarcacoes_rows_selected, {
    i <- input$tabela_embarcacoes_rows_selected
    if (length(i) == 1) {
      # Verifica se há link disponível e cria o título do modal
      title_text <- paste("Detalhes da Embarcação", notificacoes$id[i])
      title_text <- tags$a(
        title_text,
        href = notificacoes$Link[i],
        target = "_blank"
        )
      
      # Verifica se há informações extras disponíveis
      info_extra <- if(!is.na(notificacoes$TextoOpcional[i])){
        tags$span(
          paste(
            "Texto:"
          ),
          # style = "font-weight: bold;",
          br(),
          paste(
            notificacoes$TextoOpcional[i]
          )
        )
      }
      
      # Exibe o modal com o título e informações extras
      showModal(modalDialog(
        title = title_text,
        info_extra,
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  

# Perguntas ---------------------------------------------------------------

  output$BarrasKGMes <- renderPlotly({
    dados_auxiliares <- dados_ajustados %>% 
      group_by(MES) %>% 
      summarise(KG_Total = round(sum(KG),2)) %>% 
      ungroup()
    
    plot_ly(
      data = dados_auxiliares,
      type = "bar",
      x = ~MES,
      y = ~KG_Total
      ) %>% 
      layout(
        hovermode = "x",
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_auxiliares$MES), 
          ticktext = unique(dados_auxiliares$MES)
        ),
        yaxis = list(
          title = "Quilos Totais",
          ticksuffix = ' (Kg)'
        )
      )
  })
  
  output$BarrasKGMediaMes <- renderPlotly({
    dados_auxiliares9 <- dados_ajustados %>% 
      group_by(MES) %>% 
      summarise(KG_Media = round(sum(KG)/sum(DESCARGA), 2)) %>% 
      ungroup()
    
    plot_ly(
      data = dados_auxiliares9,
      type = "bar",
      x = ~MES,
      y = ~KG_Media
    ) %>% 
      layout(
        hovermode = "x",
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_auxiliares9$MES), 
          ticktext = unique(dados_auxiliares9$MES)
        ),
        yaxis = list(
          title = "Quilos Totais",
          ticksuffix = ' (Kg)'
        )
      )
  })
  
  output$BarrasKGMesAno <- renderPlotly({
    dados_auxiliares2 <- dados_ajustados %>% 
      group_by(MES, ANO) %>% 
      summarise(KG_Total = sum(KG)) %>% 
      ungroup() %>% 
      mutate(mes_ano = make_date(ANO, MES)) %>% 
      mutate(mes_ano = format(mes_ano, "%Y-%m"))
    
    plot_ly(
      data = dados_auxiliares2,
      x = ~mes_ano,
      y = ~KG_Total,
      type = "bar"
    ) %>% 
      layout(
        hovermode = "x",
        yaxis = list(
          title = "Quilos Totais",
          ticksuffix = ' (Kg)'
        )
      )
  })
  
  output$BarrasKGMesAnoMedia <- renderPlotly({
    dados_auxiliares3 <- dados_ajustados %>% 
      group_by(MES, ANO) %>% 
      summarise(
        KG_Total = sum(KG),
        Descarga_total = sum(DESCARGA),
        KG_Media = round(KG_Total/Descarga_total,2)
      ) %>% 
      ungroup() %>% 
      mutate(mes_ano = make_date(ANO, MES)) %>% 
      mutate(mes_ano = format(mes_ano, "%Y-%m"))
    
    plot_ly(
      data = dados_auxiliares3,
      x = ~mes_ano,
      y = ~KG_Media,
      hoverinfo = "text",
      text = ~paste(
        "Captura média por viagem: ", KG_Media, "Kg <br>",
        "Viagens Totais: ", Descarga_total, "<br>"
        ),
      type = "bar"
    ) %>% 
      layout(
        hovermode = "x",
        yaxis = list(
          title = "Quilos Totais",
          ticksuffix = ' (Kg)'
        )
      )
  })
  
  output$BarrasKGMediaPorEspecie <- renderPlotly({
    dados_auxiliares4 <- dados_ajustados %>% 
      group_by(MES, CATEGORIA) %>% 
      summarise(KG_Total = round(sum(KG),2)) %>% 
      ungroup()
    
    plot_ly(
      data = dados_auxiliares4,
      type = "bar",
      x = ~MES,
      y = ~KG_Total,
      color = ~CATEGORIA,
      colors = cores
    ) %>% 
      layout(
        hovermode = "x",
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_auxiliares4$MES), 
          ticktext = unique(dados_auxiliares4$MES)
        ),
        yaxis = list(
          title = "Quilos Totais",
          ticksuffix = ' (Kg)'
        ),
        showlegend = F
      )
  })
  
  output$BarrasViagemPorMes <- renderPlotly({
    dados_auxiliares5 <- dados_ajustados %>% 
      group_by(MES, ANO) %>% 
      summarise(
        Descarga_total = sum(DESCARGA)
      ) %>% 
      ungroup() %>% 
      mutate(mes_ano = make_date(ANO, MES)) %>% 
      mutate(mes_ano = format(mes_ano, "%Y-%m"))
    
    plot_ly(
      data = dados_auxiliares5,
      x = ~mes_ano,
      y = ~Descarga_total,
      type = "bar"
      # hoverinfo = "text",
      # text = ~paste(
      #   "Captura média por viagem: ", KG_Total, "Kg <br>",
      #   "Viagens Totais: ", Descarga_total, "<br>"
      # )
    ) %>% 
      layout(
        hovermode = "x",
        yaxis = list(
          title = "Número de Viagens Totais"
        )
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
}

# Inicialização da aplicação Shiny
shinyApp(ui, server)
