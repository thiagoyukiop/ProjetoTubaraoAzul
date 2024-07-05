# Bibliotecas -------------------------------------------------------------

# Carregando os Pacotes que serão utilizados no dashboard
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, leaflet, dplyr, scales, plotly,
  zoo, tidyverse, digest, DT, shinyjs, raster, readxl
)

# Carregando os Dados de um Arquivo csv
dados <- read.table(
  "dados_brutos/dados_17_24_Outros.csv", header = TRUE, sep = ",", dec = "."
)
# Ajuste nos nomes das CATEGORIAS
dados_ajustados <- dados %>% 
  mutate(
    CATEGORIA = case_when(
      CATEGORIA == "Albacora-bandolim" ~ "Albacora_bandolim",
      CATEGORIA == "Albacora-branca" ~ "Albacora_branca",
      CATEGORIA == "Albacora-lage" ~ "Albacora_lage",
      CATEGORIA == "Cacao-anequim" ~ "Cacao_anequim",
      CATEGORIA == "Cacao-azul" ~ "Cacao_azul",
      CATEGORIA == "Meca" ~ CATEGORIA,
      CATEGORIA == "Outros" ~ CATEGORIA,
      CATEGORIA == "Prego" ~ CATEGORIA, 
      TRUE ~ CATEGORIA
    )
  )

dados_falsos <- read.table(
  "dados_falsos.csv", header = TRUE, sep = ",", dec = "."
)

notificacoes <- read_excel("dados_brutos/Notificacoes.xlsx")

not_modificadas <- notificacoes %>% 
  mutate(Data = format(as.Date(Data), "%d/%m/%Y")) %>% 
  mutate(Horario = sprintf("%02d:%02d", Hora, Minuto)) %>% 
  plotly::select(-c(Hora, Minuto))

notificacoes <- not_modificadas

verifica_coluna <- function(df, coluna) {
  return(any(names(df) == coluna))
}

check_password <- function(input_password, filename) {
  # Ler o hash da senha do arquivo
  hashed_password <- readLines(filename)
  # Calcular o hash da senha fornecida
  input_hash <- digest(input_password, algo = "sha256", serialize = FALSE)
  # Verificar se os hashes são iguais
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
    # titleWidth = 250,
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
    width = 300,
    # width = 250,   # Definição da Largura em pixels
    minified = TRUE,  # Se a aba lateral ao ser fechada deverá mostrar os ícones
    collapsed = FALSE, # Se a aba lateral deve ser iniciada fechada
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
        # text = "Distribuição de comprimentos",
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
        icon = icon("earth-americas"),
        expandedName = "Distribuição espacial das capturas2"
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
      menuItem(
        text = "Mapa de distribuição de comprimentos",
        tabName = "tab7header",
        icon = icon("map")
      ),
      menuItem(
        text = "Tabela de Embarcações",
        tabName = "tab8header",
        icon = icon("ship")
      )
    )
  ),
  
  # Body --------------------------------------------------------------------
  
  # Definindo o Body do Painel
  body = dashboardBody(
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
              h3("Fluxograma do Plano de Gestão da pesca de Tubarão Azul 
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
              box(
                title = "Texto Leia-me",
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
                  do e-mail ",strong("proj.tubaraoazul.furg@gmail.com.")),
                sidebar = boxSidebar(
                  id = "boxsidebar1",
                  icon = icon("circle-info"),
                  width = 30,
                  background = "#A6ACAFEF",
                  p("Contém informações detalhadas sobre as abas presentes no
                    dashboard e contato para mais informações."),
                )
              )
            )
          )
        )
      ),
      # Definindo o conteúdo da Distribuição de Comprimentos
      tabItem(
        tabName = "tab2header",
        fluidPage(
          fluidRow(
            column(
              width = 12,
              # Definindo Caixa com conteúdo da Distribuição de Comprimentos
              box(
                # title = "Gráfico de Barras Empilhadas",
                title = "Gráfico de Área Relativa",
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
                # title = "Gráfico de Rosca",
                title = "Gráfico de Barras",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                div(
                  class = "graficos",
                  # Saída do Gráfico de Rosca, sobre a Comparação de dados
                  # plotlyOutput("RoscaTubOutros", height = "100%")
                  plotlyOutput("BarraTubOutros", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar3",
                  icon = icon("circle-info"),
                  background = "#A6ACAFEF",
                  # p("Este gráfico de rosca compara a presença de Tubarão azul
                  #   com a categoria 'Outros', que representa a junção dos dados
                  #   de todas as outras espécies de pesca. Ele mostra a proporção 
                  #   de Tubarão azul em relação ao total, permitindo visualizar
                  #   sua participação comparada com as demais categorias.")
                  p("Este gráfico de barra, compara a presença de Tubarão azul
                    com a categoria 'Outros', que representa a média de dados
                    de todas as outras espéciesde pesca. Ele mostra a proporção
                    de dados de Tubarão azul comparada com as demais categorias,
                    por mês")
                )
              )
            ),
            column(
              width = 6,
              box(
                # title = "Gráfico de Rosca",
                title = "Mapa de Calor",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                div(
                  class = "graficos",
                  # Saída do Gráfico de Rosca que compara os dados por mês
                  plotlyOutput("ComparaDadosTub", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar4",
                  icon = icon("circle-info"),
                  background = "#A6ACAFEF",
                  # p("Este gráfico de rosca compara os dados obtidos em cada mês
                  #   para todas as espécies de pesca. Cada fatia representa um 
                  #   mês específico, mostrando a distribuição proporcional dos
                  #   dados ao longo do período analisado, permitindo visualizar
                  #   variações sazonais ou tendências.")
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
                title = "Gráfico de Linhas",
                width = 12,
                solidHeader = TRUE, 
                collapsible = TRUE,
                status = "primary",
                div(
                  class = "graficos",
                  # Saída do Gráfico Plotly de Captura 
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
                # title = "Gráfico de Rosca",
                title = "Mapa de Calor",
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "primary",
                div(
                  class = "graficos",
                  # Saída do Gráfico Plotly das Espécies
                  plotlyOutput("pesoMes", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar7",
                  icon = icon("circle-info"),
                  background = "#A6ACAFEF",
                  # p("Este gráfico de rosca ilustra a composição de espécies
                  #   presentes nos dados de pesca, indicando a porcentagem de 
                  #   cada espécie em relação ao total. Cada segmento do gráfico 
                  #   representa uma espécie, facilitando a visualização das 
                  #   proporções relativas entre elas.")
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
                title = "Gráfico de Área Relativa",
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
                title = "Mapa de Capturas",
                solidHeader = TRUE,
                status = "primary",
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
                  p("Este mapa de calor mostra a localização das capturas, onde
                    a cor dos círculos varia de verde a roxo, indicando a
                    porcentagem de capturas em cada área. As áreas com uma 
                    porcentagem menor de capturas são representadas em tons 
                    mais claros de verde, enquanto áreas com uma porcentagem
                    maior são exibidas em tons mais escuros de roxo Isso 
                    permite visualizar facilmente as áreas com maior e menor 
                    concentração de capturas.")
                )
              )
            )
          )
        )
      ),
      # Definindo o conteúdo do Administrador
      tabItem(
        tabName = "tab5header",
        # value = "tab5header",
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
          column(
            width = 6,
            # flipBox(
            #   id = "teste",
            #   front = plotlyOutput("histograma_comprimentoM"),
            #   back = plotlyOutput("histograma_comprimentoF"),
            #   trigger = "click"
            # )
            # box(
            #   width = 12,
            #   solidHeader = T,
            #   title = "Histograma",
            #   status = "primary",
            #   plotlyOutput("histograma_comprimento")
            # )
            box(
              width = 12,
              solidHeader = T,
              title = "Histograma de comprimento",
              status = "primary",
              flipBox(
                id = "teste",
                width = 12,
                front = plotlyOutput("histograma_comprimentoM"),
                back = plotlyOutput("histograma_comprimentoF"),
                trigger = "click"
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              title = "Gráfico de Rosca",
              status = "primary",
              plotlyOutput("boxplot_comprimento")
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
          width = 4,
          tags$a(
            href = "http://www.univali.br", target = "_blank",
            tags$img(
              # Saída do Logo da UNIVALI 
              imageOutput("Logo_UNIVALI", height = "100%", width = "100%")
            )
          )
        ),
        column(
          width = 1,
          tags$img(
            # Saída do Logo do LEMA
            imageOutput("Logo_LEMA", height = "100%", width = "100%")
          )
        ),
        column(
          width = 1,
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
    collapsed = FALSE,
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
  
  # # Definindo as Cores de cada Espécie
  # cores <- c(
  #   "Albacora_bandolim" = "#9467bd","Albacora_branca" = "#d62728",
  #   "Albacora_lage" = "#2ca02c", "Meca" = "#8c564b","Outros" = "#ff7f0e", 
  #   "Cacao_azul" = "#1f77b4","Cacao_anequim" = "#7f7f7f", "Prego" = "#e377c2"
  # )
  
  # Cores amigáveis para pessoas com daltonismo
  cores <- c(
    "Albacora_bandolim" = "#9467bd","Albacora_branca" = "#E6194B",
    "Albacora_lage" = "#3CB44B", "Meca" = "#911EB4","Outros" = "#F58231", 
    "Cacao_azul" = "#4363D8","Cacao_anequim" = "#FFE119", "Prego" = "#42D4F4"
  )
  
  # # Definindo as Cores de cada Mês
  # cores_mes <- c(
  #   "Janeiro"="#00CCFF","Fevereiro"="#FF4500","Março"="#32CD32",
  #   "Abril"="#FFD700","Maio"="#FF69B4","Junho"="#8B4513",
  #   "Julho"="#4169E1","Agosto"="#CA70D6","Setembro"="#00FF7F",
  #   "Outubro"="#DCDCDC","Novembro"="#8A2BE2","Dezembro"="#FF0000"
  #   )
  
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
      mutate(mes_nome = nomes_meses[MES]) %>%
      # Formata a coluna mes_ano para o modelo aceitado pelo pacote Plotly
      # mutate(mes_ano = format(as.Date(paste0(ANO, "-", MES, "-01")), "%Y-%m"))
      mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES))))
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
      summarise(prod = sum(KG)) %>%
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
  
  # # Contador de dados Registrados de Cacao-azul em Comparação ao Resto
  # dados_RoscaTubOutros <- reactive({
  #   dados_aux_filtrados() %>%
  #     mutate(
  #       # CATEGORIA = ifelse(CATEGORIA == "Cacao_azul","Cacao_azul","Outros")
  #       CATEGORIA = ifelse(CATEGORIA == "Cacao_azul","Tubarão azul","Outros")
  #     ) %>%
  #     count(CATEGORIA)
  # })
  # 
  dados_BarraTubOutros <- reactive({
    dados_aux_filtrados() %>%
      group_by(MES) %>% 
      mutate(
        CATEGORIA = if_else(
          CATEGORIA != "Cacao_azul", "Outros", CATEGORIA
          )
        ) %>%
      count(CATEGORIA) %>% 
      mutate(prop = n / sum(n)*100) %>% 
      mutate(prop = round(prop,2))
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
      mutate(mes_ano_formatado = format(mes_ano_formatado, "%Y-%m"))
  }) 
  
  # Header ------------------------------------------------------------------

  output$notification_menu <- renderMenu({
    notification_items <- lapply(1:nrow(notificacoes), function(i) {
      notificationItem(
        icon = icon("bell"),
        status = "primary",
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
            # paste(
            #   "Data:", format(
            #     notificacoes$Data[i],
            #     "%d/%m/%Y"
            #     )
            #   ),
            paste(
              "Data:", 
              notificacoes$Data[i]
            ),
            style = "font-weight: bold;"
            ),
          tags$br(),
          tags$span(
            paste(
              "Hora:",
              notificacoes$Horario[i]
              ),
            style = "font-weight: bold;"
            )#,
          # tags$br(),
          # if(!is.na(notificacoes$TextoOpcional[i])){
          #   tags$span(
          #     notificacoes$TextoOpcional[i],
          #     style = "font-style: italic;"
          #   )
          # }
        )
      )
    })
    dropdownMenu(
      type = "notifications",
      icon = icon("bell"),
      .list = notification_items
    )
  })
  
  output$tabelaEmbarcacoes <- renderDT({
    datatable(notificacoes) %>%
      formatDate(
        columns = "Data",
        method = "toLocaleDateString",
        params = list(
          "pt-BR",
          list(
            year = "numeric",
            month = "2-digit",
            day = "2-digit"
            )
          )
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
      width = "72px",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  output$Logo_UNIVALI <- renderImage({
    list(
      src = "dados_brutos/Logo_univali.jpg",
      height = "80px",
      width = "320px",
      contentType = "image/jpg"
    )
  }, deleteFile = FALSE)
  
  output$Logo_LEMA <- renderImage({
    list(
      src = "dados_brutos/Logo_LEMA2.png",
      height = "80px",
      width = "75px",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  output$Logo_MAPA <- renderImage({
    list(
      src = "dados_brutos/logo_MAPA2.png",
      height = "80px",
      width = "300px",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  # Distribuição de Comprimentos --------------------------------------------
  
  output$TubMesAno <- renderPlotly({
    # Calcula a soma total por mês/ano
    data <- dados_TubMesAno() %>%
      group_by(mes_ano_formatado) %>%
      mutate(total = sum(Quantidade)) %>%
      ungroup() %>%
      # Calcula a porcentagem para cada categoria
      mutate(percentage = Quantidade / total*100)
    
    plot_ly(
      data = data,
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
        title = "Dados Registrados por Mês, Ano e Categoria",
        xaxis = list(
          title = "",
          showgrid = FALSE
        ),
        yaxis = list(
          title = "Porcentagem de Dados",
          tickformat = ".0f",
          ticksuffix = '%',
          showgrid = FALSE
        ),
        barmode = "stack",
        showlegend = FALSE,
        hovermode = "x",
        xaxis = list(
          categoryorder = "category ascending"  
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # # Renderização do Gráfico da Comparação de Dados da Tubarão azul para o Resto
  # output$RoscaTubOutros <- renderPlotly({
  #   plot_ly(
  #     data = dados_RoscaTubOutros(),
  #     labels = ~CATEGORIA,
  #     values = ~n,
  #     type = "pie",
  #     hole = 0.6,
  #     textinfo = "label",
  #     hoverinfo = "text+percent",
  #     text = ~paste("Quantidade: ", n),
  #     marker = list(colors = c("Cacao-azul" = "#4363D8", "Outros" = "#F58231"))
  #   ) %>%
  #     layout(
  #       title = "Comparação de Dados da Tubarão azul para o Resto",
  #       showlegend = FALSE
  #     )
  # })
  
  output$BarraTubOutros <- renderPlotly({
    plot_ly(
      data = dados_BarraTubOutros(),
      x = ~MES,
      y = ~prop,
      color = ~CATEGORIA,
      colors = cores,
      type = 'bar',
      text = ~paste(
        "Categoria: ",case_when(
          CATEGORIA == "Cacao_azul" ~ "Tubarão azul",
          CATEGORIA == "Outros" ~ CATEGORIA,
          TRUE ~ CATEGORIA
        ),
        "<br>",
        "Quantidade: ", n, "<br>",
        "Porcentagem: ", round(prop, 2), "%"
      ),
      # text = ~paste0(prop, '%'),
      hoverinfo = 'text'
    ) %>%
      layout(
        # title = "Comparação de Dados da Tubarão Azul para Outros",
        title = "Comparação de Dados Registrados",
        showlegend = FALSE,
        yaxis = list(
          # title = "Porcentagem de Dados",
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
        )
      )
  })
  
  # # Renderização do Gráfico da Comparação de Dados da Tubarão azul por Mês
  # output$ComparaDadosTub <- renderPlotly({
  #   
  #   dados_modificados <- dados_ComparaDadosTub() %>%
  #     mutate(CATEGORIA = recode(CATEGORIA, "Cacao_azul" = "Tubarão azul"))
  #   
  #   plot_ly(
  #     data = dados_modificados,
  #     labels = ~mes_nome,
  #     parents = ~CATEGORIA,
  #     values = ~Quantidade,
  #     type = "sunburst",
  #     branchvalues = "total", 
  #     hoverinfo = "percent entry+value",
  #     textinfo = "label",
  #     marker = list(
  #       colors = cores_mes[as.character(dados_modificados$mes_nome)]
  #     )
  #   ) %>%
  #     layout(
  #       title = "Dados Registrados de Tubarão azul por Mês",
  #       showlegend = FALSE
  #     )
  # })
  
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
        title = "Comparação de Dados Registrados",
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
        showlegend = F
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
        "Espécie: ", 
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
        title = "Média Mensal de Captura por Viagem",
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_graficoCaptura()$MES), 
          ticktext = unique(dados_graficoCaptura()$MES)
        ),
        yaxis = list(
          title = "Captura Média (KG) por Viagem"
        ), 
        showlegend = FALSE,
        hovermode = "x"
      )
  })
  
  output$graficoAreaDesembarque <- renderPlotly({
    
    dados_captura <- dados_graficoAreaDesembarque()
    
    data <- data.frame("mes_ano"=dados_captura$mes_ano, dados_captura)
    
    data_wide <- pivot_wider(
      data, names_from = CATEGORIA, values_from = Media_KG_por_Viagem)
    
    data_wide <- data_wide %>% 
      plotly::select(-c("ANO","MES", "mes_nome", "mes_ano.1"))
    
    data_wide_filtrado <- data_wide[, c("mes_ano","Cacao_azul", input$species)]
    
    req(ncol(data_wide_filtrado) > 1)
    plot_data <- plot_ly(
      data = data_wide_filtrado,
      x = ~mes_ano,
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
        title = 'Média Mensal de Captura por Viagem ao Longo do Período',
        xaxis = list(
          title = "",
          showgrid = FALSE
        ),
        yaxis = list(
          title = "Porcentagem da Captura Média por Viagem",
          showgrid = FALSE,
          ticksuffix = '%'
        ),
        showlegend = FALSE,
        hovermode = "x"
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
        title = "Média Mensal de Captura por Viagem",
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
        showlegend = F
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
        lng = -40, lat = -28, zoom = 5
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
        label = paste0(
          "Captura: ", round(tab01$prod, 0), " kg"
        )
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
      },options = list(paging = TRUE, searching = FALSE))
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
      type = 'histogram'
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
      type = 'histogram'
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
    datatable(notificacoes[, !names(notificacoes) %in% c("TextoOpcional", "Link")])
  })
  
  observeEvent(input$tabela_embarcacoes_rows_selected, {
    i <- input$tabela_embarcacoes_rows_selected
    if (length(i) == 1) {
      # Verifica se há link disponível e cria o título do modal
      title_text <- paste("Detalhes da Embarcação", notificacoes$id[i])
      title_text <- tags$a(title_text, href = notificacoes$Link[i], target = "_blank")
      
      
      # Verifica se há informações extras disponíveis
      info_extra <- if(!is.na(notificacoes$TextoOpcional[i])){
        tags$span(
          paste(
            "Texto:"
          ),
          style = "font-weight: bold;",
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

  # output$lista_embarcacoes <- renderUI({
  #   # tabela_embarcacoes <- 
  #   lapply(1:nrow(notificacoes), function(i) {
  #     box(
  #       title = notificacoes$Titulo[i],
  #       width = 12,
  #       status = "primary",
  #       solidHeader = T,
  #       collapsible = T,
  #       collapsed = T,
  #       closable = T,
  #       tags$div(
  #         tags$span(
  #           paste(
  #             "Local:",
  #             notificacoes$Local[i]
  #           ),
  #           style = "font-weight: bold;"
  #         ),
  #         br(),
  #         tags$span(
  #           paste(
  #             "Data:", format(
  #               notificacoes$Data[i],
  #               "%d/%m/%Y"
  #             )
  #           ),
  #           style = "font-weight: bold;"
  #         ),
  #         tags$br(),
  #         tags$span(
  #           paste(
  #             "Hora:",
  #             sprintf(
  #               "%02d:%02d",
  #               notificacoes$Hora[i],
  #               notificacoes$Minuto[i]
  #             )
  #           ),
  #           style = "font-weight: bold;"
  #         ),
  #         tags$br(),
  #         if(!is.na(notificacoes$TextoOpcional[i])){
  #           tags$span(
  #             paste(
  #               "Texto:"
  #             ),
  #             style = "font-weight: bold;",
  #             br(),
  #             paste(
  #               notificacoes$TextoOpcional[i]
  #             )
  #           )
  #         }
  #       )
  #     )
  #   })
  # })
  
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
