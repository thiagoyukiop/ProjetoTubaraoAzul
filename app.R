# Bibliotecas -------------------------------------------------------------

# Carregando os Pacotes que serão utilizados no dashboard
pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus,# shinyauthr,
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
# notificacoes <- dbReadTable(db, "Notificacoes")
dados_falsos <- dbReadTable(db, "Dados_falsos")

notificacoes <- read.csv("dados_brutos/NotificacoesTabela.csv", fileEncoding = "UTF-8")

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

# # Cria a coluna 'Horário' e remove as colunas 'Hora' e 'Minuto'
# notificacoes <- notificacoes %>%
#   mutate(Horário = sprintf("%02d:%02d", Hora, Minuto)) %>%
# dplyr::select(-c(Hora, Minuto))

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
    # titleWidth = 300,
    # titleWidth = 250,
    titleWidth = 230,
    # Definição do Título com Link da Header
    # title = tags$a( # Cria uma tag que define um hyperlink
    #   href = "https://lrpdc.shinyapps.io/proj_tubarao_azul/", # Link URL
    #   target = "_blank", # Abre o link em uma nova aba
    #   # Cria uma tag que é um contêiner em linha usado para aplicar estilos
    #   tags$span(
    #     # Saída de Icon ou Título depende da situação do sidebar
    #     uiOutput("textoHeader")
    #   ),
    #   class = "logo"
    # ),
    title = uiOutput("textoHeader"),
    controlbarIcon = icon("sliders"), # Definição do ícone da aba de Controle
    # Definição do Menu Suspenso
    dropdownMenuOutput("notification_menu")#,
    # leftUi = tagList(
    #   # Dropdown para login com UI personalizada
    #   dropdownBlock(
    #     id = "loginDropdown",
    #     title = "Login",
    #     icon = icon("user"),
    #     badgeStatus = NULL,
    #     loginUI(
    #       id = "login",
    #       title = "Login",
    #       user_title = "Nome do usuário",
    #       pass_title = "Senha",
    #       error_message = "Usuário ou senha incorretos",
    #       login_title = "Entrar"
    #     )
    #   ),
    #   div(
    #     class = "pull-right",
    #     logoutUI(
    #       id = "logout",
    #       icon = icon("right-from-bracket")
    #     )
    #   )
    # ),
    # # Renderiza o usuário autenticado
    # userOutput("user")
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
    width = 230,
    # width = 250,
    # width = 300, # Definição da Largura em pixels
    minified = TRUE,  # Se a aba lateral ao ser fechada deverá mostrar os ícones
    collapsed = FALSE, # Se a aba lateral deve ser iniciada fechada
    sidebarMenu(
      id = "sidebarMenu",
      menuItem(
        text = "Apresentação",
        icon = icon("house"),
        menuSubItem(
          text = "Projeto",
          tabName = "projeto",
          icon = icon("r-project")
        ),
        menuSubItem(
          text = "Leia-me",
          tabName = "leia_me",
          icon = icon("readme")
        ),
        menuSubItem(
          text = "Sobre",
          tabName = "sobre",
          icon = icon("circle-info")
        )
      ),
      menuItem(
        text = "Distribuição",
        icon = icon("chart-bar"),
        menuSubItem(
          text = "Captura",
          tabName = "captura",
          icon = icon("chart-pie")
        ),
        menuSubItem(
          text = "Comprimento",
          tabName = "comprimento",
          icon = icon("chart-simple")
        )
      ),
      menuItem(
        text = "Desembarque",
        tabName = "desembarque",
        icon = icon("chart-area")
      ),
      menuItem(
        text = "Distribuição espacial",
        icon = icon("globe"),
        menuSubItem(
          text = "Captura",
          tabName = "captura_espacial",
          icon = icon("earth-americas")
        ),
        menuSubItem(
          text = "Comprimento",
          tabName = "comprimento_espacial"#,
          # icon = icon("")
        )
      ),
      # menuItem(
      #   text = "Administrador",
      #   tabName = "administrador",
      #   icon = icon("user-tie")
      # ),
      menuItem(
        text = "Tabela de embarcações",
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
    # tags$script(HTML("
    #   Shiny.addCustomMessageHandler('sidebarState', function(collapsed) {
    #     if (collapsed) {
    #       // Sidebar is closed
    #       $('#Logo_FURG').css('width', '52%');
    #     } else {
    #       // Sidebar is open
    #       $('#Logo_FURG').css('width', '80%');
    #     }
    #   });
    # ")),
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
                  h6("Tubarões Medidos"),
                  style = "display: block; text-align: center;"
                ),
                # title = "Tubarões Medidos",
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
              # offset = 6,
              width = 5,
              infoBox(
                title = tags$div(
                  h6("Entrevista de Desembarque"),
                  style = "display: block; text-align: center;"
                ),
                # title = "Entrevista de Desembarque",
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
                  h6("Cadernos de Bordo"),
                  style = "display: block; text-align: center;"
                ),
                # title = "Cadernos de Bordo",
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
              # offset = 6,
              width = 5,
              infoBox(
                title = tags$div(
                  h6("Embarcações Monitoradas"),
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
              offset = 2, # Definindo Deslocamento da Coluna
              # Definindo Texto do Projeto
              tags$div(
                h3("Geração de subsídios e elaboração do Plano de gestão da
                   pesca do Tubarão Azul, e monitoramento da atividade no 
                   Estado do Rio Grande do Sul"),
                style = "text-align:center;"
              ),
              br(),
              tags$div(
                style = "text-align:center;",
                h4("Como surgiu o Projeto Tubarão Azul?")
              ),
              # Cria uma tag que define um parágrafo de texto 
              tags$div(
                style = "text-align:justify;",
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
                br()#,
              ),
              tags$div(
                style = "text-align:center;",
                h4("Legal, mas o que é um Plano de Gestão da Pesca?")
              ),
              tags$div(
                style = "text-align:justify;",
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
                br()
              ),
              tags$div(
                style = "text-align:center;",
                h4("Fluxograma do Plano de Gestão da pesca de Tubarão azul 
                   no Rio Grande do Sul")
              ),
              div(
                style = "text-align: center;",
                # Saída da Imagem do Fluxograma do Projeto
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
                  efetuar login para visualizar essas informações."),
                  p("Na aba “Distribuição de comprimentos”, você encontrará as 
                  composições de comprimentos dos indivíduos amostrados em cada
                  ano para machos e fêmeas. Está disponível também a proporção 
                  de sexos dos indivíduos capturados."),
                  p("Na aba “Tabela de embarcações” você encontrará uma tabela 
                  que contém dados sobre as embarcações que já ocorreram e das
                  embarcações que irão ocorrer, ao clicar na linha de uma 
                  embarcação específica, é possível verificar mais informações
                  sobre tal."),
                  p("Para a construção dos gráficos apresentados nesta plataforma
                  são utilizados dados atualizados anualmente."),
                  p("Para maiores informações, por favor, entre em contato através
                  do e-mail ",strong("proj.tubaraoazul.furg@gmail.com."))
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "sobre"#,
        
      ),
      # Definindo o conteúdo da Distribuição de Captura
      tabItem(
        tabName = "captura",
        fluidRow(
          column(
            width = 12,
            # Definindo Caixa com conteúdo da Distribuição de Captura
            box(
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
      ),
      # Definindo o conteúdo de Desembarques
      tabItem(
        tabName = "desembarque",
        fluidRow(
          column(
            width = 6,
            box(
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
              title ='Média Mensal de Captura por Viagem ao Longo do Período',
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
      ),
      # Definindo o conteúdo da Distribuição Espacial das Capturas
      tabItem(
        tabName = "captura_espacial",
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              title = "Mapas",
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
                # p("Este mapa de calor mostra a localização das capturas, com o
                # valor total de Quilos capturados, onde a cor dos círculos
                # varia de verde a roxo, indicando a porcentagem de capturas
                # em cada área. As áreas com uma porcentagem menor de capturas
                # são representadas em tons mais claros de verde, enquanto
                # áreas com uma porcentagem maior são exibidas em tons mais
                # escuros de roxo. Isso permite visualizar facilmente as
                # áreas com maior e menor concentração de capturas.")
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
              title = "Histograma de Comprimento",
              status = "primary",
              plotlyOutput("histograma_comprimento"),
              sidebar = boxSidebar(
                id = "boxsidebar10",
                icon = icon("circle-info"),
                width = 50,
                background = "#A6ACAFEF",
                p("Este é um histograma do comprimento de Tubarões azul machos
                  e fêmeas. Que indica a distribuição de comprimento por 
                  intervalos específicos, que estão em centímetros. No filtro
                  é possível trocar o sexo da espécie, no histograma.")
              )
            )
          ),
          column(
            width = 6,
            box(
              width = 12,
              solidHeader = T,
              title = "Distribuição de Comprimento de Tubarões azul",
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
      # tabItem(
      #   tabName = "comprimento_espacial",
      #   fluidRow(
      #     column(
      #       width = 12,
      #       box(
      #         width = 12,
      #         solidHeader = T,
      #         title = "Mapa de Capturas",
      #         status = "primary",
      #         div(
      #           class = "mapa",
      #           # Saída do Gráfico do Mapa de Calor
      #           leafletOutput("MapaComprimento",height = "100%")
      #         ),
      #         sidebar = boxSidebar(
      #           id = "boxsidebar12",
      #           icon = icon("circle-info"),
      #           width = 30,
      #           background = "#A6ACAFEF",
      #           p("")
      #         )
      #       )
      #     )
      #   )
      # ),
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
          h4("Instituições Executoras")
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
            style = "margin-left: 50px; margin-top: -15px; margin-bottom: -30px;
          padding-right: 0px;",
            h4("Apoio"),
            br()
          ),
          imageOutput("Logo_MAPA",height = "100%", width = "100%")
          # tags$div(
          #   style = "margin-right: 20px; padding-right: 0px;",
          #   tags$a(
          #     href = "https://www.gov.br/mpa/pt-br", target = "_blank",
          #     # Saída do Logo do MAPA
          #     imageOutput("Logo_MAPA",height = "100%", width = "100%")
          #   )
          # )
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
    # width = 300,
    # width = 250,
    width = 230,
    # Definindo controlbar Menu
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        title = "Filtros",
        icon = icon("filter"),
        conditionalPanel(
          condition = "input.sidebarMenu == 'captura'",
          sliderInput(
            inputId = "anos_captura",
            label = "Intervalo de Anos:",
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
            label = "Seletor de Espécies:",
            choiceValues = c(
              "Albacora_bandolim", "Albacora_branca", "Albacora_lage",
              "Cacao_anequim", "Meca", "Outros", "Prego"
              ),
            choiceNames = c(
              "Albacora bandolim", "Albacora branca", "Albacora lage",
              "Cação Anequim", "Meca", "Outros", "Prego"
              ),
            selected = dados_ajustados$CATEGORIA
          )#,
          # actionButton(
          #   inputId = "selectAll_captura",
          #   label = "Todos",
          #   icon = icon("square-check")
          # ),
          # actionButton(
          #   inputId = "deselectAll_captura",
          #   label = "Nenhum",
          #   icon = icon("square")
          # )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'comprimento'",
          radioButtons(
            inputId = "sexo_comprimento",
            label = "Seletor de Sexo:",
            choiceValues = c("Todos", "M", "F"),
            choiceNames = c("Todos", "Macho", "Femea"),
            selected = "Todos"
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'desembarque'",
          sliderInput(
            inputId = "anos_desembarque",
            label = "Intervalo de Anos:",
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
            label = "Seletor de Espécies:",
            choiceValues = c("Albacora_bandolim", "Albacora_branca", "Albacora_lage",
                             "Cacao_anequim", "Meca", "Outros", "Prego"),
            choiceNames = c("Albacora bandolim", "Albacora branca", "Albacora lage",
                            "Cação Anequim", "Meca", "Outros", "Prego"),
            selected = dados_ajustados$CATEGORIA
          )
        ),
        conditionalPanel(
          condition = "input.sidebarMenu == 'captura_espacial'",
          sliderInput(
            inputId = "anos_cap_esp",
            label = "Intervalo de Anos:",
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
            label = "Seletor de Espécies:",
            choiceValues = c("Albacora_bandolim", "Albacora_branca", "Albacora_lage",
                             "Cacao_anequim", "Meca", "Outros", "Prego"),
            choiceNames = c("Albacora bandolim", "Albacora branca", "Albacora lage",
                            "Cação Anequim", "Meca", "Outros", "Prego"),
            selected = dados_ajustados$CATEGORIA
          ),
          radioButtons(
            inputId = "mapa_cap_esp",
            label = "Seletor de Mapa:",
            choiceValues = c("KilosTotais", "KiloPorViagem", "Viagens"),
            choiceNames = c("Quilos Totais", "Quilos por Viagem", "Viagens"),
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
            label = "Defina o Status das Embarcações",
            choices = c("Todos", "Hoje", "Passado", "Futuro"),
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
  
  # Definindo nome dos meses
  nomes_meses <- c(
    "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho","Julho",
    "Agosto", "Setembro", "Outubro", "Novembro","Dezembro"
  )
  
  categorias <- unique(dados_ajustados$CATEGORIA)
  
  cores <- brewer.pal(n = 8, name = "Set1")
  
  cores <- cores[cores != "#377EB8"]
  
  cores_categoria <- setNames(c(rep(NA, length(categorias))), categorias)
  
  cores_categoria["Cacao_azul"] <- "#377EB8"
  
  outras_categorias <- categorias[categorias != "Cacao_azul"]
  cores_categoria[outras_categorias] <- cores[1:length(outras_categorias)]
  
  # Filtro de Dados ---------------------------------------------------------
  
  # # Filtrando os Dados Gerais Completos Reativamente
  # dados_gerais_filtrados <- reactive({
  #   # Filtrando as Espécies
  #   dados_aux <- subset(
  #     dados_gerais, CATEGORIA %in% union(input$species, "Cacao_azul"))
  #   # Filtrando o Intervalo de Anos
  #   dados_aux <- subset(
  #     dados_aux,
  #     ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
  #     )
  #   data.frame(dados_aux)
  # })
  
  # dados_captura_filtrada <- reactive({
  #   dados_aux <- subset(
  #     dados_gerais,
  #     ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
  #     )
  #   data.frame(dados_aux)
  # })
  
  # # Filtrando os Dados da Tabela Inicial
  # dados_aux_filtrados <- reactive({
  #   # Filtrando as Espécies 
  #   dados_aux <- subset(
  #     dados_ajustados, CATEGORIA %in% union(input$species, "Cacao_azul")
  #     )
  #   # Filtrando o Intervalo de Anos
  #   dados_aux <- subset(
  #     dados_aux,
  #     ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
  #     )
  #   data.frame(dados_aux)
  # })
  
  # # Fazendo o cálculo da Média de Captura por Kg, por Viagem, por Mês/Ano
  # dados_graficoAreaDesembarque <- reactive({
  #   dados_captura_filtrada() %>%
  #     mutate(KG_por_Viagem = (KG/DESCARGA)) %>% 
  #     # Agrupa os Dados por Colunas Selecionadas
  #     group_by(CATEGORIA, ANO, MES) %>% 
  #     # Média das Toneladas de Captura de Cada Grupo
  #     summarise(Media_KG_por_Viagem = mean(KG_por_Viagem)) %>%
  #     # Substituindo NAs por Zero
  #     mutate(Media_KG_por_Viagem = replace_na(Media_KG_por_Viagem, 0)) %>% 
  #     # Arredondando a Média de Toneladas para Duas Casas Decimais
  #     mutate(Media_KG_por_Viagem = round(Media_KG_por_Viagem, 2)) %>%
  #     mutate(mes_ano_formatado = make_date(ANO, MES)) %>%
  #     mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES)))) %>%
  #     mutate(mes_ano_formatado = format(mes_ano_formatado, "%Y-%m")) 
  # })
  
  # # Filtrando Dados para o Gráfico de Captura
  # dados_graficoCaptura <- reactive({
  #   dados_gerais_filtrados() %>%
  #     mutate(KG_por_Viagem = (KG/DESCARGA)) %>%
  #     group_by(CATEGORIA, ANO, MES) %>%
  #     summarise(Media_KG_por_Viagem = mean(KG_por_Viagem)) %>%
  #     # Substituindo NAs por Zero
  #     mutate(Media_KG_por_Viagem = replace_na(Media_KG_por_Viagem, 0)) %>%
  #     group_by(CATEGORIA, MES) %>%
  #     # Média das Toneladas de Captura, de cada Mês, com Anos Agrupados
  #     summarise(MediaKG_Mes_Viagem = mean(Media_KG_por_Viagem)) %>%
  #     mutate(MediaKGMesViagem = round(MediaKG_Mes_Viagem, 2)) %>%
  #     dplyr::select(-MediaKG_Mes_Viagem) %>%
  #     mutate(mes_nome = nomes_meses[MES])
  # })
  
  # # Filtrando os Dados da Tabela Inicial com somente a CATEGORIA Cacao-azul
  # dadostub_aux_filtrados <- reactive({
  #   dados_auxiliar <- subset(dados_ajustados, CATEGORIA == "Cacao_azul")
  #   subset(
  #     dados_auxiliar,
  #     ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
  #   )
  # })
  
  # Filtrando dados Para o Mapa
  # db_filtrado <- reactive({
  #   dados_aux <- subset(
  #     dados_ajustados,CATEGORIA %in% union(input$species, "Cacao_azul")
  #     )
  #   dados_aux <- subset(
  #     dados_aux,
  #     ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
  #     )
  #   tab01 <- dados_aux %>%
  #     group_by(LON, LAT) %>%
  #     summarise(
  #       prod = sum(KG),
  #       prod2 = sum(KG)/sum(DESCARGA),
  #       viagem = sum(DESCARGA)
  #       ) %>%
  #     ungroup()
  #   list(dados = dados_aux, tab01 = tab01)
  # })

  # # Fazendo o cálculo da captura por mês
  # dados_PesoMes <- reactive({ 
  #   dados_aux_filtrados() %>%
  #     mutate(KG_por_Viagem = (KG/DESCARGA)) %>%
  #     complete(CATEGORIA, ANO, MES, fill = list(KG_por_Viagem = 0)) %>% 
  #     group_by(CATEGORIA, ANO, MES) %>%
  #     summarise(MedKGPorViagemMesAno = mean(KG_por_Viagem)) %>%
  #     mutate(MedKGPorViagemMesAno = replace_na(MedKGPorViagemMesAno, 0)) %>% 
  #     group_by(CATEGORIA, MES) %>%
  #     summarise(Media_KG_por_Viagem = mean(MedKGPorViagemMesAno)) %>%
  #     mutate(Media_KG = round(Media_KG_por_Viagem, 2)) %>%
  #     mutate(mes_nome = nomes_meses[MES]) %>% 
  #     mutate(
  #       CATEGORIA = case_when(
  #         CATEGORIA == "Albacora_bandolim" ~ "Albacora bandolim",
  #         CATEGORIA == "Albacora_branca" ~ "Albacora branca",
  #         CATEGORIA == "Albacora_lage" ~ "Albacora lage",
  #         CATEGORIA == "Cacao_anequim" ~ "Cação anequim",
  #         CATEGORIA == "Cacao_azul" ~ "Tubarão azul",
  #         TRUE ~ CATEGORIA
  #       )
  #     )
  # })
  
  # # Fazendo o cálculo de Dados Totais Registrados por Mes de Cacao-azul
  # dados_ComparaDadosTub <- reactive({
  #   dadostub_aux_filtrados() %>%
  #     group_by(CATEGORIA, MES, ANO) %>%
  #     summarise(Quantidade = n()) %>%
  #     ungroup() %>%
  #     complete(CATEGORIA, MES = 1:12, fill = list(Quantidade = 0)) %>% 
  #     mutate(mes_nome = nomes_meses[MES])
  # }) 
  
  # dados_aux_filtrados <- reactive({
  #   # Filtrando as Espécies 
  #   dados_aux <- subset(
  #     dados_ajustados, CATEGORIA %in% union(input$species, "Cacao_azul")
  #   )
  #   # Filtrando o Intervalo de Anos
  #   dados_aux <- subset(
  #     dados_aux,
  #     ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
  #   )
  #   data.frame(dados_aux)
  # })
  # 
  # # Dividindo os dados em duas categorias, e fazendo a proporção de dados
  # dados_BarraTubOutros <- reactive({
  #   dados_aux_filtrados() %>%
  #     group_by(MES) %>% 
  #     mutate(
  #       CATEGORIA = if_else(
  #         CATEGORIA != "Cacao_azul", "Outros", CATEGORIA
  #         )
  #       ) %>%
  #     count(CATEGORIA) %>% 
  #     mutate(prop = (n / sum(n)) * 100) %>% 
  #     mutate(media = round(n / 12, 2))
  # })
  
  # # Dividindo os dados Registrados de Cacao-azul em Comparação ao Resto e 
  # # Completando os Mês/Ano sem registros, completando com Zero
  # dados_TubMesAno <- reactive({
  #   dados_aux_filtrados() %>%
  #     mutate(CATEGORIA=if_else(CATEGORIA!="Cacao_azul","Outros",CATEGORIA)) %>%
  #     group_by(CATEGORIA, ANO, MES) %>%
  #     summarise(Quantidade = n()) %>%
  #     ungroup() %>%
  #     complete(CATEGORIA, ANO, MES = 1:12, fill = list(Quantidade = 0)) %>%
  #     filter(!(ANO == 2024 & MES >= 5)) %>%
  #     mutate(mes_ano_formatado = make_date(ANO, MES)) %>%
  #     mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES)))) %>%
  #     mutate(mes_ano_formatado = format(mes_ano_formatado, "%Y-%m")) %>% 
  #     group_by(mes_ano_formatado) %>%
  #     mutate(total = sum(Quantidade)) %>%
  #     ungroup() %>%
  #     # Calcula a porcentagem para cada categoria
  #     mutate(percentage = Quantidade / total*100)
  # }) 
  
  # Header ------------------------------------------------------------------
  
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
                  "Ocorrerá/Ocorreu Hoje"
                ), style = "color: #a94442;"
              )
            }
            else{
              tags$span(
                paste(
                  "Ocorrerá em ",
                  notificacoesTabela$DiasRestantes[i],
                  "dias"
                ), style = "color: #337ab7;"
              )
            }
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
    list(
      src = "dados_brutos/logo_tuba_azul_3.png", # Local do arquivo da Imagem
      # height = "100%",                     # Altura da Imagem
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
      dados_ajustados, CATEGORIA %in% union(input$especies_captura, "Cacao_azul")
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
        " Data: ", mes_ano, "<br>",
        "Categoria: ",case_when(
          CATEGORIA == "Cacao_azul" ~ "Tubarão azul",
          CATEGORIA == "Outros" ~ CATEGORIA,
          TRUE ~ CATEGORIA
        ),
        "<br>",
        "Quantidade: ", Quantidade, "<br>",
        "Porcentagem: ", round(percentage, 2), "%"
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
      dados_ajustados, CATEGORIA %in% union(input$especies_captura, "Cacao_azul")
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
      complete(CATEGORIA, MES = 1:12, fill = list(Quantidade = 0)) %>% 
      mutate(mes_nome = nomes_meses[MES])
  }) 
  
  output$ComparaDadosTub <- renderPlotly({
    plot_ly(
      dados_ComparaDadosTub(),
      x = ~MES,
      y = ~ANO,
      z = ~Quantidade,
      type = "heatmap",
      colorscale = "Plasma",
      hoverinfo = "text",
      text = ~paste(
        " Mês: ", mes_nome, "<br>",
        "Ano: ", ANO, "<br>",
        "Dados Registrados: ", Quantidade, "<br>"
      )
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_ComparaDadosTub()$MES),
          ticktext = unique(dados_ComparaDadosTub()$MES),
          showgrid = F
          ),
        yaxis = list(
          title = "Ano",
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
      dados_gerais, CATEGORIA %in% union(input$especies_desembarque, "Cacao_azul"))
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
      dplyr::select(-MediaKG_Mes_Viagem) %>%
      mutate(mes_nome = nomes_meses[MES])
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
        size = 10#, # Tamanho do Marcador
        ), 
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
        "Média de Captura por Viagem: ", MediaKGMesViagem, "kg <br>"
      ),
      hoverlabel = list(
        font = list(
          size = 11 # Tamanho da fonte do hover
        )
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Mês",
          tickvals = unique(dados_graficoCaptura()$MES),
          ticktext = unique(dados_graficoCaptura()$MES),
          showgrid = F
        ),
        yaxis = list(
          title = "Captura Média (KG) por Viagem",
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
      name = "Tubarão Azul",
      fillcolor = "#377EB8",
      text = ~paste(
        " Espécie: ", 'Tubarão azul', "<br>",
        "Data: ", mes_ano, "<br>",
        "Média de Captura por Viagem: ", Cacao_azul, "kg <br>"
      )
    )
    if(any(names(data_wide_filtrado) == "Albacora_bandolim")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_bandolim,
          name = "Albacora bandolim",
          fillcolor = "#F781BF",
          text = ~paste(
            " Espécie: ", 'Albacora bandolim', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de Captura por Viagem: ", Albacora_bandolim, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Albacora_branca")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_branca,
          name = 'Albacora branca',
          fillcolor = '#E41A1C',
          text = ~paste(
            " Espécie: ", 'Albacora branca', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de Captura por Viagem: ", Albacora_branca, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Albacora_lage")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Albacora_lage,
          name = 'Albacora lage',
          fillcolor = '#4DAF4A',
          text = ~paste(
            " Espécie: ", 'Albacora lage', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de Captura por Viagem: ", Albacora_lage, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Cacao_anequim")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Cacao_anequim,
          name = 'Cação anequim',
          fillcolor = '#984EA3',
          text = ~paste(
            " Espécie: ", 'Cação anequim', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de Captura por Viagem: ", Cacao_anequim, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Meca")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Meca,
          name = 'Meca',
          fillcolor = '#FFFF33',
          text = ~paste(
            " Espécie: ", 'Meca', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de Captura por Viagem: ", Meca, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Outros")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Outros,
          name = 'Outros',
          fillcolor = '#FF7F00',
          text = ~paste(
            " Espécie: ", 'Outros', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de Captura por Viagem: ", Outros, "kg <br>"
          )
        )
    }
    if(any(names(data_wide_filtrado) == "Prego")) {
      plot_data <- plot_data %>% 
        add_trace(
          y = ~Prego,
          name = 'Prego',
          fillcolor = '#A65628',
          text = ~paste(
            " Espécie: ", 'Prego', "<br>",
            "Data: ", mes_ano, "<br>",
            "Média de Captura por Viagem: ", Prego, "kg <br>"
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
  
  output$pesoMes <- renderPlotly({
    plot_ly(
      data = dados_PesoMes_desembarque(),
      x = ~MES,
      y = ~CATEGORIA,
      z = ~Media_KG,
      type = "heatmap",
      colorscale = "Viridis",
      hoverinfo = "text",
      text = ~paste(
        " Mês: ", mes_nome, "<br>",
        "Categoria: ", CATEGORIA, "<br>",
        "Média de Captura por Viagem: ", Media_KG, "kg <br>"
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Mês",
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
      dados_ajustados,CATEGORIA %in% union(input$especies_cap_esp, "Cacao_azul")
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
      palette = "Blues",
      domain = tab01$prod,
      probs = seq(0, 1, 0.1)
    )
    
    Mapa_Capturas_Totais <- leaflet() %>%
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
        lng = -40,
        lat = -28,
        zoom = 4
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
        pal = pal,
        values = tab01$prod,
        group = tab01$prod,
        position = "bottomright",
        title = "Percentual da Captura"
      ) %>%
      # Controle de Estilo de Mapa
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
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
    
    breaksPorViagem <- quantile(tab01$prod2, probs = seq(0, 1, 0.1), na.rm = TRUE)
    
    if (any(duplicated(breaksPorViagem))) {
      tab01$prod2 <- jitter(tab01$prod2, factor = 0.1)
    }
    
    pal <- colorQuantile(
      palette = "Blues",
      domain = tab01$prod2,
      probs = seq(0, 1, 0.1)
    )
    
    Mapa_kg_por_viagem <- leaflet() %>%
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
        group = "Marcadores Circulares",
        radius = 7,
        lng = tab01$LON,
        lat = tab01$LAT, 
        stroke = FALSE,
        color = pal(tab01$prod2),
        fillOpacity = 0.7,
        label = paste0(
          "Captura: ", round(tab01$prod2, 0), " kg"
        )
      ) %>%
      addLegend(
        pal = pal, values = tab01$prod2, group = tab01$prod,
        position = "bottomright", title = "Percentual da Captura"
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
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
    
    breaksViagem <- quantile(tab01$viagem, probs = seq(0, 1, 0.1), na.rm = TRUE)
    
    if (any(duplicated(breaksViagem))) {
      tab01$viagem <- jitter(tab01$viagem, factor = 0.1)
    }
    
    pal <- colorQuantile(
      palette = "Blues",
      domain = tab01$viagem,
      probs = seq(0, 1, 0.1)
    )
    
    Mapa_Viagem <- leaflet() %>%
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
        group = tab01$viagem,
        radius = 7,
        lng = tab01$LON,
        lat = tab01$LAT,
        stroke = FALSE,
        color = pal(tab01$viagem), 
        fillOpacity = 0.7,
        label = paste0("Viagens: ", round(tab01$viagem),0)
      ) %>%
      addLegend(
        pal = pal, values = tab01$viagem, group = tab01$viagem,
        position = "bottomright", title = "Percentual da Viagens"
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
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
      p("Este mapa de calor mostra a localização das capturas, com o valor total
      de Quilos capturados, onde a cor dos círculos varia de verde a roxo, 
      indicando a porcentagem de capturas em cada área. As áreas com uma
      porcentagem menor de capturas são representadas em tons mais claros de 
      verde, enquanto áreas com uma porcentagem maior são exibidas em tons mais
      escuros de roxo. Isso permite visualizar facilmente as áreas com maior e 
      menor concentração de capturas.")
    } else if (input$mapa_cap_esp == "KiloPorViagem") {
      p("Este mapa de calor mostra a localização das capturas, com o valor em 
      Quilos por Viagem, onde a cor dos círculos varia de verde a roxo, 
      indicando a porcentagem de capturas em cada área. As áreas com uma 
      porcentagem menor de capturas são representadas em tons mais claros de 
      verde, enquanto áreas com uma porcentagem maior são exibidas em tons mais 
      escuros de roxo. Isso permite visualizar facilmente as áreas com maior e
      menor média de concentração de capturas.")
    } else if (input$mapa_cap_esp == "Viagens") {
      p("Este mapa de calor mostra a localização das viagens, onde a cor dos 
      círculos varia de verde a roxo, indicando a porcentagem de viagens em cada
      área. As áreas com uma porcentagem menor de viagens são representadas em
      tons mais claros de verde, enquanto áreas com uma porcentagem maior são
      exibidas em tons mais escuros de roxo. Isso permite visualizar facilmente 
      as áreas com maior e menor concentração de viagens.")
    }
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
          title = "Frequência Relativa",
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
          title = "Comprimento (cm)"
        )
      )
  })
  
# Tabela de Embarcações ---------------------------------------------------
  
    output$tabela_embarcacoes <- renderDT({
  
      colnames(notificacoesTabela) <- c(
        "Status", "Embarcação", "Aviso de Desembarque",
        "Data do Desembarque", "Saída", "Chegada",
        "Indivíduos Medidos de Tubarão Azul",
        "Indivíduos Medidos de Tubarão Anequim", "DiasRestantes"
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
            list(className = 'dt-center', targets = "_all")  # Centraliza o texto
          ),
          # order = list(list(4, 'desc'))
          order = list(list(3, 'desc'))
        ),
        class = "cell-border stripe hover",
        selection = "single"
      ) %>%
        formatDate(
          c('Aviso de Desembarque',
            'Data do Desembarque',
            'Saída',
            'Chegada'),
          method = "toLocaleDateString",
          params = list("pt-BR")
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
