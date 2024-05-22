# Bibliotecas -------------------------------------------------------------

pacman::p_load(
  shiny, shinydashboard, shinydashboardPlus, apexcharter, leaflet, dplyr, 
  scales, plotly,zoo, tidyverse, digest, DT
)

# Carregando os Dados de um Arquivo csv
dados_aux <- read.table("dados_brutos/dados_17_24_Outros.csv",
                        header = TRUE, sep = ",", dec = ".")

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
        icon = icon("earth-americas"),
        expandedName = "Distribuição espacial das capturas2"
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
    # Ajustando Visualização de Mapa para que sempre fique com a altura ideal
    tags$head(tags$style(HTML(' 
    .mapa {
    display: flex;
    width: 100%;
    height: calc(100vh - 145px);
    visibility: inherit;
    position: relative;
    z-index: 100;
    }
    
    .graficos {
    display: flex;
    width: 100%;
    height: calc(50vh - 120px);
    visibility: inherit;
    position: relative;
    z-index: 100;
    }
    
      .direct-chat-contacts {
        z-index: 100 !important;
      }
                              ')
    )
    ),
    tabItems(
      # Definindo o conteúdo do Projeto
      tabItem(
        tabName = "tab1body",
        fluidPage(
          fluidRow(
            column(
              offset = 2,
              width = 9,
              carousel(
                width = 12,
                id = "mycarousel",
                indicators = F,
                carouselItem(
                  infoBox(
                    title = "Tubarões Medidos",
                    fill = T,               # Se a infoBox deve ser preenchida
                    width = 10,             # Definindo a largura da infoBox
                    color = "light-blue",   # Definindo cor da infoBox
                    # Definindo Configurações do conteúdo da infoBox
                    value = tags$div(
                      style = "display: block; text-align: center;", 
                      h1(strong("28954"))
                    ),
                    icon = icon("fish"),
                    br()
                  )
                ),
                carouselItem(
                  infoBox(
                    title = "Entrevista de Desembarque",
                    fill = T,
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
                    fill = T,
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
                    fill = T,
                    width = 10,
                    color = "light-blue",
                    value = tags$div(
                      style = "display: block; text-align: center;",
                      h1(strong("92"))
                    ),
                    icon = icon("ship"),
                    br()
                  )
                )
              )
            )
            #   column(
            #     width = 6, # Definindo a largura da coluna
            #     infoBox(
            #       title = "Tubarões Medidos",
            #       fill = T,                  # Se a infoBox deve ser preenchida
            #       width = 10,                # Definindo a largura da infoBox
            #       color = "light-blue",      # Definindo cor da infoBox
            #       # Definindo Configurações do conteúdo da infoBox
            #       value = tags$div(
            #         style = "display: block; text-align: center;", 
            #         h1(strong("28954"))
            #       ),
            #       icon = icon("fish")
            #     )
            #   ),
            #   column(
            #     width = 6,
            #     infoBox(
            #       title = "Entrevista de desembarque",
            #       fill = T,
            #       width = 10,
            #       color = "light-blue", 
            #       value = tags$div(
            #         style = "display: block; text-align: center;", 
            #         h1(strong("731"))
            #       ),
            #       icon = icon("paste")
            #     )
            #   )
            # ),
            # fluidRow(
            #   column(
            #     width = 6,
            #     infoBox(
            #       title = "Cadernos de bordo",
            #       fill = T,
            #       width = 10,
            #       color = "light-blue",
            #       value = tags$div(
            #         style = "display: block; text-align: center;", 
            #         h1(strong("465"))
            #       ),
            #       icon = icon("book-open")
            #     )
            #   ),
            #   column(
            #     width = 6,
            #     infoBox(
            #       title = "Embarcações Monitoradas",
            #       fill = T,
            #       width = 10,
            #       color = "light-blue",
            #       value = tags$div(
            #         style = "display: block; text-align: center;",
            #         h1(strong("92"))
            #       ),
            #       icon = icon("ship")
            #     )
            #   )
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
              box(
                title = "Texto Leia-me",
                width = 12,
                headerBorder = F,
                # background = "gray",
                # POSTERIORMENTE É NECESSÁRIO ADAPTAR O TEXTO,
                # POIS HÁ ALGUNS GRÁFICOS QUE MUDARAM
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
                  do e-mail ",strong("proj.tubaraoazul.furg@gmail.com.")),
                # div(
                #   class = "boxsidebar",
                #   
                # )
                sidebar = boxSidebar(
                  id = "boxsidebar1",
                  icon = icon("circle-info"),
                  width = 40,
                  background = "#A6ACAFEF",
                  p("Contém informações detalhadas sobre as abas presentes no
                    dashboard."),
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
                # background = "light-blue",
                title = "Gráfico de Barras Empilhadas",
                collapsible = T,
                width = 12,
                solidHeader = T, # Se a Header é sólida
                status = "primary",
                div(
                  class = "graficos",
                  plotlyOutput("TubMesAno", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar2",
                  icon = icon("circle-info"),
                  background = "#A6ACAFEF",
                  width = 30,
                  p("Este gráfico de barras empilhadas apresenta a quantidade de
                    dados registrados por mês/ano, divididos por categoria de
                    pesca. Cada barra representa um mês/ano, com segmentos 
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
                # background = "red",
                title = "Gráfico de Rosca",
                collapsible = T,
                width = 12,
                solidHeader = T,
                status = "primary",
                div(
                  class = "graficos",
                  plotlyOutput("RoscaTubOutros", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar3",
                  icon = icon("circle-info"),
                  background = "#A6ACAFEF",
                  p("Este gráfico de rosca compara a presença de Cação Azul com
                    a categoria 'Outros', que representa a junção dos dados de 
                    todas as outras espécies de pesca. Ele mostra a proporção de 
                    Cação Azul em relação ao total, permitindo visualizar sua
                    participação comparada com as demais categorias.")
                )
              )
            ),
            column(
              width = 6,
              box(
                # background = "green",
                title = "Gráfico de Rosca",
                collapsible = T,
                width = 12,
                solidHeader = T,
                status = "primary",
                div(
                  class = "graficos",
                  plotlyOutput("TubMes", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar4",
                  icon = icon("circle-info"),
                  background = "#A6ACAFEF",
                  p("Este gráfico de rosca compara os dados obtidos em cada mês
                    para todas as espécies de pesca. Cada fatia representa um 
                    mês específico, mostrando a distribuição proporcional dos
                    dados ao longo do período analisado, permitindo visualizar
                    variações sazonais ou tendências.")
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              box(
                # background = "aqua",
                title = "Histograma",
                collapsible = T,
                width = 12,
                solidHeader = T,
                status = "primary",
                div(
                  class = "graficos",
                  plotlyOutput("historamaPeso", height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar5",
                  icon = icon("circle-info"),
                  background = "#A6ACAFEF",
                  width = 30,
                  p("Este histograma mostra a frequência relativa do peso de 
                    Cação Azul, destacando a distribuição dos dados de pesca em 
                    relação ao peso dessa espécie específica.")
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
                # background = "yellow",
                title = "Gráfico de Linhas",
                width = 12,
                solidHeader = T, 
                status = "primary",
                div(
                  class = "graficos",
                  # Saída do Gráfico Plotly de Captura 
                  plotlyOutput("graficoCaptura",height = "100%")
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
                # background = "blue",
                title = "Gráfico de Rosca",
                width = 12,
                solidHeader = T,
                status = "primary",
                div(
                  class = "graficos",
                  # Saída do Gráfico Plotly das Espécies
                  plotlyOutput("graficoEspecies",height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar7",
                  icon = icon("circle-info"),
                  background = "#A6ACAFEF",
                  p("Este gráfico de rosca ilustra a composição de espécies
                    presentes nos dados de pesca, indicando a porcentagem de 
                    cada espécie em relação ao total. Cada segmento do gráfico 
                    representa uma espécie, facilitando a visualização das 
                    proporções relativas entre elas.")
                )
              )
            ) 
          ),
          fluidRow(
            column(
              width = 12,
              box(
                # background = "navy",
                title = "Gráfico de Área",
                width = 12,
                solidHeader = T,
                status = "primary",
                div(
                  class = "graficos",
                  # Saída do Gráfico Plotly do Desembarque
                  apexchartOutput("graficoDesembarque",height = "100%")
                  # plotlyOutput("graficoDesembarque")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar8",
                  icon = icon("circle-info"),
                  width = 30,
                  background = "#A6ACAFEF",
                  p("Este gráfico de área apresenta a captura média em quilos 
                    por viagem, categorizada por tipo de peixe, para cada 
                    mês/ano no período analisado. As diferentes cores
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
                # background = "teal",
                width = 12,
                # height = "2rem",
                title = "Mapa de Capturas",
                solidHeader = T,
                status = "primary",
                div(
                  class = "mapa",
                  leafletOutput("MapaLeaflet",height = "100%")
                ),
                sidebar = boxSidebar(
                  id = "boxsidebar9",
                  icon = icon("circle-info"),
                  width = 30,
                  background = "#A6ACAFEF",
                  # h3("Descrição: "),
                  p("Este mapa de calor mostra a localização das capturas, onde
                    a cor dos círculos varia de amarelo a vermelho, indicando a
                    porcentagem de capturas em cada área. As áreas com uma 
                    porcentagem menor de capturas são representadas em tons mais
                    amarelos, enquanto áreas com uma porcentagem maior são 
                    exibidas em tons mais vermelhos. Isso permite visualizar 
                    facilmente as áreas com maior e menor concentração de
                    capturas.")
                )
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
  # footer = dashboardFooter(
  #   left = "Por Thiago Pacheco",
  #   right = "Itajaí, 2024"
  # ),
  
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
          min = min(dados_aux$ANO),  # Valor Mínimo do controle deslizante
          max = max(dados_aux$ANO),  # Valor Máximo do controle deslizante
          value = c(                    # Valor Inicial do controle deslizante
            min(dados_aux$ANO),
            max(dados_aux$ANO)
          ), 
          step = 1,        # Intervalo entre os valores do controle deslizante
          # Opções das animações
          animate = animationOptions(
            interval = 1700,
            playButton = icon("play"),
            pauseButton = icon("pause")
          )
        ),
        # # Entrada de grupo de caixas de seleção
        # checkboxGroupInput(
        #   inputId = "species",
        #   label = "Seletor de Espécies:",
        #   choices = c("Albacora bandolim","Albacora branca","Albacora laje",
        #               "Meca", "Outros","Tubarao Azul"),
        #   selected = c("Albacora bandolim","Albacora branca","Albacora laje",
        #                "Meca","Outros", "Tubarao Azul")
        # ),
        checkboxGroupInput(
          inputId = "species",
          label = "Seletor de Espécies:",
          choices = names(sort(table(dados_aux$CATEGORIA), decreasing = TRUE)),
          selected = "Cacao-azul"
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
  # senha_admin <- Sys.getenv("SENHA_ADMIN")
  
  # senha_admin <- Sys.getenv("SENHA_ACESSO")
  
  
  
  dados_completos <- dados_aux %>%
    complete(CATEGORIA, ANO, MES = 1:12, fill = list(VALOR = NA))
  
  dados_gerais <- dados_completos %>%
    mutate(KG = replace_na(KG, 0))
  
  nomes_meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                   "Julho", "Agosto", "Setembro", "Outubro", "Novembro",
                   "Dezembro")
  
  # Definir manualmente uma paleta de cores
  yellow_to_red_palette <- c("#FFFF00AA", "#FFCC00AA", "#FF9900AA",
                             "#FF6600AA", "#FF3300AA", "#FF0000AA")
  
  # # Definindo as Cores de cada Espécie
  # cores <- c("Albacora bandolim" = "purple","Albacora branca" = "red",
  #            "Albacora laje" = "green", "Meca" = "yellow",
  #            "Outros" = "orange", "Tubarao Azul" = "blue")
  
  # coresAux <- as.list(cores)
  
  # Criando um Valores Reativos que vão Iniciar Nulo
  conteudo_senha_adm <- reactiveVal(NULL)
  conteudo_entrar_adm <- reactiveVal(NULL)
  conteudo_tabela_adm <- reactiveVal(NULL)
  
  # Filtro de Dados ---------------------------------------------------------
  
  # Filtrando os Dados Gerais Reativamente
  dados_gerais_filtrados <- reactive({
    # Filtrando as Espécies 
    dados_filtrados <- subset(dados_gerais, CATEGORIA %in% input$species)
    # Filtrando o Intervalo de Anos
    dados_filtrados <- subset(
      dados_filtrados,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2])
    data.frame(dados_filtrados)
  })
  
  dados_aux_filtrados <- reactive({
    # Filtrando as Espécies 
    dados_filtrados <- subset(dados_aux, CATEGORIA %in% input$species)
    # Filtrando o Intervalo de Anos
    dados_filtrados <- subset(
      dados_filtrados,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2])
    data.frame(dados_filtrados)
  })
  
  # Filtrando Dados Para o Gráfico de Desembarque
  CapturasPorMesDesembarque <- reactive({
    dados_gerais_filtrados() %>%
      # Agrupa os Dados por Colunas Selecionadas
      group_by(CATEGORIA, ANO, MES) %>% 
      # Média das Toneladas de Captura de Cada Grupo
      summarise(Media_KG = mean(KG)) %>%
      # Arredondando a Média de Toneladas para Duas Casas Decimais
      mutate(Media_KG = round(Media_KG, 2)) %>%
      # Criação de Nome do Mês para Legenda
      mutate(mes_nome = nomes_meses[MES]) %>%
      #  Cria uma Variável Formato mes_ano
      mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES)))) %>%
      # Formata a coluna mes_ano para exibir apenas o mês e o ano
      mutate(mes_ano_formatado = format(as.Date(
        mes_ano, format = "%Y-%m"),"%b %Y")) %>% 
      select(-mes_ano)
  })
  
  # Filtrando Dados para o Gráfico de Captura
  CapturasMediasPorMes <- reactive({
    dados_gerais_filtrados() %>%
      group_by(CATEGORIA, ANO, MES) %>%
      summarise(Media_KG = mean(KG)) %>%
      # Agrupando as Colunas Selecionadas
      group_by(CATEGORIA, MES) %>% 
      # Média das Toneladas de Captura, de cada Mês, com Anos Agrupados
      summarise(Media_KG_por_Mes = mean(Media_KG)) %>%
      mutate(MediaKGMes = round(Media_KG_por_Mes, 2)) %>%
      select(-Media_KG_por_Mes) %>%
      mutate(mes_nome = nomes_meses[MES])
  })
  
  # Filtrando os Dados para o Histograma de Comprimento dos Tubarões Azul
  dadostub_filtrados <- reactive({
    dados_auxiliar <- subset(dados_gerais, CATEGORIA == "Cacao-azul")
    subset(
      dados_auxiliar,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
    )
  })
  
  dadostub_aux_filtrados <- reactive({
    dados_auxiliar <- subset(dados_aux, CATEGORIA == "Cacao-azul")
    subset(
      dados_auxiliar,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2]
    )
  })
  
  # Filtrando dados Para o Mapa
  db_filtrado <- reactive({
    dados_filtrados <- subset(dados_aux, CATEGORIA %in% input$species)
    dados_filtrados <- subset(
      dados_filtrados,
      ANO >= input$intervalo_anos[1] & ANO <= input$intervalo_anos[2])
    tab01 <- dados_filtrados %>%
      group_by(LON, LAT) %>%
      summarise(prod = sum(KG)) %>%
      ungroup()
    list(dados = dados_filtrados, tab01 = tab01)
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
  
  # Tentativa de gráfico de porcentagem de captura de Cacao-azul por Mes
  output$TubMesAno <- renderPlotly({
    
    tubAzulMes <- dadostub_filtrados() %>% 
      group_by(ANO, MES,CATEGORIA) %>% 
      summarise(Quantidade = n()) %>%
      mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES)))) %>%
      mutate(mes_ano_formatado = format(as.Date(
        mes_ano, format = "%Y-%m"),"%b %Y")) %>% 
      select(-mes_ano)
    
    # Converter mes_ano_formatado em fator ordenado
    tubAzulMes$mes_ano_formatado <- factor(
      tubAzulMes$mes_ano_formatado, 
      levels = unique(tubAzulMes$mes_ano_formatado),
      ordered = TRUE)
    
    dadostub_filtrados
    tubAzulMesAnoCompleto <- dados_aux_filtrados() %>%
      mutate(CATEGORIA=if_else(CATEGORIA!="Cacao-azul","Outros",CATEGORIA)) %>%
      group_by(CATEGORIA, ANO, MES) %>%
      summarise(Quantidade = n()) %>%
      ungroup() %>%
      complete(CATEGORIA, ANO, MES = 1:12, fill = list(Quantidade = 0)) %>%
      mutate(mes_ano_formatado = make_date(ANO, MES)) %>%
      mutate(mes_ano = as.yearmon(paste0(ANO, "-", sprintf("%02d", MES))))
    
    tubAzulMesAnoCompleto$mes_ano_formatado <- format(
      tubAzulMesAnoCompleto$mes_ano_formatado, "%Y-%m")
    
    plot_ly(
      data = tubAzulMesAnoCompleto,
      x = ~mes_ano_formatado,
      y = ~Quantidade,
      color = ~CATEGORIA,
      type = "bar",
      hoverinfo = "text",
      text = ~paste(
        "Data: ",mes_ano,"<br>",
        "Categoria: ", CATEGORIA, "<br>",
        "Quantidade: ", Quantidade
      )
    ) %>%
      layout(
        title = "Dados Registrados por Mês, Ano e Categoria",
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = "Quantidade"
        ),
        barmode = "stack",
        showlegend = F
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$RoscaTubOutros <- renderPlotly({
    tubazul <- dados_aux_filtrados() %>%
      mutate(
        CATEGORIA = ifelse(
          CATEGORIA == "Cacao-azul","Cacao-azul","Outros"
        )
      ) %>%
      count(CATEGORIA)
    
    plot_ly(
      data = tubazul,
      labels = ~CATEGORIA,
      values = ~n,
      type = "pie",
      hole = 0.6,
      textinfo = "label",
      hoverinfo = "text+percent",
      text = ~paste("Quantidade: ", n, "<br>"),
      marker = list(colors = c("Cacao-azul" = "#1f77b4", "Outros" = "#ff7f0e"))
    ) %>%
      layout(
        title = "Comparação de Dados da Cação Azul para o Resto",
        showlegend = FALSE
      )
  })
  
  output$TubMes <- renderPlotly({
    
    tubAzulMesCompleto <- dadostub_aux_filtrados() %>%
      group_by(CATEGORIA, MES) %>%
      summarise(Quantidade = n()) %>%
      ungroup() %>%
      complete(CATEGORIA, MES = 1:12, fill = list(Quantidade = 0)) %>% 
      mutate(mes_nome = nomes_meses[MES])
    
    plot_ly(
      data = tubAzulMesCompleto,
      labels = ~mes_nome,
      parents = ~CATEGORIA,
      values = ~Quantidade,
      type = "sunburst",
      branchvalues = "total", 
      hoverinfo = "percent entry+value",
      textinfo = "label"
    ) %>%
      layout(
        title = "Dados Registrados de Cação Azul por Mês",
        showlegend = FALSE
      )
  })
  
  # Histograma da Distribuição de Peso em Kg da Cação Azul
  output$historamaPeso <- renderPlotly({
    start_value <- floor(min(dadostub_aux_filtrados()$KG) / 25) * 25
    
    plot_ly(
      data = dadostub_aux_filtrados(),
      x = ~KG,
      histnorm = "percent",
      xbins = list(start = start_value ,size = 25, end = 5000),
      marker = list(
        color = "#3C8DBC",
        line = list(
          color = "black",
          width = 1
        )
      ),
      hoverinfo = "x+y"
    ) %>% 
      layout(
        title = "Frequência Relativa do Peso de Cações Azul",
        yaxis = list(
          title = "Frequência Relativa (%)",
          tickwidth = 2,
          showgrid = T,
          titlefont = list(size = 18)
        ),
        xaxis = list(
          title = "Peso Total (Kg)",
          titlefont = list(size = 18)
        )
      ) 
  })
  
  # Desembarques ------------------------------------------------------------
  
  # Renderização do Gráfico Plotly da Média Mensal de Capturas (mes)
  output$graficoCaptura <- renderPlotly({
    plot_ly(
      data = CapturasMediasPorMes(),
      x = ~MES,
      y = ~MediaKGMes,
      type = 'scatter',
      mode = 'lines+markers',
      # color = ~CATEGORIA,
      # colors = cores,
      marker = list(size = 10), # Tamanho do Marcador
      hoverinfo = "text",
      text = ~paste(
        "Espécie: ", CATEGORIA, "<br>",
        "Mês: ", mes_nome, "<br>",
        "Média de KG: ", MediaKGMes, "<br>"
      )
    ) %>%
      layout(
        title = "Captura Média por Viagem",
        xaxis = list(
          title = "Mês"
        ),
        yaxis = list(
          title = "Captura Média (KG) por Viagem"
        ), showlegend = FALSE
      )
  })
  
  output$graficoDesembarque <- renderApexchart({
    apex(
      data = CapturasPorMesDesembarque(),
      type = "area",
      mapping = aes(
        x = mes_ano_formatado,
        y = Media_KG,
        fill = CATEGORIA
      ),
      showlegend = F
    ) %>% 
      ax_yaxis(
        title = list(
          text = "Captura Média (KG) por Viagem"
        )
      )
  })
  
  ##Renderização do Gráfico Plotly da Média Mensal de Capturas de Todo Período
  # # (mes_ano)
  # output$graficoDesembarque <- renderPlotly({
  #   plot_ly(
  #     data = CapturasPorMesDesembarque(),
  #     x = ~mes_ano,
  #     y = ~Media_Toneladas,
  #     type = 'scatter',
  #     mode = 'lines+markers',
  #     color = ~CATEGORIA,
  #     colors = cores,
  #     marker = list(size = 5),
  #     hoverinfo = "text",
  #     text = ~paste(
  #       "Espécie: ", CATEGORIA, "<br>",
  #       "Mês: ", Mes_Nome, "<br>",
  #       # "ANO: ", ANO, "<br>",
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
    tiposEspecies <- dados_aux_filtrados() %>%
      group_by(CATEGORIA) %>% # Agrupando os Dados por Espécie
      summarise(
        n = n(), # Quantidade de Capturas por Espécie
        KG_Totais = sum(KG) # Somando as Toneladas para cada Grupo
      ) %>%
      mutate(porc = n/sum(n)#,
             # # Para que cada Espécie Mantenha a Mesma Cor
             # cores = case_when(
             #   CATEGORIA == "Albacora bandolim" ~ "purple",
             #   CATEGORIA == "Albacora branca" ~ "red",
             #   CATEGORIA == "Albacora laje" ~ "green",
             #   CATEGORIA == "Meca" ~ "yellow",
             #   CATEGORIA == "Outros" ~ "orange",
             #   CATEGORIA == "Tubarao Azul" ~ "blue",
             #   TRUE ~ NA_character_  # Caso Não haja Correspondência
             #   )
      )
    
    plot_ly(
      data = tiposEspecies,
      labels = ~CATEGORIA,
      values = ~n,
      type = "pie",
      hole = 0.6,
      textinfo = 'label', # Cada Fatia terá o Nome de sua Espécie nela
      hoverinfo = "text",
      text = ~paste(
        "Quantidade: ", n, "<br>",
        "Porcentagem: ", percent(porc, accuracy = 0.1), "<br>"
      )#,
      # marker = list(colors = tiposEspecies$cores)
    ) %>%
      layout(
        title = "Composição de Espécies",
        showlegend = FALSE
      )
  })
  
  # Distribuição Espacial das Capturas --------------------------------------
  
  output$MapaLeaflet <- renderLeaflet({
    dados_filtrados <- db_filtrado()$dados
    tab01 <- db_filtrado()$tab01
    
    range_values <- range(tab01$prod)
    
    # Calcular os intervalos
    intervals <- seq(range_values[1], range_values[2],length.out = 11)
    
    # Criar a paleta de cores com base nos intervalos
    pal <- colorQuantile(
      palette = yellow_to_red_palette,
      domain = tab01$prod,
      probs = seq(0, 1, 0.1)
    )
    
    
    # ######@> Color palette...
    # pal <- colorQuantile(
    #   palette = "viridis",
    #   domain = tab01$prod,
    #   probs = seq(0, 1, 0.1))
    
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
        lng = -40, lat = -29, zoom = 4
      ) %>%
      addCircleMarkers(
        group = tab01$prod,
        radius = 10,
        lng = tab01$LON,
        lat = tab01$LAT,
        stroke = FALSE,
        color = pal(tab01$prod),
        fillOpacity = 0.7,
        label = paste0("Captura: ", round(tab01$prod, 0), " kg")
      ) %>%
      addLegend(
        pal = pal, values = tab01$prod, group = tab01$prod,
        position = "bottomright", title = "Percentual da Captura"
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("Dark Map", "Light Map"),
        options =
          layersControlOptions(collapsed = FALSE)
      ) %>%
      addMiniMap(
        position = "bottomleft"
      ) %>%
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
    # print("0")
    if (senha_correta) {
      conteudo_tabela_adm({
        # Saída da Data Table, que Possuí a Tabela com os Dados
        DTOutput("tabela_tub")
      })
      # print("1")
      # Valores Reativos Recebem o Valor de Nulo
      conteudo_senha_adm(NULL)
      conteudo_entrar_adm(NULL)
      # Renderizando a DataTable com os Dados Filtrados
      output$tabela_tub <- renderDT({
        # print("2")
        if (!is.null(input$entrar) && input$entrar > 0) {
          # print("3")
          if (senha_correta) {
            # print("4")
            dados_aux_filtrados()
          }
        }
        # Opções da Data Table
      },options = list(paging = T, searching = FALSE))
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
  
  # ControlBar --------------------------------------------------------------
  
  # Verificação do Botão Reset, ao ser Pressionado Atualizará os Controles
  observeEvent(input$reset, {
    # Atualização no Controle Deslizante 
    updateSliderInput(
      session,
      inputId = "intervalo_anos",
      value = c(min(dados_aux$ANO), max(dados_aux$ANO))
    )
    
    # Atualização no Grupo de Caixas de Seleção
    updateCheckboxGroupInput(
      session, 
      inputId = "species",
      selected = unique(dados_aux$CATEGORIA)
    )
  })
}

shinyApp(ui,server)
