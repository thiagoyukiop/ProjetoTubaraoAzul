library(dplyr)
library(rnaturalearth)
library(plotly)
library(sf)
library(maps)
library(oce)
library(sp)

verificar_coordenada_mar <- function(latitude, longitude) {
  world_map <- map("world", fill = TRUE, plot = FALSE)
  is_on_land <- point.in.polygon(longitude, latitude, world_map$x, world_map$y)
  if (!is_on_land) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Função para gerar coordenadas aleatórias no mar
gerar_coordenadas_aleatorias <- function(n) {
  coordenadas_mar <- data.frame(Latitude = numeric(), Longitude = numeric())
  contador <- 0
  while (contador < n) {
    Latitude <- runif(1, -90, 90)  # Gerar latitude aleatória entre -90 e 90
    Longitude <- runif(1, -180, 180)  # Gerar longitude aleatória entre -180 e 180
    if (verificar_coordenada_mar(Latitude, Longitude)) {
      # Se a coordenada estiver no mar, adicioná-la ao data frame e incrementar o contador
      coordenadas_mar <- rbind(coordenadas_mar, data.frame(Latitude, Longitude))
      contador <- contador + 1
    }
  }
  return(coordenadas_mar)
}


set.seed(42)  # Definir a semente para reproduzibilidade
coordenadas_mar <- gerar_coordenadas_aleatorias(10000)

# coordenadas_mar10k <- coordenadas_mar[rep(seq_len(nrow(coordenadas_mar)),length.out=10000),]

# Defina os valores possíveis para cada coluna
especies <- c("Albacora bandolim", "Albacora branca", "Albacora laje", "Meca", "Outros", "Tubarao Azul")
toneladas <- seq(0.1, 8, by = 0.01)  # Sequência de 0.1 até 8 com incremento de 0.01
sexos <- c("M", "F")

# Gere os dados aleatórios
dados <- data.frame(
  Especie = sample(especies, 10000, replace = TRUE),
  Toneladas = round(runif(10000, min = 0.1, max = 8), 2),
  Tamanho = round(runif(10000, min = 115,max = 190),2),
  Sexo = sample(sexos, 10000, replace = TRUE),
  Dia = sample(1:28, 10000, replace = TRUE),
  Mes = sample(1:12, 10000, replace = TRUE),
  Ano = sample(2018:2023, 10000, replace = TRUE)
)

df_completo <- cbind(dados, coordenadas_mar)

# # Salve os dados em um arquivo CSV
# write.csv(df_completo, "dados_brutos/dados_PTA.csv", row.names = FALSE)


# Carregue os dados
# dados <- read.csv("dados_brutos/dados_captura_coordenadas.csv")

# Crie o gráfico de mapa de calor
plot_ly(
  data = dados_gerais,
  type = 'densitymapbox',
  lat = ~Latitude,
  lon = ~Longitude,
  z = ~Toneladas,
  radius = 1000
) %>%
  layout(
    title = "Mapa de Calor das Capturas",
    mapbox = list(
      style = "open-street-map",  # Escolha um estilo de mapa adequado
      zoom = 1
    )
  )
