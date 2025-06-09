library(gganimate)

install.packages("gganimate")
library(gganimate)
library(rnaturalearth)
library(tidyverse)
library(WDI) 

Data_info <- WDI_data

library(rnaturalearth)
install.packages("rnaturalearth")

library(tidyverse)
install.packages(c(
  "WDI", "tidyverse", "sf", "rnaturalearth", "rnaturalearthdata",
  "gganimate", "gifski", "transformr"
))
install.packages("transformr")



 # Adicionado para carregar o pacote WDI

# Baixando os dados do World Bank para o indicador "Self-employed" de 1990 a 2019
indicator <- c("Self-employed" = 'SL.EMP.SELF.ZS')
datWM7 <- WDI(indicator, country = "all", start = 1990, end = 2019)

# Pegando o nome do indicador
name_self_employed <- "Self-employed"

# Pegando a fonte dos dados
source_self_employed <- "World Bank"

# Pegando os países com o pacote rnaturalearth
countries_sf <- ne_countries(returnclass = "sf")

# Unindo os dados do World Bank com os dados geoespaciais
datWM7_sf <- countries_sf %>%
  left_join(datWM7, by = c("iso_a2" = "iso2c")) %>%
  filter(iso_a2 != "ATA")  # Remover a Antártida

# Criando o gráfico animado
ggplot(datWM7_sf, aes(fill = `Self-employed`)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::percent_format(scale = 1)) +
  theme(legend.position = "bottom") +
  labs(
    title = paste0(name_self_employed, " in {closest_state}"),
    fill = NULL,
    caption = paste0("Source: ", source_self_employed)
  ) + 
  transition_states(year, transition_length = 3, state_length = 1)  # Criando a animação pela variável 'year'

