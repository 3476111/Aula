#API (forma de acesso)
#DADOS DO BANCO MUNDIAL (WORL BANK)
#WORLD DEVELOPMENT INDICATORS (BASE DE DADOS)

#NA AULA PASSADA, ACESSAMOS OS DADOS DO PIB
#PRODUTO INTERNO BRUTO

install.packages("WDI")
library("WDI")

options(scipen = 999)
basepib <- WDI(country = 'all',
               indicator = 'NY.GDP.PCAP.CD')

basepib2023 <- WDI(country = 'all',
                   indicator = 'NY.GDP.PCAP.CD',
                   start = 2023, end = 2023)

basepibbr <- WDI(country = 'BR',
                 indicator = 'NY.GDP.PCAP.CD')

#Access to electricity (% of population)(EG.ELC.ACCS.ZS)

accesstoelectricity <- WDI(country = 'all',
                           indicator = 'EG.ELC.ACCS.ZS')

accesstoelectricitybr <-  WDI(country = 'BR',
                              indicator = 'EG.ELC.ACCS.ZS')

accesstoelectricity2010 <- WDI(country = 'all',
                               indicator = 'EG.ELC.ACCS.ZS',
                               start = 2010, end = 2010)

#FAZER GRAFICOS
#ggplot2 (faz parte do pacote tidyverse)

#install.packages ('tidyverse')
library(tidyverse)

grafpainel <- ggplot(basepib,
                     mapping = aes(y = NY.GDP.PCAP.CD,
                                   x = year)) +
  geom_line()

print(grafpainel)


grafcorte <- ggplot(basepib2023,
                    mapping = aes(y = NY.GDP.PCAP.CD,
                                  x = year)) +
  geom_point()

print(grafcorte)

grafserie <- ggplot(basepibbr,
                    mapping = aes(y = NY.GDP.PCAP.CD,
                                  x = year)) +
  geom_point()

print(grafserie)

#


grafelectricity <- ggplot(accesstoelectricity,
                          mapping = aes(y = EG.ELC.ACCS.ZS,
                                        x = year)) +
  geom_point()

print(grafelectricity)


grafelectricitybr <- ggplot(accesstoelectricitybr,
                            mapping = aes(y = EG.ELC.ACCS.ZS,
                                          x = year)) +
  geom_point()

print(grafelectricitybr)


grafelectricity2010 <- ggplot(accesstoelectricity2010,
                              mapping = aes(y = EG.ELC.ACCS.ZS,
                                            x = year)) +
  geom_point()

print(grafelectricity2010)


library(ggplot2)

# Supondo que 'accesstoelectricity' seja seu data frame
# Primeiro, vamos garantir que o nome da coluna com o nome do país seja correto
# Aqui estou assumindo que a coluna é chamada 'country'. Troque o nome conforme necessário.

accesstoelectricity$color <- ifelse(accesstoelectricity$country == "Brazil", "green", "red")

# Gráfico
grafelectricity <- ggplot(accesstoelectricity, aes(x = year, y = EG.ELC.ACCS.ZS)) +
  geom_point(aes(color = color), size = 3) +  # Cor para os pontos, tamanho ajustável
  scale_color_manual(values = c("green" = "green", "red" = "red")) +  # Definir cores manualmente
  ggtitle("Access to Electricity") +  # Título do gráfico
  labs(color = "Country") +  # Legenda para a cor
  theme_minimal() +  # Tema minimalista
  theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título

# Exibindo o gráfico
print(grafelectricity)



library(ggplot2)

# Gráfico para o Brasil
grafelectricitybr <- ggplot(accesstoelectricitybr, 
                            mapping = aes(y = EG.ELC.ACCS.ZS, 
                                          x = year)) +
  geom_point(color = "green", size = 3) +  # Usar cor verde para o Brasil
  ggtitle("Access to Electricity Brazil") +  # Título do gráfico
  theme_minimal() +  # Tema minimalista
  theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título

# Exibindo o gráfico
print(grafelectricitybr)




library(ggplot2)

# Gráfico para o ano de 2010
grafelectricity2010 <- ggplot(accesstoelectricity2010, 
                              mapping = aes(y = EG.ELC.ACCS.ZS, 
                                            x = year)) +
  geom_point(color = "green", size = 3) +  # Usar cor verde para os pontos
  ggtitle("Access to Electricity in 2010") +  # Título para o gráfico de 2010
  theme_minimal() +  # Tema minimalista
  theme(plot.title = element_text(hjust = 0.5))  # Centralizar o título

# Exibindo o gráfico
print(grafelectricity2010)
