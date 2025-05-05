#API (forma de acesso)
#DADOS DO BANCO MUNDIAL (WORL BANK)
#WORLD DEVELOPMENT INDICATORS (BASE DE DADOS)

#NA AULA PASSADA, ACESSAMOS OS DADOS DO PIB
#PRODUTO INTERNO BRUTO

#install.packages("WDI")
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

#Novos: 

library(ggplot2)

# Define cor: Brasil em verde, outros em cinza claro
accesstoelectricity$color <- ifelse(accesstoelectricity$country == "Brazil", "Brazil", "Other")

# Gráfico
grafelectricity <- ggplot(accesstoelectricity, aes(x = year, y = EG.ELC.ACCS.ZS, group = country)) +
  geom_line(aes(color = color), size = 1) +  # Linhas no lugar dos pontos
  scale_color_manual(
    values = c("Brazil" = "green", "Other" = "lightgray"), 
    breaks = "Brazil"  # Apenas "Brazil" na legenda
  ) +
  ggtitle("Access to Electricity Over Time") +
  labs(x = "Year", y = "Access to Electricity (% of Population)", color = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

# Exibe o gráfico
print(grafelectricity)


#NOVO: 

library(ggplot2)
library(dplyr)

# Define as cores: Brasil em verde escuro, outros em cinza claro
accesstoelectricity$color <- ifelse(accesstoelectricity$country == "Brazil", "Brazil", "Other")

# Filtra o primeiro ano com dados para evitar espaço em branco no início
# Filtra pelo Brasil, mas você pode ajustar para o conjunto completo se preferir
first_year <- min(accesstoelectricity$year[!is.na(accesstoelectricity$EG.ELC.ACCS.ZS)])

# Filtra os dados a partir desse ano
filtered_data <- accesstoelectricity %>% filter(year >= first_year)

# Gráfico
grafelectricity <- ggplot(filtered_data, aes(x = year, y = EG.ELC.ACCS.ZS, group = country)) +
  # Linhas dos outros países (primeiro, para ficarem no fundo)
  geom_line(data = subset(filtered_data, color == "Other"), 
            aes(group = country), 
            color = "#D3D3D3", size = 0.8) +
  # Linha do Brasil por cima
  geom_line(data = subset(filtered_data, color == "Brazil"), 
            aes(group = country), 
            color = "darkgreen", size = 1.2) +
  ggtitle("Access to Electricity Over Time") +
  labs(x = "Year", y = "Access to Electricity (% of Population)", color = "Country") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"  # Remove legenda se for só o Brasil destacado
  )

# Exibe o gráfico
print(grafelectricity)

#ALT:

library(ggplot2)
library(dplyr)

# Define as cores: Brasil em verde escuro, outros em cinza claro
accesstoelectricity$color <- ifelse(accesstoelectricity$country == "Brazil", "Brazil", "Other")

# Filtra o primeiro ano com dados
first_year <- min(accesstoelectricity$year[!is.na(accesstoelectricity$EG.ELC.ACCS.ZS)])
filtered_data <- accesstoelectricity %>% filter(year >= first_year)

# Gráfico com fundo preto
grafelectricity <- ggplot(filtered_data, aes(x = year, y = EG.ELC.ACCS.ZS, group = country)) +
  # Linhas dos outros países
  geom_line(data = subset(filtered_data, color == "Other"),
            aes(group = country),
            color = "#808080", size = 0.8) +  # cinza mais escuro para fundo preto
  # Linha do Brasil
  geom_line(data = subset(filtered_data, color == "Brazil"),
            aes(group = country),
            color = "darkgreen", size = 1.2) +
  ggtitle("Access to Electricity Over Time") +
  labs(x = "Year", y = "Access to Electricity (% of Population)") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_line(color = "#333333"),
    plot.title = element_text(hjust = 0.5, color = "white", size = 14),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.position = "none"
  )

# Exibe o gráfico
print(grafelectricity)


#ALT 2.0:

library(ggplot2)

# Gráfico do Brasil com linha e fundo preto
grafelectricitybr <- ggplot(accesstoelectricitybr, 
                            aes(x = year, y = EG.ELC.ACCS.ZS)) +
  geom_line(color = "darkgreen", size = 1.2) +  # Linha verde escura
  ggtitle("Access to Electricity - Brazil") +
  labs(x = "Year", y = "Access to Electricity (% of Population)") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_line(color = "#333333"),
    plot.title = element_text(hjust = 0.5, color = "white", size = 14),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# Exibe o gráfico
print(grafelectricitybr)


#ALT 3:

library(ggplot2)

# Gráfico com linha e fundo preto para os dados de 2010
grafelectricity2010 <- ggplot(accesstoelectricity2010, 
                              aes(x = year, y = EG.ELC.ACCS.ZS)) +
  geom_line(color = "darkgreen", size = 1.2) +  # Linha verde escura
  ggtitle("Access to Electricity in 2010") +
  labs(x = "Year", y = "Access to Electricity (% of Population)") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_line(color = "#333333"),
    plot.title = element_text(hjust = 0.5, color = "white", size = 14),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# Exibindo o gráfico
print(grafelectricity2010)

#CURIOSIDADE: 

# Assumindo que accesstoelectricity2010 contém vários países
ggplot(accesstoelectricity2010, aes(x = reorder(country, -EG.ELC.ACCS.ZS), 
                                    y = EG.ELC.ACCS.ZS, fill = country == "Brazil")) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "steelblue")) +
  labs(title = "Access to Electricity by Country (2010)",
       x = "Country", y = "Access to Electricity (%)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
    axis.text.y = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(hjust = 0.5, color = "white")
  )

#




