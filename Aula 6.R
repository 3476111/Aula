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
