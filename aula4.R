install.packages("WDI")
library("WDI")
GDP per capita (current US$)(NY.GDP.PCAP.CD)

#sempre procurem as vignettes
#paginas com orientações dos pacotes

#baixar os dados do pib 
#tudo que é produzido no pais/ESTADO/MUNICIPIO
#em um determinado periodo
             





COD-GDP <- WDIsearch("gdp")







options(scipen = 999)
basepib <- WDI(country = 'all',
               indicator = 'NY.GDP.PCAP.CD')

basepib2023 <- WDI(country = 'all',
               indicator = 'NY.GDP.PCAP.CD',
               start = 2023, end = 2023)
