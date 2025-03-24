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

accesstoelectricity2023 <- WDI(country = 'all',
                   indicator = 'EG.ELC.ACCS.ZS',
                   start = 2023, end = 2023)