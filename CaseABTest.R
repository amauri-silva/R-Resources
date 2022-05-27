#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
##--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

# Clean all environment's objects
rm (list = ls())
# Turn off scientific notation
options(scipen = 999)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)


#--------------------------------------------------------------------------------#
# Carregando os datasets
ab_test     <- tibble(read.csv('E:/r-resource/NextCampaign.csv', stringsAsFactors = TRUE))
next_camp   <- tibble(read.csv('E:/r-resource/ABTest.csv', stringsAsFactors = TRUE))

View(ab_test)
View(next_camp)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 1 - A CAMPANHA DE CUPONS FUNCIONOU?

# 2 - A EMPRESA DEVE CONTINUAR ENVIANDO CUPONS NO FUTURO?

# 3 - QUAIS NOVOS SEGMENTOS DE CLIENTES DEVERIAM RECEBER O CUPOM? QUAL A PORCENTAGEM DE CLIENTES QUE
#   A ARTEA DEVERIA IMPACTAR COM CAMPANHAS DESSA NATUREZA?

# 4 - MESMO COM O VALOR COM DESCONTO, A EMPRESA CONSEGUIU AUMENTAR AS VENDAS COM ESSA CAMPANHA? EM QUANTO?