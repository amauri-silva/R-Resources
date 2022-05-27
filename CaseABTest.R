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
ab_test     <- tibble(read.csv('E:/r-resource/ABTest.csv', stringsAsFactors = TRUE))
next_camp   <- tibble(read.csv('E:/r-resource/NextCampaign.csv', stringsAsFactors = TRUE))



View(ab_test)
View(next_camp)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# test_coupon: Se o cliente recebeu ou não o cupom
#   0 = controle
#   1 = tratamento
# --------------------------------------------------------------------------------------------------
# 1 - A CAMPANHA DE CUPONS FUNCIONOU?

# Criando uma nova variavel com adicionando o nome do grupo
ab_test$grupo = ifelse(ab_test$test_coupon == 0, "controle", "tratamento") 


r_after <- ggplot(ab_test, aes(x=grupo, y=revenue_after, fill=grupo)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
r_after

t_after <- ggplot(ab_test, aes(x=grupo, y=trans_after, fill=grupo)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
t_after



# 2 - A EMPRESA DEVE CONTINUAR ENVIANDO CUPONS NO FUTURO?

# 3 - QUAIS NOVOS SEGMENTOS DE CLIENTES DEVERIAM RECEBER O CUPOM? QUAL A PORCENTAGEM DE CLIENTES QUE
#   A ARTEA DEVERIA IMPACTAR COM CAMPANHAS DESSA NATUREZA?




# 4 - MESMO COM O VALOR COM DESCONTO, A EMPRESA CONSEGUIU AUMENTAR AS VENDAS COM ESSA CAMPANHA? EM QUANTO?

# O COUPON oference 20% na compra

# Somar o total de compras descontando 20% do total
trans_after	
total_faturado = sum(ab_test$revenue_after)

# $38.296.62
total_faturado

desconto_20 = total_faturado * 20 / 100
total_liq = total_faturado - desconto_20
total_liq


lucro_anterior = sum(ab_test$spent_last_purchase)
lucro_anterior

names = c("total_liq", "lucro_anterior")
revenue = c(total_liq, lucro_anterior)

df = data.frame(names, revenue)
View(df)

a <- ggplot(df, aes(x=names, y=revenue, fill=names)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14), axis.text.x = element_text(face="bold"))
a

lucro_anterior_vs_pos_coupon = total_liq - lucro_anterior
lucro_anterior_vs_pos_coupon








