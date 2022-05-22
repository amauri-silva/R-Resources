# Clean all thigns on memory
rm(list = ls())
#Encoding(df) = "UTF-8"

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)


clientes = read.csv("E:/r-resource/Bweb/base_clientes.csv", sep = ",", stringsAsFactors = TRUE)
summary(clientes)
str(clientes)
View(clientes)


#Missing Values ----------------------------------------------------------------
sum(is.na(clientes))

# Total de linhas contendo valor vazio
sum(clientes == "")

# Matriz de correlação das veriaveis -------------------------------------------
corr_matrix  = select(clientes, c(-id_cliente, -persona))
View(df_corr)
str(df_corr)
corr_matrix = cor(df_corr[])

corr_matrix = round(corr_matrix, 3)
corr_matrix


# Selecionando 100 Clientes Aleatoriamente -------------------------------------
clientes_100 = clientes[sample(nrow(clientes), 100),]
View(clientes_100)

proba <- ggplot(clientes_100, aes(x=persona, fill=persona)) +
  geom_bar() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
proba




# ==============================================================================
# Analise univariada AMAURI-----------------------------------------------------

proba <- ggplot(clientes, aes(x=persona, fill=persona)) +
  geom_bar() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))+
  xlab(size_sum())
proba

engaj <- ggplot(clientes, aes(x=engajamento, fill=persona, )) +
  geom_bar() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
engaj

# Plota os dois grádicos
grid.arrange(engaj , proba)



# Analise BIVARIADA AMAURI------------------------------------------------------

enga_box <-ggplot(clientes, aes(x=persona, y=engajamento, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
enga_box

enga_bar <- ggplot(clientes, aes(x=persona, y=engajamento, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
enga_bar

# Plota os dois grádicos
grid.arrange(enga_box , enga_bar)



compra_box <- ggplot(clientes, aes(x=persona, y=compras_60_dias, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14))
compra_box

compra_bar <- ggplot(clientes, aes(x=persona, y=compras_60_dias, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14))
compra_bar

# Plota os dois grádicos
grid.arrange(compra_box , compra_bar)



proba_box <- ggplot(clientes, aes(x=persona, y=proba, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14))
proba_box

proba_bar <- ggplot(clientes, aes(x=persona, y=proba, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14))
proba_bar

# Plota os dois grádicos
grid.arrange(proba_box , proba_bar)


# ==============================================================================
# Analise univariada CAIO ------------------------------------------------------

#Compras_60_dias

ggplot(data = clientes, aes(x = compras_60_dias)) + 
  geom_histogram(binwidth = 10, color = "white",fill = "dodgerblue", position = "dodge") +
  xlab("Compras") +
  ylab("Frequencia") +
  scale_x_continuous(limits = c(0,250), breaks = seq(0,250, by = 10)) +
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600, by = 100))
  ggtitle("Distribuição Compras nos últimos 60 dias")


#Probabilidade de Lift
  
  ggplot(data = clientes, aes(x = proba)) + 
    geom_histogram(color = "white",fill = "dodgerblue", position = "dodge") +
    xlab("Probabilidade") +
    ylab("Frequencia")


# Analise BIVARIADA CAIO -------------------------------------------------------

#Engajamenbto X Compras últimos 60 dias

ggplot(data = clientes, aes(x = engajamento, y = compras_60_dias)) + 
  geom_point(position = "jitter", color = "dodgerblue") +
  geom_smooth(method = lm, se = F)+
  xlab("Engajamento") +
  ylab("Compras últimos 60 dias") +
  theme(legend.position = "right")


#Engajamenbto X Probabilidade de lift

ggplot(data = clientes, aes(x = engajamento, y = proba)) + 
  geom_point(position = "jitter", color = "dodgerblue") +
  geom_smooth(method = lm, se = F)+
  xlab("Engajamento") +
  ylab("Probabilidade de Lift") +
  theme(legend.position = "right")


#Compras últimos 60 dias X Probabilidade de lift

ggplot(data = clientes, aes(x = compras_60_dias, y = proba)) + 
  +   geom_point(position = "jitter", color = "dodgerblue") +
  +   geom_smooth(method = lm, se = F)+
  +   xlab("Compras últimos 60 dias") +
  +   ylab("Probabilidade de Lift") +
  +   theme(legend.position = "right")



