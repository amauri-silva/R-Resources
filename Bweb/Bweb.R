# Clean all thigns on memory
rm(list = ls())
#Encoding(df) = "UTF-8"

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
theme_set(theme_pubr())
library(gridExtra)
library(plyr)

clientes = read.csv("E:/r-resource/Bweb/base_clientes.csv", sep = ",", stringsAsFactors = TRUE)
View(clientes)
summary(clientes)
str(clientes)

# Quantidade de clientes por amostra -------------------------------------------
table(clientes$persona)


#Missing Values ----------------------------------------------------------------
sum(is.na(clientes))

# Total de linhas contendo valor vazio
sum(clientes == "")

# Matriz de correla??o das veriaveis -------------------------------------------
corr_matrix  = select(clientes, c(-id_cliente, -persona))
View(df_corr)
str(df_corr)
corr_matrix = cor(df_corr[])

corr_matrix = round(corr_matrix, 3)
corr_matrix


# ==============================================================================
# Analise univariada AMAURI-----------------------------------------------------

proba <- ggplot(clientes, aes(x=persona, fill=persona)) +
  geom_bar() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))+
  xlab(size_sum())
proba


engaj <- ggplot(clientes, aes(x=engajamento)) +
  geom_histogram(color = "white",fill = "dodgerblue") +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
engaj

# Plota os dois gr?dicos
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

# Plota os dois gr?dicos
grid.arrange(enga_box , enga_bar)



compra_box <- ggplot(clientes, aes(x=persona, y=compras_60_dias, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14))
compra_box

compra_bar <- ggplot(clientes, aes(x=persona, y=compras_60_dias, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14))
compra_bar

# Plota os dois gr?dicos
grid.arrange(compra_box , compra_bar)



proba_box <- ggplot(clientes, aes(x=persona, y=proba, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14))
proba_box

proba_bar <- ggplot(clientes, aes(x=persona, y=proba, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14))
proba_bar

# Plota os dois gr?dicos
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
ggtitle("Distribui??o Compras nos ?ltimos 60 dias")


#Probabilidade de Lift

ggplot(data = clientes, aes(x = proba)) +
  geom_histogram(color = "white",fill = "dodgerblue", position = "dodge") +
  xlab("Probabilidade") +
  ylab("Frequencia")


# Analise BIVARIADA CAIO -------------------------------------------------------

#Engajamenbto X Compras ?ltimos 60 dias

ggplot(data = clientes, aes(x = engajamento, y = compras_60_dias)) +
  geom_point(position = "jitter", color = "dodgerblue") +
  geom_smooth(method = lm, se = F)+
  xlab("Engajamento") +
  ylab("Compras ?ltimos 60 dias") +
  theme(legend.position = "right")


#Engajamenbto X Probabilidade de lift

ggplot(data = clientes, aes(x = engajamento, y = proba)) +
  geom_point(position = "jitter", color = "dodgerblue") +
  geom_smooth(method = lm, se = F)+
  xlab("Engajamento") +
  ylab("Probabilidade de Lift") +
  theme(legend.position = "right")


#Compras ?ltimos 60 dias X Probabilidade de lift

ggplot(data = clientes, aes(x = compras_60_dias, y = proba)) +
  +   geom_point(position = "jitter", color = "dodgerblue") +
  +   geom_smooth(method = lm, se = F)+
  +   xlab("Compras ?ltimos 60 dias") +
  +   ylab("Probabilidade de Lift") +
  +   theme(legend.position = "right")





# ==============================================================================
# Selecionando 100 Clientes Aleatoriamente -------------------------------------

clientes_100 = clientes[sample(nrow(clientes), 100),]
View(clientes_100)

proba <- ggplot(clientes_100, aes(x=persona, fill=persona)) +
  geom_bar() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
proba

# Seleciona apeas os IDs
client_id = select(clientes_100, c(id_cliente))
View(client_id)

# Separa 50%/50% controle/tratamento
controle = as.data.frame(client_id[1:50,])
View(controle)

tratamento = as.data.frame(client_id[51:100,])
View(tratamento)

# Salva em CSV
write.csv(controle, "E:/r-resource/Bweb/controle_100T1.csv", row.names = FALSE,  quote=FALSE)
write.csv(tratamento, "E:/r-resource/Bweb/tratamento_100T1.csv", row.names = FALSE,  quote=FALSE)




# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Analise dos dados ap?s execu??o da A??o de CUPONS via PYTHON (Google Colab)
# ------------------------------------------------------------------------------
# IMPORTANTE
#     Grupo A ? o CONTRLE 	  (condi??o atual) 	    = VENDAS SEM CUPONS
#	    Grupo B ? o TRATAMENTO 	(condi??o desafiante) = VENDAS COM CUPONS
# ------------------------------------------------------------------------------

experi_100cli = read.csv("E:/FGV/AULAS2022/Geracao-de-valor/Trabalho/final-work1-70_Bewb/test_100_V1/api_experiment_data_v1_100clients.csv")
View(experi_100cli)

# Analise univariada AMAURI-----------------------------------------------------

vendas_box <- ggplot(experi_100cli, aes(y=vendas, fill=grupo, )) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
vendas_box

vendas_bar <- ggplot(experi_100cli, aes(x=vendas, fill=grupo, )) +
  geom_bar() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
vendas_bar

# Plota os dois gr?dicos
grid.arrange(engaj , proba)

# -------------------------------------------------------------------------------
# Analise BIVARIADA AMAURI------------------------------------------------------

enga_box <-ggplot(experi_100cli, aes(x=grupo, y=vendas, fill=grupo)) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
enga_box

enga_bar <- ggplot(experi_100cli, aes(x=grupo, y=vendas, fill=grupo)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))+
  ylim(0, 3000)
enga_bar

# Plota os dois gr?dicos
grid.arrange(enga_box , enga_bar)


# ==================================================================================================
# JOIN do dataframe original com do dataframe gerado apos os testes com 100 clientes
# Obter os valores da Variavel PERSON com base no ID_CLIENTE ----------------------
filter_persona = clientes[clientes$id_cliente %in% c(experi_100cli$id_cliente), ]
View(filter_persona)


#### Left Join using merge function
merge_client = merge(x=filter_persona,y=experi_100cli,by="id_cliente")
View(merge_client)



# Analise BIVARIADA AMAURI------------------------------------------------------
# GRUPO CONTROLE::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
merge_client_controle = merge_client[merge_client$grupo =='controle', ]
View(merge_client_controle)

# Quantidade de clientes por amostra -------------------------------------------
table(merge_client_controle$persona)


# ......................................................................................
c_persona_vend_bar <- ggplot(merge_client_controle, aes(x=persona, y=vendas, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_vend_bar

c_persona_vend_box <-ggplot(merge_client_controle, aes(x=persona, y=vendas, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_vend_box

c1 = ggarrange(c_persona_vend_bar, c_persona_vend_box, ncol=2)
annotate_figure(c1, top = text_grob(" CONTROLE", color = "black", face = "bold", size = 14))
# ......................................................................................


# ......................................................................................
c_persona_enga_bar <- ggplot(merge_client_controle, aes(x=persona, y=engajamento, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_enga_bar

c_persona_enga_box <- ggplot(merge_client_controle, aes(x=persona, y=engajamento, fill=persona)) +
  geom_boxplot()+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_enga_box

c2 = ggarrange(c_persona_enga_bar, c_persona_enga_box, ncol=2)
annotate_figure(c2, top = text_grob(" CONTROLE", color = "black", face = "bold", size = 14))
# ......................................................................................


# ......................................................................................
c_persona_com_bar <- ggplot(merge_client_controle, aes(x=persona, y=compras_60_dias, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_com_bar

c_persona_com_box <- ggplot(merge_client_controle, aes(x=persona, y=compras_60_dias, fill=persona)) +
  geom_boxplot()+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_com_box

c3 = ggarrange(c_persona_com_bar, c_persona_com_box, ncol=2)
annotate_figure(c3, top = text_grob(" CONTROLE", color = "black", face = "bold", size = 14))
# ......................................................................................


# ......................................................................................
c_persona_prob_bar <- ggplot(merge_client_controle, aes(x=persona, y=proba, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_prob_bar

c_persona_prob_box <- ggplot(merge_client_controle, aes(x=persona, y=proba, fill=persona)) +
  geom_boxplot()+
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14))
c_persona_prob_box

c4 = ggarrange(c_persona_prob_bar, c_persona_prob_box, ncol=2)
annotate_figure(c4, top = text_grob(" CONTROLE", color = "black", face = "bold", size = 14))

# ......................................................................................

colnames(merge_client_controle)




# GRUPO TRATAMENTO::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
merge_client_tratamento = merge_client[merge_client$grupo =='tratamento', ]
View(merge_client_tratamento)

# Quantidade de clientes por amostra -------------------------------------------
table(merge_client_tratamento$persona)


# ......................................................................................
persona_vend_bar <- ggplot(merge_client_tratamento, aes(x=persona, y=vendas, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_vend_bar

persona_vend_box <-ggplot(merge_client_tratamento, aes(x=persona, y=vendas, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_vend_box

t1 = ggarrange(persona_vend_bar, persona_vend_box, ncol=2)
annotate_figure(a1, top = text_grob(" TRATAMENTO", color = "black", face = "bold", size = 14))
# ......................................................................................


# ......................................................................................
persona_enga_bar <- ggplot(merge_client_tratamento, aes(x=persona, y=engajamento, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_enga_bar

persona_enga_box <- ggplot(merge_client_tratamento, aes(x=persona, y=engajamento, fill=persona)) +
  geom_boxplot()+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_enga_box

t2 = ggarrange(persona_enga_bar, persona_enga_box, ncol=2)
annotate_figure(t2, top = text_grob(" TRATAMENTO", color = "black", face = "bold", size = 14))

# ......................................................................................


# ......................................................................................
persona_com_bar <- ggplot(merge_client_tratamento, aes(x=persona, y=compras_60_dias, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_com_bar

persona_com_box <- ggplot(merge_client_tratamento, aes(x=persona, y=compras_60_dias, fill=persona)) +
  geom_boxplot()+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_com_box

t3 = ggarrange(persona_com_bar, persona_com_box, ncol=2)
annotate_figure(t3, top = text_grob(" TRATAMENTO", color = "black", face = "bold", size = 14))

# ......................................................................................


# ......................................................................................
persona_prob_bar <- ggplot(merge_client_tratamento, aes(x=persona, y=proba, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_prob_bar

persona_prob_box <- ggplot(merge_client_tratamento, aes(x=persona, y=proba, fill=persona)) +
  geom_boxplot()+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_prob_box

t4 = ggarrange(persona_prob_bar, persona_prob_box, ncol=2)
annotate_figure(t4, top = text_grob(" TRATAMENTO", color = "black", face = "bold", size = 14))

# ......................................................................................

colnames(merge_client_tratamento)

# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# SEGUNDO TESTE com 100 clientes escolhidos de forma rand?mica
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Retirando os clientes do primeiro teste
clientes_sem_100T1 = as.data.frame(clientes[!clientes$id_cliente %in% clientes_100$id_cliente, ])
View(clientes_sem_100T1)

# Selecionando 100 Clientes Aleatoriamente -------------------------------------
clientes_100T2 = clientes[sample(nrow(clientes_sem_100T1), 100),]
View(clientes_100T2)

proba <- ggplot(clientes_100T2, aes(x=persona, fill=persona)) +
  geom_bar() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
proba

# Seleciona apeas os IDs
client_id = select(clientes_100T2, c(id_cliente))
View(client_id)

# Separa 50%/50% controle/tratamento
controle = as.data.frame(client_id[1:50,])
View(controle)

tratamento = as.data.frame(client_id[51:100,])
View(tratamento)

# Salva em CSV
write.csv(controle, "E:/r-resource/Bweb/controle_100T2.csv", row.names = FALSE,  quote=FALSE)
write.csv(tratamento, "E:/r-resource/Bweb/tratamento_100T2.csv", row.names = FALSE,  quote=FALSE)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Analise dos dados apos execusao da Acao de CUPONS via PYTHON (Google Colab)
# ------------------------------------------------------------------------------
# IMPORTANTE
#     Grupo A ? o CONTRLE 	  (condi??o atual) 	    = VENDAS SEM CUPONS
#	    Grupo B ? o TRATAMENTO 	(condi??o desafiante) = VENDAS COM CUPONS
# ------------------------------------------------------------------------------

experi_100cli_T2 = read.csv("E:/FGV/AULAS2022/Geracao-de-valor/Trabalho/final-work1-70_Bewb/test_100_V2/api_experiment_data_v2_100clients.csv")
View(experi_100cli_T2)


# Analise univariada AMAURI-----------------------------------------------------
vendas_box_T2 <- ggplot(experi_100cli_T2, aes(y=vendas, fill=grupo, )) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
vendas_box_T2

vendas_bar_T2 <- ggplot(experi_100cli_T2, aes(x=vendas, fill=grupo, )) +
  geom_histogram(bins = 30)+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
vendas_bar_T2


# -------------------------------------------------------------------------------
# Analise BIVARIADA AMAURI------------------------------------------------------

enga_box_T2 <-ggplot(experi_100cli_T2, aes(x=grupo, y=vendas, fill=grupo)) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
enga_box_T2

enga_bar_T2 <- ggplot(experi_100cli_T2, aes(x=grupo, y=vendas, fill=grupo)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))+
  ylim(0, 3000)
enga_bar_T2

# Plota os dois gr?dicos
grid.arrange(enga_box_T2, enga_bar_T2, ncol=2)


# ==================================================================================================
# JOIN do dataframe original com do dataframe gerado apos os testes com 100 clientes
# Obter os valores da Variavel PERSON com base no ID_CLIENTE ----------------------
filter_persona_T2 = clientes[clientes$id_cliente %in% c(experi_100cli_T2$id_cliente), ]
View(filter_persona_T2)


#### Left Join using merge function
merge_client_T2 = merge(x=filter_persona_T2,y=experi_100cli_T2,by="id_cliente")
View(merge_client_T2)


table(merge_client_T2$persona)




# Analise BIVARIADA AMAURI------------------------------------------------------
# GRUPO CONTROLE::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
merge_client_controle_T2 = merge_client_T2[merge_client_T2$grupo =='controle', ]
View(merge_client_controle_T2)

# Quantidade de clientes por amostra -------------------------------------------
table(merge_client_controle_T2$persona)


# ......................................................................................
c_persona_vend_bar <- ggplot(merge_client_controle_T2, aes(x=persona, y=vendas, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_vend_bar

c_persona_vend_box <-ggplot(merge_client_controle_T2, aes(x=persona, y=vendas, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
c_persona_vend_box

c1 = ggarrange(c_persona_vend_bar, c_persona_vend_box, ncol=2)
annotate_figure(c1, top = text_grob(" CONTROLE", color = "black", face = "bold", size = 14))
# ......................................................................................



# GRUPO TRATAMENTO::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
merge_client_tratamento_T2 = merge_client_T2[merge_client_T2$grupo =='tratamento', ]
View(merge_client_tratamento_T2)

# Quantidade de clientes por amostra -------------------------------------------
table(merge_client_tratamento_T2$persona)


# ......................................................................................
persona_vend_bar <- ggplot(merge_client_tratamento_T2, aes(x=persona, y=vendas, fill=persona)) +
  geom_bar(stat = "identity")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_vend_bar

persona_vend_box <-ggplot(merge_client_tratamento_T2, aes(x=persona, y=vendas, fill=persona)) +
  geom_boxplot() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
persona_vend_box

t1 = ggarrange(persona_vend_bar, persona_vend_box, ncol=2)
annotate_figure(t1, top = text_grob(" TRATAMENTO", color = "black", face = "bold", size = 14))
# ......................................................................................


# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# =================================================================================================
# TERCEIRO TESTE com todos os clientes restantes ou seja 397 (clientes tratamento) clientes escolhidos de forma randomica
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Retirando os clientes do primeiro teste
clientes_sem_100T1 = as.data.frame(clientes[!clientes$id_cliente %in% experi_100cli$id_cliente, ])
View(clientes_sem_100T1)

# Retirando os clientes do primeiro teste
clientes_sem_100T2 = as.data.frame(clientes[!clientes$id_cliente %in% experi_100cli_T2$id_cliente, ])
View(clientes_sem_100T2)

# Total de Linhas/Registros em cada dataframe, apos o tratamento acima
nrow(clientes)
nrow(clientes_sem_100T1)
nrow(clientes_sem_100T2)

# Join dos dois dataframes
clientes_T3 = merge(x = clientes_sem_100T1, y = clientes_sem_100T2, by = "id_cliente", all.x = TRUE)
nrow(clientes_T3)

# Validando de existe registros dos dataframes T1 e T2
validate_T1 = ifelse(clientes_sem_100T1$id_cliente %in% clientes_T3, "TRUE", "FALSE")
unique(validate_T1)

validate_T2 = ifelse(clientes_sem_100T2$id_cliente %in% clientes_T3, "TRUE", "FALSE")
unique(validate_T2)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Selecionando 794 Clientes Aleatoriamente ---------------------------------------------------------
clientes_794T3 = clientes[sample(nrow(clientes_T3), 794),]
table(clientes_794T3$persona)
View(clientes_794T3)


proba <- ggplot(clientes_794T3, aes(x=persona, fill=persona)) +
  geom_bar() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))
proba

# Seleciona apenas os IDs
client_id = select(clientes_794T3, c(id_cliente))
View(client_id)

# Separa 50%/50% controle/tratamento
controle = as.data.frame(client_id[1:397,])
View(controle)

tratamento = as.data.frame(client_id[398:794,])
View(tratamento)

# Salva em CSV
write.csv(controle, "E:/r-resource/Bweb/controle_397T3.csv", row.names = FALSE,  quote=FALSE)
write.csv(tratamento, "E:/r-resource/Bweb/tratamento_397T3.csv", row.names = FALSE,  quote=FALSE)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Analise dos dados apos execusao da Acao de CUPONS via PYTHON (Google Colab)
# ------------------------------------------------------------------------------
# IMPORTANTE
#     Grupo A e o CONTRLE 	  (condicao atual) 	    = VENDAS SEM CUPONS
#	    Grupo B e o TRATAMENTO 	(condicao desafiante) = VENDAS COM CUPONS
# ------------------------------------------------------------------------------

experi_397cli_T3 = read.csv("E:/r-resource/Bweb/api_experiment_data_T3_794clientes.csv")
View(experi_100cli_T2)

# ::::Analise dos resultados





