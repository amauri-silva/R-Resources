# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
#
# Amauri Silva      - Matrícula: A58330068
# Caio Ramos        - Matrícula: A58338177
# Isabela Massaro   - Matrícula: A58337886
# Márcia Magalhães  - Matrícula: A58331043
# Marina 			      - Matrícula:
#
# Turma: T10
# Curso: MBA EM BUSINESS ANALYTICS E BIG DATA
# Matéria: ANALISE DE MIDIAS SOCIAIS E TEXTO
# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

# :::Importação das libs
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("tidytext")
library(tidytext)
install.packages("topicmodels")
library(topicmodels)
install.packages("ggplot2")
library(ggplot2)


# :::Limpa os dados da memória
rm(list = ls())

# :::Para evitar notação cientifica (3,4E+28)
options(scipen = 999)


# :::Importação do dataset
dataFrame = read.csv("./data/trump_insult_tweets_2014_to_2021.csv", sep = ',')

# Tratamento do dataset
dataFrame = select(dataFrame, -X)
dataFrame$year = year(dataFrame$date)
col_order = c("target", "insult", "tweet", "date", "year")

# Dataset final
twitters = dataFrame[, col_order]
View(twitters)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# (Amauri)
# 1)Tratar a data (criar variável ano)

# (CAIO)
# 2) (Descritiva) - Tokenização, Contagem de Palavras
# Analise-1: geral
# Analise-2: por ano (2014 - 2021)


# 3) (Aprendiz.N.Superv.) - Análise de Sentimento.
# (Isabela)Analise-1: geral
# (Marina) Analise-2: por ano (2014 - 2021)

twitters_2 <- twitters
nrow(twitters_2)

#atribuindo uma identifica??o para cada tweet
for (i in 1:nrow(twitters_2)) {
  twitters_2$row[i] <- i
}

#abrindo cada tweet por palavra e definindo um sentimento (dicion?rio bing)
twitters_word <- twitters_2 
twitters_word <- unnest_tokens(twitters_word, input = tweet, output = word) 
twitters_word <-twitters_word %>% inner_join(get_sentiments("bing"), by = c("word" = "word"))

#removendo as colunas que n?o v?o ser usadas
twitters_word <- twitters_word %>% select (-target, -insult)

#abrindo os tweets por sentimento, e agrupando por ano
twitters_ano <- twitters_word %>% count(row, sentiment,year) %>% spread(key = sentiment, value = n)
twitters_word <- twitters_word %>% count(row, sentiment) %>% spread(key = sentiment, value = n)

#trocando NA por 0, para c?lculo do sentimento l?quido
twitters_word$negative[which(is.na(twitters_word$negative))] <- 0
twitters_word$positive[which(is.na(twitters_word$positive))] <- 0

twitters_ano$negative[which(is.na(twitters_ano$negative))] <- 0
twitters_ano$positive[which(is.na(twitters_ano$positive))] <- 0

#calculando o sentimento l?quido
twitters_word$sentimento <- twitters_word$positive - twitters_word$negative
twitters_word <- twitters_word %>% select (-positive, -negative)

twitters_ano$sentimento <- twitters_ano$positive - twitters_ano$negative
twitters_ano <- twitters_ano %>% select (-positive, -negative)

#plotando por anos
twitters_ano %>% ggplot(aes(x = nrow(twitters_ano), y = sentimento, color = row)) +geom_col() +
  facet_wrap(~year)

twitters_ano %>% ggplot(aes(x = year, y = sentimento, color = row)) +geom_col()


# (Marcia)
# 4) (Aprendiz.N.Superv.) - Lei de Zipf. TF-IDF
# Analise-1: geral

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 5) (Aprendiz.N.Superv.) - Topic Modeling (Clusterização de Documentos e Palavras)

# 1°:::::::Etapa: Prepara os dados para gerar a matriz (DTM)
twitters_list = select(twitters, -tweet, -date)
# Convertendo a coluna year para inteiro
twitters_list$year = as.integer(twitters_list$year)
# Reordenando as colunas
twitters_list = twitters_list[, c(3, 1, 2)]

View(twitters_list)
sapply(twitters_list, class)

# Fazendo um cast para gerar o objeto DTM (DocumentTermMatrix)
twitter_dtm <- twitters_list %>%
  cast_dtm(target, insult, year)

View(twitter_dtm)


# 2°::::::::Etapa: Gerando cluster (K=2)
ap_lda <- LDA(twitter_dtm, k = 2, control = list(seed = 1234))

ap_topics_beta <-tidy(ap_lda, matrix = "beta")
ap_topics_beta
tidy(ap_lda, matrix = "gamma")

# plot 1
top_terms <- ap_topics_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Matriz beta (Maiores betas)::::::::::::::::::::::::::::::
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE, ) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  theme(axis.text = element_text(size = 24))

# Matriz beta (palavra-topico) - Maiores diferenciais
tidy(ap_lda, matrix = "beta") %>%
  mutate(topic = paste("topico",topic,sep="")) %>%
  spread(key=topic, value=beta) %>%
  filter(topico1>0.001 | topico2>0.001) %>%
  mutate(beta_spread = log10(topico2/topico1)) %>%
  group_by(topico = beta_spread>0) %>%
  top_n(10, abs(beta_spread)) %>%
  ggplot(aes(x=reorder(term,beta_spread), y=beta_spread)) +
  geom_col() + coord_flip() +
  theme(axis.text = element_text(size = 18))


# 2°::::::::Gerando cluster (K=4) ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
apk4_lda <- LDA(twitter_dtm, k = 4, control = list(seed = 1234))

apk4_topics_beta <-tidy(apk4_lda, matrix = "beta")
apk4_topics_beta
tidy(apk4_lda, matrix = "gamma")

# plot 1
top_terms <- apk4_topics_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  theme(axis.text = element_text(size = 14))


# Matriz beta (Maiores betas)::::::::::::::::::::::::::::::
tidy(apk4_lda, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  ggplot(aes(x=reorder(term,beta), y=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free_y") +
  coord_flip()+
  theme(axis.text = element_text(size = 14))

# Matriz beta (palavra-topico) - Maiores diferenciais
tidy(apk4_lda, matrix = "beta") %>%
  mutate(topic = paste("topico",topic,sep="")) %>%
  spread(key=topic, value=beta) %>%
  filter(topico1>0.001 | topico2>0.001) %>%
  mutate(beta_spread = log10(topico2/topico1)) %>%
  group_by(topico = beta_spread>0) %>%
  top_n(10, abs(beta_spread)) %>%
  ggplot(aes(x=reorder(term,beta_spread), y=beta_spread)) +
  geom_col() + coord_flip()


















