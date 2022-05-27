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
library(NLP)
#library(hrbrthemes)
#library(gdtools)



#--------------------------------------------------------------------------------#
listing   <- tibble(read.csv('./data/rio/listings.csv', stringsAsFactors = TRUE))
calendar  <- tibble(read.csv('./data/rio/calendar.csv', stringsAsFactors = TRUE))
reviews   <- tibble(read.csv('./data/rio/reviews.csv', stringsAsFactors = TRUE))
# Dataset sem valor para a analise (removido)
neighbhd  <- tibble(read.csv('./data/rio/neighbourhoods.csv', stringsAsFactors = TRUE))

View(listing)
View(calendar)
View(reviews)
View(neighbhd)


#--------------------------------------------------------------------------------#
# [Bloco 2] Base de Dados e Técnicas Estatísticas e/ou de Machine Learning do Projeto Analítico

# 6) Quais variáveis formam a BASE DE DADOS (mostrar a base de dados completa)?
colnames(reviews)
nrow(reviews)
colnames(calendar)
nrow(calendar)
colnames(listing)
nrow(listing)
#colnames(neighbhd)
#nrow(neighbhd)


#===============================================================================
# 7) Quais variáveis foram concebidas após aplicação de feature engineering?

#Quantidade de variaveis por dataset




#===============================================================================
#-------------------------------------------------
# 8) Qual ANÁLISE EXPLORATÓRIA DE DADOS (EDA) foi realizada?

# ::::Listing:::::::::::::::::::::::::::::::::::::::::::
# Limpeza da tabela (remoção de alguns valores no qual não tem relevância)

# Os IDs listados nessas pesqusa não tem registros de dispobinilidade

test = listing[listing$price == 0, ]
View(test)

# Gerando novo dataset sem os valores Zerados (price)
listing2 = listing[! (listing$price == 0), ]
View(listing2)


#Missing Values ----------------------------------------------------------------
sum(is.na(listing))
summary(colnames(listing))
misv_listing <- colSums(is.na(listing))
df3 <- data.frame(t(misv_listing[-1]))
df3 <- select(df3, neighbourhood_group, reviews_per_month, license)
View(df3)

# Com base no resultado acima, será excludino as colunas neighbourhood_group e license
str(listing)

listing3 <- subset(listing2,select = c(-neighbourhood_group, -license))
View(listing3)


# Numero de reviews X numero de reviews por mês (substituindo os NAs por 0.0)
listing4 <- listing3 %>% mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 0.0, reviews_per_month))
View(listing4)
misv_listing <- colSums(is.na(listing4))
misv_listing

# Total de linhas contendo valor vazio
sum(listing4 == "")

sum(listing4$id == "")
sum(listing4$name == "")
name = c(21)
sum(listing4$host_id == "")
sum(listing4$host_name == "")
host_name  = c(15)
sum(listing4$neighbourhood == "")
sum(listing4$latitude == "")
sum(listing4$longitude == "")
sum(listing4$room_type == "")
sum(listing4$price == "")
sum(listing4$minimum_nights == "")
sum(listing4$number_of_reviews == "")
sum(listing4$last_review == "")
last_review = c(8955)
sum(listing4$reviews_per_month == "")
sum(listing4$calculated_host_listings_count == "")
sum(listing4$availability_365 == "")
sum(listing4$number_of_reviews_ltm == "")


emp_listing = data.frame(last_review, host_name, name)
View(emp_listing)


# -----------------------------------------------------------------
# Correlação das veriaveis
df_corr  = select(listing4, c(-id, -name, -host_name, -neighbourhood, -room_type, -last_review))
View(df_corr)
str(df_corr)
corr_matrix = cor(df_corr[, -1])

corr_matrix = round(corr_matrix, 3)

write.csv(corr_matrix, file = './data/rio/corr_matrix.csv')
dfcorr_matrix = tibble(read.csv('./data/rio/corr_matrix.csv', sep = ','))
View(dfcorr_matrix)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# GRAFICOS/PLOTS 

# Analise univariada------------------------------------------------------------

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Analise univariada------------------------------------------------------------

listing4 %>%
  ggplot( aes(x=calculated_host_listings_count)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Quantidade de residências/apartamentos por hoster")

listing4 %>%
  ggplot( aes(x=calculated_host_listings_count)) +
  geom_bar() +
  ggtitle("") +
  xlab("Quantidade de residências/apartamentos por hoster")

# ------------------------------------------------------------------------------
listing4 %>%
  ggplot( aes(x=availability_365)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Disponibilidade no decorrer de 12 meses")

listing4 %>%
  ggplot( aes(x=availability_365)) +
  geom_bar() +
  ggtitle("") +
  xlab("Disponibilidade no decorrer de 12 meses")

# ------------------------------------------------------------------------------

listing4 %>%
  ggplot( aes(x=number_of_reviews)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Número de reviews")

listing4 %>%
  ggplot( aes(x=number_of_reviews)) +
  geom_bar() +
  ggtitle("") +
  xlab("Número de reviews")

listing4 %>%
  ggplot( aes(x=price)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Preço")

listing4 %>%
  ggplot( aes(x=minimum_nights)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Reserva mínima")

listing4 %>%
  ggplot( aes(x=number_of_reviews_ltm)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Número de reviews nos últimos 12 meses")

# ------------------------------------------------------------------------------
listing %>%
  ggplot( aes(x=room_type)) +
  geom_bar() +
  ggtitle("") +
  xlab("Tipo de quarto")


# Analise Bivariada-------------------------------------------------------------
listing4 %>%
  ggplot( aes(x=room_type, y=price, color= room_type)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Tipo de quarto")


# Removendo Outlier
listing5 = subset(listing4, price != 650476)
View(listing5)

listing5 %>%
  ggplot( aes(x=room_type, y=price, color=room_type)) +
  geom_boxplot() +
  ggtitle("") +
  ylim(0, 5000)+
  xlab("Tipo de quarto")

listing5 %>%
  ggplot( aes(x=room_type, y=price, color=room_type)) +
  geom_boxplot() +
  ggtitle("") +
  ylim(0, 500)+
  xlab("Tipo de quarto")

g =unique(listing5$host_id == 224192)
g

ggplot(listing5, aes(x=room_type, y=price, color=room_type)) + 
  geom_point(size=2)

listing5 %>%
  ggplot( aes(x=number_of_reviews, fill=room_type, color=room_type)) +
  geom_histogram(position="identity", alpha=.05) +
  ggtitle("") +
  xlab("Número de Reviews")


listing5 %>%
  ggplot( aes(x=price, fill=neighbourhood, color=neighbourhood)) +
  geom_histogram(position="identity", alpha=.05, stat = "count") +
  ggtitle("") +
  xlim(0, 5000)
  xlab("Número de Reviews")+
  ylab("Quantidade")
# Com base nos dados do gráfico a cima, seria interessante adicionar uma nova variável com a
# classificação das regiões do Rio (Centro, Zona Sul, Zona Norte, etc) para então classificar cada bairros de acordo com a região
# https://blog.loft.com.br/zonas-rio-de-janeiro/


listing5 %>%
  ggplot( aes(x=neighbourhood['copacabana'], fill=room_type, color=room_type)) +
  geom_histogram(position="identity", alpha=.1, stat = "count") +
  ggtitle("") +
  xlab("Número de Reviews")+
  ylab("Quantidade")


listing5 %>%
  ggplot( aes(x=number_of_reviews, y=room_type, color=room_type)) +
  geom_boxplot() +
  ggtitle("") +
  xlim(0, 50)
  xlab("Número de Reviews")+
  ylab("Tipo de Quarto")
  



# Obtento os valores/IDs únicos
unique(listing5$neighbourhood)

# Gráfico de barras ------------------------------------------------------------
listing5 %>%
  ggplot( aes(x=number_of_reviews)) +
  geom_bar() +
  ggtitle("") +
  xlab("Número de Reviews")+
  ylab("Preço")


# Gráfico de geopoints ---------------------------------------------------------




listing5 %>%
  ggplot( aes(x=room_type, y=neighbourhood['copacabana'])) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Tipo de quarto")+
  ylab("Preço")


ggplot(listing5, aes(x=room_type, y=price)) + 
  geom_bar(stat = "identity")


ggplot(listing5, aes(x=room_type, y=price, color=room_type)) + 
  geom_point(size=2)


ggplot(listing5, aes(x=room_type, y=calendar$available, color=room_type)) + 
  geom_point(size=2)





plot(listing5$last_review, listing4$price, pch = 19, col = "black")

plot(listing5$neighbourhood, listing4$reviews_per_month, pch = 19, col = "black")



# ------------------------------------------------------------------------------
# ::::Reviews:::::::::::::::::::::::::::::::::::::::::::
summary(colnames(reviews))
str(reviews)

# Missin Values
sum(is.na(reviews))

# Verificando se tem valor vazio
sum(reviews$listing_id == "")
sum(reviews$date == "")



# Univariada -------------------------------------------------------------------

reviews %>%
  ggplot( aes(x=date)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Tipo de quarto")

# ------------------------------------------------------------------------------
reviews %>%
  ggplot( aes(x=date, color="red")) +
  geom_histogram(position="identity", alpha=.05, stat = "count") +
  ggtitle("") +
  xlab("Número de Reviews")+
  ylab("Quantidade")

# ------------------------------------------------------------------------------
# ::::Calendar:::::::::::::::::::::::::::::::::::::::::::

# Verificando se tem valor vazio
sum(calendar$listing_id == "")
sum(calendar$date == "")
sum(calendar$available == "")
sum(calendar$price == "")
sum(calendar$adjusted_price == "")
sum(calendar$minimum_nights == "")
sum(calendar$maximum_nights == "")

price = c(sum(calendar$price == ""))
adjusted_price = c(sum(calendar$adjusted_price == ""))

df_calen = data.frame(price, adjusted_price)
View(df_calen)

cor(calendar)

summary(colnames(calendar))
str(calendar)

#Missing Values
sum(is.na(calendar))
sum(any(is.na(calendar$price)))

missv_bycolumn <- colSums(is.na(calendar))
df2 <- data.frame(t(missv_bycolumn[-1]))
View(df2)

min_n <- calendar[is.na(calendar$minimum_nights), ]
max_n <- calendar[is.na(calendar$maximum_nights), ]
View(min_n)
View(max_n)

identical(calendar$price, calendar$adjusted_price)
intersect(calendar$price, calendar$adjusted_price)


View(miss_v_by_column)

df_calen2 <- calendar[(calendar$price != calendar$adjusted_price)]


# Obtento os valores/IDs únicos
unique(calendar$listing_id)

de = select(calendar$price == "" && calendar$listing_id, calendar$listing_id[13544282],0 )
View(de)



dm2 = calendar %>% filter(price != "")
View(dm2)
unique(dm2$listing_id)

ggplot(dm2, aes(x=date, color=available, fill= available)) + 
  geom_histogram(size=2)

ggplot(dm2, aes(x=date, y=available)) + 
  geom_bar(stat = "identity")


dm2 %>%
  ggplot( aes(x=date, y=available)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Número de Reviews")+
  ylab("Disponibilidade para locação")


dm2 %>%
  ggplot( aes(x=price, color=available)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Número de Reviews")+
  ylab("Tipo de Quarto")



calendar %>%
  ggplot( aes(x=price)) +
  geom_boxplot() +
  ggtitle("") +
  xlab("Tipo de quarto")
 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#--------------------------------------------------------------------------------#

# 1.1 - Criação variável preco_medio (usando o dataset Calendar) :::AMAURI
# DICIONARIO DE DADOS (variavel price)
# price	 = currency	daily price in local currency

View(calendar)


# Removendo o caractere $ para tranformar o valor em Integer
calendar$price = gsub("\\$", "", calendar$price)
View(calendar)

# Convertendo para integer
calendar$price = as.integer(calendar$price)
str(calendar)

# Calculando o valor medio do preco
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/ave
calendarv2 = calendar
calendarv2$preco_medio = ave(calendar$price, calendar$listing_id)
calendarv2$preco_medio = ifelse(is.na(calendarv2$preco_medio), calendar$price, calendarv2$preco_medio)
View(calendarv2)

# Caso queram remover as casas decimais do preco medio
#calendarv2$preco_medio = round(calendarv2$preco_medio, digits = 0)

# salvando o novo dataset/CSV 
write.csv(calendarv2, file = "./data/rio/calendarv2.csv", sep = ",", row.names = FALSE)
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------