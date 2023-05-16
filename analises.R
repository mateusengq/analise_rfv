###### Mateus Santos Xavier #######

## Pacotes
library(tidyverse)
library(readr)

## Leitura dos dados

dados <- read_csv('data.csv')
head(dados)
tail(dados)

dplyr::glimpse(dados) # avaliação geral das colunas

## avaliação de valores ausentes
sum(is.na(dados))
map(dados, ~sum(is.na(.)))
# elevando número de dados ausentes, mas não temos poder de ação

## correção da variável data
dados$InvoiceDate <- as.Date(dados$InvoiceDate, format = "%d/%m/%y")

## Inclusão da variável gasto

dados$gasto <- dados$Quantity * dados$UnitPrice
summary(dados$gasto) # há variáveis com valores ou quantidades negativas
# para este modelo, optamos pela exclusão

## Exclusão da quantidade ou preço negativo
dados <- dados %>%
    filter(UnitPrice > 0 & Quantity > 0)

## Exclusão dos dados de CustomerID ausentes
dados <- dados %>%
    filter(!is.na(dados$CustomerID))

dplyr::glimpse(dados)
