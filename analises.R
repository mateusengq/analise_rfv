###### Mateus Santos Xavier #######

## Pacotes
library(tidyverse)
library(readr)
library(class)
library(factoextra)

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
dados$InvoiceDate <- mdy_hm(dados$InvoiceDate)

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

## Análises Exploratórias
## Quartil - método 1

dados$quartil <- ''
q1 <- quantile(dados$gasto, 0.25)
q2 <- quantile(dados$gasto, 0.5)
q3 <- quantile(dados$gasto, 0.75)

startTime <- Sys.time()
for (i in 1:nrow(dados)){
    if(dados$gasto[i] <= q1){
        dados$quartil[i] <- "q1"
    }else if(dados$gasto[i] <= q2){
        dados$quartil[i] <- "q2"
    }else if(dados$gasto[i] <= q3){
        dados$quartil[i] <- "q3"
    }else{
        dados$quartil[i] <- "q4"
    }
}
endTime <- Sys.time()
tempo_modelo1 <- endTime - startTime

# método extremamente lento, dicas: transformar o quantile em um número, assim evita um cálculo por vex

## Análise da variável gasto
hist(dados$gasto)
summary(dados$gasto)

## criado um novo data frame com recencia
df_recencia <- dados %>%
    select(CustomerID, InvoiceDate) %>%
    group_by(CustomerID) %>%
    summarise(data_maior = max(InvoiceDate, na.rm = T),
              data_menor = min(InvoiceDate, na.rm = T)) # a inclusão de data menor foi apenas como análise

head(df_recencia)

# variável - última compra
cur_date <- max(dados$InvoiceDate, na.rm = T)
cur_date

# criação de uma função que receberá a data cur_date e a data da compra

df_recencia$recencia <- (lubridate::interval(df_recencia$data_maior, cur_date)) %/% months(1)
table(df_recencia$recencia)
head(df_recencia)

summary(df_recencia$CustomerID)
summary(df_recencia$recencia)

# gráfico de frequências da recência

ggplot(dados = df_recencia) +
    geom_bar(aes(x = df_recencia$recencia), fill = 'darkblue') +
    labs(x = 'Meses', y = 'Número de observações', title = 'Recências por mês',
         caption = 'Fonte: Kaggle | E-commerce Data') +
    xlim(0, 12) +
    scale_x_continuous(breaks = seq(0, 12, by = 1)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 20),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)
          )

# selecionando apenas as informações necessárias da base recencias
df_recencia <- df_recencia %>%
    select(CustomerID, recencia)

## Avaliar as variáveis de frequencia e valor monetário
df_fm <- dados[,c("CustomerID", "InvoiceNo", "gasto")]

# Calculando o número de pedidos e o pagamento médio

df_fm <- df_fm %>%
    group_by(CustomerID) %>%
    summarise(cont = n(),
              media = round(mean(gasto, na.rm = T), 2))

# junção das bases de trabalho

df_rfv = merge(df_fm, df_recencia, by = 'CustomerID')
View(df_rfv)                            

## Segmentação de clientes
# Como o modelo KNN-mean é baseado em distância, é necessário ajustar um tratamento de escala
# Criando uma base para tratamento dos dados

df <- df_rfv
summary(df)

df$cont <- scale(df$cont)
hist(df$cont)
df$media <- scale(df$media)
hist(df$media)
df$recencia <- scale(df$recencia)


summary(df)

#### Análise do cluster
## Definição do número de cluster
?fviz_nbclust
fviz_nbclust(df[2:4], kmeans, method = 'wss') +
    geom_vline(xintercept = 6, linetype = 2)

# Com 3 grupos já temos um número bom de cluster, mas pensando no negócio, testaremos com 6

set.seed(0711)

km.res <- kmeans(df[2:4], 6, nstart = 25)
print(km.res)

aggregate(df_rfv, by = list(km.res$cluster), mean)
df_2 <- cbind(df, cluster = km.res$cluster)
table(df_2$cluster)

fviz_cluster(km.res, data=df_2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", 'green', 'blue', "firebrick"),
             #ellipse.type="euclid",
             star.plot=TRUE,
             #repel=TRUE,
             ggtheme=theme_minimal()
)

## análise do impacto de cada variável
## 
ggplot(df_2, aes(x = as.factor(cluster), y = recencia)) +
    geom_boxplot()

ggplot(df_2, aes(x = as.factor(cluster), y = cont)) +
    geom_boxplot()

ggplot(df_2, aes(x = as.factor(cluster), y = media)) +
    geom_boxplot()

ggplot(df_2, aes(x = CustomerID, y = cont, col = as.factor(cluster))) +
    geom_point()
