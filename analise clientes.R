# Pacotes e importando os dados 
######
# Pacotes
library(readr)
library(tidyverse)
library(lubridate)
library(corrplot)
library(psych)

# Importando os dados
dados_clientes <- read_csv("C:/Users/Lucas/Desktop/Case - Numera/dados_clientes.csv")
######

## Data exploration & cleaning
# Dados Gerais
#####
# Estatisticas descritivas
summary(dados_clientes)
str(dados_clientes)

# Checando os NA's
lapply(dados_clientes,
       function(x) { length(which(is.na(x)))})
  # mesmo numero de na's em genero e renda

# definindo um dataframe com as modifica��es necess�rias
df_Clientes <- dados_clientes %>%
  select(-1) %>%  # tirando o id
  drop_na()  # tirando os NA's

# Fatorando a vari�vel genero
df_Clientes$genero <- factor(df_Clientes$genero,
                             levels = c('M', 'F', 'O'),
                             labels = c('Homem', 'Mulher', 'Outro'))# convertendo a vari�vel membro_desde para tipo data

# Definindo a variavel de data como date
df_Clientes$membro_desde <- ymd(df_Clientes$membro_desde)

# Explorando os dados dos clientes
summary(df_Clientes)

# Identifiquei uma quantidade anormal de idades acima dos 90 anos
df_Clientes %>% filter_all(any_vars(. %in% c(101))) 

#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(df_Clientes$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))
# A maior parte dos clientes � constitu�da por homens

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(df_Clientes$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 40  e 60 anos

# Histograma das idades dos clientes
hist(df_Clientes$idade,
     col= "blue",
     main="Histograma das idades dos clientes",
     xlab="Idade",
     ylab="Frequencia",
     labels=TRUE) 
# de fato tem muita gente acima de 80

# Renda
# Histograma da renda dos clientes
hist(df_Clientes$renda_anual,
     col="#660033",
     main="Renda anual dos clientes (R$)",
     xlab="Renda Anual",
     ylab="Frequencia",
     labels=TRUE)
# Parece haver uma concentra��o maior de clientes entre 40 e 80k

# Box plot da renda dos clientes
boxplot(df_Clientes$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# De fato a m�dia de renda est� entre 40 e 80k

### An�lise bivariada
## genero x idade
# Avaliando as idades de acordo com o genero
ggplot(df_Clientes) + 
  aes(idade, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Idade de acordo com o genero')
#parece haver um grupo grande de pessoas entre 50 e 75 anos

# density plot
ggplot(df_Clientes)+
  aes(idade, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Idade dos clientes de acordo com o genero')
# As mulheres s�o em m�dia um pouco mais velhas que os homens
# Ha mais homens jovens que mulheres

## genero x renda anual
# Avaliando a renda anual de acordo com o genero
ggplot(df_Clientes) + 
  aes(renda_anual, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Renda anual de acordo com o genero') 
# � poss�vel identificar 4 grupos de renda
# Parecem haver mais mulheres no grupo com maior renda (acima de 100k)

# density plot
ggplot(df_Clientes)+
  aes(renda_anual, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Renda anual de acordo com o genero')
# A maior parte dos homens ganha at� 75k anual
# h� mais mulheres ganhando mais de 75k do que homens

## genero x tempo como membro
# Bar plot
ggplot(df_Clientes) + 
  aes(membro_desde, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Tempo de fidelidade de acordo com o genero')
# parecem haver 3 grupos distintos de membros de acordo com o tempo como membro

# density plot
ggplot(df_Clientes)+
  aes(membro_desde, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Tempo de fidelidade de acordo com o genero')
# No primeiro momento(2014/2015) pareciam haver mais membros homens
# No segundo (2016-2017) entraram mais mulheres

## Idade x Renda
ggplot(df_Clientes, 
       aes(x = idade, 
           y = renda_anual)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# parece haver uma clara distin��o de idade e renda
# a linha de tendencia indica que quanto mais velhos as pessoas recebem mais

# analisando a correla��o entre renda e idade
df_num <- df_Clientes %>%
  select_if(is.numeric)

mcor <- cor(df_num)

# Corr plot
corrplot(mcor, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7)
# correla��o positiva, como esperado

# Idade x Tempo de fidalidade
ggplot(df_Clientes, 
       aes(x = idade, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# refor�a que h� uma diferen�a clara no numero de membros no tempo em que entraram

## Detectei umas anomalias: pessoas com idade avan�ada e rendimentos muito altos
#####

# Analizando de acordo com o ano em que o cliente cadastrou
# Grupo 1 - at� 2016
group1 <- df_Clientes %>%
  filter(membro_desde < '2016-01-01') 
#####
# Estatisticas descritivas
summary(group1)
str(group1)

#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group1$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))
# A maior parte � constitu�da por homens

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group1$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 40  60 e  anos

# Histograma das idades dos clientes
hist(group1$idade,
     col= "blue",
     main="Histograma das idades dos clientes",
     xlab="Idade",
     ylab="Frequencia",
     labels=TRUE) 
# mesma distribui��o normal

# Renda
# Box plot da renda dos clientes
boxplot(group1$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# De fato a m�dia de renda est� entre 50 e 70k e alguns outliers

### An�lise bivariada
## genero x idade
# density plot
ggplot(group1)+
  aes(idade, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Idade dos clientes de acordo com o genero')
# As mulheres s�o em m�dia um pouco mais velhas que os homens
# Ha mais homens jovens que mulheres

## genero x renda anual
# Avaliando a renda anual de acordo com o genero
# density plot
ggplot(group1)+
  aes(renda_anual, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Renda anual de acordo com o genero')
# A maior parte dos homens ganha at� 75k anual
# h� mais mulheres ganhando mais de 75k do que homens
# Parecem haver mais mulheres no grupo com maior renda

## genero x tempo como membro
# density plot
ggplot(group1)+
  aes(membro_desde, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Tempo de fidelidade de acordo com o genero')
# De fato haviam mais homens nos primeiros anos e em 2016 o cen�rio mudou

# Renda x Tempo de fidelidade
ggplot(group1, 
       aes(x = renda_anual, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# Nao haviam membros com rendimento superior a 100k ate 2016

#####
# Grupo 2 - 2016/2017
group2 <- df_Clientes %>% 
  filter(between(membro_desde,
                 as.Date('2016-01-01'),
                 as.Date('2017-12-31')))
#####
# Estatisticas descritivas
summary(group2)
str(group2)

#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group2$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))
# A diferen�a entre homens e mulheres � a menor

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group2$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 40 e poucos e 60 e poucos anos

# Renda
# Box plot da renda dos clientes
boxplot(group2$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# M�dia mais concentrada entre 50 e poucos e 80 e poucos

### An�lise bivariada
## genero x idade
# density plot
ggplot(group2)+
  aes(idade, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Idade dos clientes de acordo com o genero')
# As mulheres s�o em m�dia um pouco mais velhas que os homens
# Ha mais homens jovens que mulheres

## genero x renda anual
# Avaliando a renda anual de acordo com o genero
# density plot
ggplot(group2)+
  aes(renda_anual, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Renda anual de acordo com o genero')
# A maior parte dos homens ganha at� 75k anual
# h� mais mulheres ganhando mais de 75k do que homens
# Parecem haver mais mulheres no grupo com maior renda

## Idade x Renda
ggplot(group2, 
       aes(x = idade, 
           y = renda_anual)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# Repete o padr�o dos outros anos
# Apresenta o mesmo padr�o de segmento dos dados completos

# Renda x Tempo de fidelidade
ggplot(group2, 
       aes(x = renda_anual, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# uma correla��o negativa entre a renda e o tempo de fidelidade
#####
# Grupo3 - 2018 em diante
group3 <- df_Clientes %>%
  filter(membro_desde > '2017-12-31')
#####
#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group3$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group3$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 40 e 70  anos

# Renda
# Box plot da renda dos clientes
boxplot(group3$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# M�dia mais concentrada entre 40 e poucos e 70 e poucos com alguns outliers

### An�lise bivariada
## genero x idade
# density plot
ggplot(group3)+
  aes(idade, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Idade dos clientes de acordo com o genero')
# Ha mais homens jovens que mulheres
# H� uma concentra��o maior de mulheres entre 40 e 70 anos

## genero x renda anual
# Avaliando a renda anual de acordo com o genero
ggplot(group3) + 
  aes(renda_anual, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Renda anual de acordo com o genero') 
# Aqui tem muito mais homens ganhando menos

## Idade x Renda
ggplot(group3, 
       aes(x = idade, 
           y = renda_anual)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# Repete o padr�o dos outros anos
#####

# A an�lise por ano de cadastro refor�a a tese que a segementa��o esta relacionada a idade e renda dos clientes

# Analisando por grupos de acordo com as idades dos clientes

# Grupo 1 - at� 35 anos
group1 <- df_Clientes %>%
  filter(idade <= 35)
#####
# Estatisticas descritivas
summary(group1)
str(group1)

#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group1$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))
# Na faixa etaria ate 35 anos tem muito mais homem

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group1$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# m�dia entre 23 e 32

# Renda
# Histograma da renda dos clientes
hist(group1$renda_anual,
     col="#660033",
     main="Renda anual dos clientes (R$)",
     xlab="Renda Anual",
     ylab="Frequencia",
     labels=TRUE)
# quanto maior a renda menor o numero de clientes
# poquissimos com rendimento superior a 75k

# Box plot da renda dos clientes
boxplot(group1$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# m�dia entre 40k e 60 e poucos k anuais

### An�lise bivariada
## genero x idade
# density plot
ggplot(group1)+
  aes(idade, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Idade dos clientes de acordo com o genero')
# uma ocorrencia maior de mulheres acima dos 30

## genero x renda anual
# density plot
ggplot(group1)+
  aes(renda_anual, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Renda anual de acordo com o genero')
# as mulheres desse grupo parecem ter uma renda anual um pouco maior
# Os homens dessa faixa et�ria parecem receber menos

## genero x tempo como membro
# density plot
ggplot(group1)+
  aes(membro_desde, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Tempo de fidelidade de acordo com o genero')
# houve uma ocorrenci maior de mulheres entre os anos de 2016 e 2017, mas depois a maioria foi de homens
# a maior parte dos jovens entrou em 2018 em diante

## Idade x Renda
ggplot(group1, 
       aes(x = idade, 
           y = renda_anual)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# realmente ha um teto de renda para essa faixa et�ria

# Idade x Tempo de fidalidade
ggplot(group1, 
       aes(x = idade, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")

# Renda x Tempo de fidelidade
ggplot(group1, 
       aes(x = renda_anual, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# N�o havia ninguem ganhando mais que 70k

#####

# Grupo 2 - De 35 a 48 anos 
group2 <- df_Clientes %>%
  filter(idade > 35 & idade <= 48)
#####
#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group2$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))
# A discrepancia entre homens e mulheres segue a mesma propor��o

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group2$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 40 e 46

# Histograma das idades dos clientes
hist(group2$idade,
     col= "blue",
     main="Histograma das idades dos clientes",
     xlab="Idade",
     ylab="Frequencia",
     labels=TRUE) 
# h� uma grande concentra��o de pessoas com 36

# Renda
# Histograma da renda dos clientes
hist(group2$renda_anual,
     col="#660033",
     main="Renda anual dos clientes (R$)",
     xlab="Renda Anual",
     ylab="Frequencia",
     labels=TRUE)
# H� uma concentra��o grande de rendimentos entre 40k e 80k
# poucos ganhando acima de 100k

# Box plot da renda dos clientes
boxplot(group2$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# M�dia mais concentrada entre 50 e poucos e 80  com outliers

### An�lise bivariada
## genero x idade
# Avaliando as idades de acordo com o genero
ggplot(group2) + 
  aes(idade, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Idade de acordo com o genero')

# apesar de minoria h� uma maior ocorrencia de mulheres acima dos 40

## genero x renda anual
# Avaliando a renda anual de acordo com o genero
ggplot(group2) + 
  aes(renda_anual, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Renda anual de acordo com o genero') 
# Parecem haver relativamente mais mulheres no grupo com maior renda
# ha maior ocorrencia de homens recebendo ate 50k
# ha uma ocorrencia maior de mulheres recebendo de 50 a 70k

## genero x tempo como membro
# density plot
ggplot(group2)+
  aes(membro_desde, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Tempo de fidelidade de acordo com o genero')
# ha uma ocorrencia maior de mulheres aderindo a aprtir de 2018

## Idade x Renda
ggplot(group2, 
       aes(x = idade, 
           y = renda_anual)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# h� um teto de rendimento de 100k at� os 48 anos

#####

# Grupo 3 - De 48 em diante
group3 <- df_Clientes %>%
  filter(idade > 48)
#####
#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group3$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))
# pouca diferen�a entre homens e mulheres nessa faixa etaria

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group3$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 50 e 70 anos
# quantidade consider�vel de outliers

# Histograma das idades dos clientes
hist(group3$idade,
     col= "blue",
     main="Histograma das idades dos clientes",
     xlab="Idade",
     ylab="Frequencia",
     labels=TRUE) 
# uma concentra��o grande de pessoas de 50 a 70 anos

# Renda
# Histograma da renda dos clientes
hist(group3$renda_anual,
     col="#660033",
     main="Renda anual dos clientes (R$)",
     xlab="Renda Anual",
     ylab="Frequencia",
     labels=TRUE)

# Box plot da renda dos clientes
boxplot(group3$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# M�dia mais concentrada entre 40 e poucos e 70 e poucos com alguns outliers

### An�lise bivariada
## genero x idade
# density plot
ggplot(group3)+
  aes(idade, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Idade dos clientes de acordo com o genero')
# h� uma quantidade maior de homens mais "jovens"
# Ha uma concentra��o maior de homens ate 60
# H� uma concentra��o maior de mulheres depois dos 60

## genero x renda anual
# density plot
ggplot(group3)+
  aes(renda_anual, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Renda anual de acordo com o genero')
# ha uma concentra��o maior de homens ganhando ate 75k
# ha uma concentra��o maior de mulheres ganhando mais de 75k

## genero x tempo como membro
# density plot
ggplot(group3)+
  aes(membro_desde, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Tempo de fidelidade de acordo com o genero')

# Idade x Tempo de fidalidade
ggplot(group3, 
       aes(x = idade, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")

# Renda x Tempo de fidelidade
ggplot(group3, 
       aes(x = renda_anual, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# os membros que ganham mais de 100k s�o mais novos

#####

# Analisando o grupo 3 eu corroborei a suspeita de uma quantidade anormal de pessoas declaando ter mais de 90 anos
# Grupo 4 - acima de 90
group4 <- df_Clientes %>%
  filter(idade > 90)
#####
#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group4$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))
# maioria de mulheres

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group4$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')

# Histograma das idades dos clientes
hist(group4$idade,
     col= "blue",
     main="Histograma das idades dos clientes",
     xlab="Idade",
     ylab="Frequencia",
     labels=TRUE) 
# h� um numero muito grande de pessoas com 90 anos

# Renda
# Histograma da renda dos clientes
hist(group4$renda_anual,
     col="#660033",
     main="Renda anual dos clientes (R$)",
     xlab="Renda Anual",
     ylab="Frequencia",
     labels=TRUE)
# Muita gente ganhando mais de 100k
#####

# Considerando que a idade minima de aposentadoria para homens e mulheres no Brasil n�o passa dos 70 anos,
# e levando e considera��o que as aposentadorias no brasil n�o s�o t�o altas
# � muito imporv�vel que hajam tantas pessoas com mais de 90 anos ganhando tanto ao ano
# Essas fatos corroboram a hip�tese que os dados de clientes podem estar enviesados:
# alguns clientes podem n�o ter preenchido corretamente de forma acidental ou deliberada