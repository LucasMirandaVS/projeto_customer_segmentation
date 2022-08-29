# Setando o diret�rio
setwd("C:/Users/Lucas/Desktop/Case - Numera")

dados_clientes <- read_csv("C:/Users/Lucas/Desktop/Case - Numera/dados_clientes.csv")

# Analizando por grupos de ano
df_Clientes <- dados_clientes %>%
  select(-1) %>%  # tirando o id
  drop_na()


group1 <- df_Clientes %>%
  filter(membro_desde < '2016-01-01') 
#####
# Estatisticas descritivas
summary(group1)
str(group1)

# Identifiquei uma quantidade anormal de idades acima dos 90 anos
group1 %>% filter_all(any_vars(. %in% c(101))) # so 1

group1 %>% filter(idade > 90) # 28

#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group1$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))
# A maior parte da amostra � constitu�da por homens

# pie chart do genero
group1 %>% 
  pull(genero) %>%
  table() %>%
  pie(col = hcl.colors(length(table(group1$genero)),
                       "BluYl"))

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group1$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 40 e poucos e 60 e poucos anos

# Histograma das idades dos clientes
hist(group1$idade,
     col= "blue",
     main="Histograma das idades dos clientes",
     xlab="Idade",
     ylab="Frequencia",
     labels=TRUE) 
# apesar da m�dia da coluna cliente ja estar acima dos 40 anos,
# tem muita gente acima de 80
# aparenta ser uma distribui��o normal

# Renda
# Histograma da renda dos clientes
hist(group1$renda_anual,
     col="#660033",
     main="Renda anual dos clientes (R$)",
     xlab="Renda Anual",
     ylab="Frequencia",
     labels=TRUE)
# Parece haver uma concentra��o maior de clientes entre 50 e 70k

# Box plot da renda dos clientes
boxplot(group1$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# De fato a m�dia de renda est� entre 50 e 70k e alguns outliers

### An�lise bivariada
## genero x idade
# Avaliando as idades de acordo com o genero
ggplot(group1) + 
  aes(idade, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Idade de acordo com o genero')
# h� uma quantidade maior de homens

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
ggplot(group1) + 
  aes(renda_anual, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Renda anual de acordo com o genero') 
# Parecem haver mais mulheres no grupo com maior renda

# density plot
ggplot(group1)+
  aes(renda_anual, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Renda anual de acordo com o genero')
# A maior parte dos homens ganha at� 75k anual
# h� mais mulheres ganhando mais de 75k do que homens

## genero x tempo como membro
# Bar plot
ggplot(group1) + 
  aes(membro_desde, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Tempo de fidelidade de acordo com o genero')


# density plot
ggplot(group1)+
  aes(membro_desde, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Tempo de fidelidade de acordo com o genero')
# De fato haviam mais homens nos primeiros anos e em 2016 o cen�rio mudou

## Idade x Renda
ggplot(group1, 
       aes(x = idade, 
           y = renda_anual)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# Repete o padr�o dos outros anos


# analisando a correla��o entre renda e idade
df_num <- group1 %>%
  select_if(is.numeric)

mcor <- cor(df_num)

# Corr plot
corrplot(mcor, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7)
# correla��o positiva, como esperado
# Covariancia
print(cov(group1$idade,
          group1$renda_anual, method = "spearman")) 

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
# Nao haviam membros com rendimento superior a 100k ate 2016
#####
group2 <- df_Clientes %>% 
  filter(between(membro_desde,
                      as.Date('2016-01-01'),
                      as.Date('2017-12-31')))
#####
# Estatisticas descritivas
summary(group2)
str(group2)

# Identifiquei uma quantidade anormal de idades acima dos 90 anos
group2 %>% filter_all(any_vars(. %in% c(101))) # 4

group2 %>% filter(idade > 90) # 144

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

# pie chart do genero
group2 %>% 
  pull(genero) %>%
  table() %>%
  pie(col = hcl.colors(length(table(group2$genero)),
                       "BluYl"))

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group2$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 40 e poucos e 60 e poucos anos

# Histograma das idades dos clientes
hist(group2$idade,
     col= "blue",
     main="Histograma das idades dos clientes",
     xlab="Idade",
     ylab="Frequencia",
     labels=TRUE) 
# a mesma distribui��o do grupo completo e do grupo1

# Renda
# Histograma da renda dos clientes
hist(group2$renda_anual,
     col="#660033",
     main="Renda anual dos clientes (R$)",
     xlab="Renda Anual",
     ylab="Frequencia",
     labels=TRUE)

# Box plot da renda dos clientes
boxplot(group2$renda_anual, horizontal = TRUE,
        col = 'dark green', main = 'Renda anual dos clientes') 
# M�dia mais concentrada entre 50 e poucos e 80 e poucos

### An�lise bivariada
## genero x idade
# Avaliando as idades de acordo com o genero
ggplot(group2) + 
  aes(idade, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Idade de acordo com o genero')
# h� uma quantidade maior de homens

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
ggplot(group2) + 
  aes(renda_anual, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Renda anual de acordo com o genero') 
# Parecem haver mais mulheres no grupo com maior renda

# density plot
ggplot(group2)+
  aes(renda_anual, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Renda anual de acordo com o genero')
# A maior parte dos homens ganha at� 75k anual
# h� mais mulheres ganhando mais de 75k do que homens

## genero x tempo como membro
# Bar plot
ggplot(group2) + 
  aes(membro_desde, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Tempo de fidelidade de acordo com o genero')


# density plot
ggplot(group2)+
  aes(membro_desde, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Tempo de fidelidade de acordo com o genero')


## Idade x Renda
ggplot(group2, 
       aes(x = idade, 
           y = renda_anual)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# Repete o padr�o dos outros anos


# analisando a correla��o entre renda e idade
df_num <- group2 %>%
  select_if(is.numeric)

mcor <- cor(df_num)

# Corr plot
corrplot(mcor, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7)
# correla��o positiva, como esperado
# Covariancia
print(cov(group2$idade,
          group2$renda_anual, method = "spearman")) 

# Idade x Tempo de fidalidade
ggplot(group2, 
       aes(x = idade, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# aparentemente quanto mais velho, menos tempo como membro

# Renda x Tempo de fidelidade
ggplot(group2, 
       aes(x = renda_anual, 
           y = membro_desde)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# uma correla��o negativa entre a renda e o empo de fidelidade

#####
group3 <- df_Clientes %>%
  filter(membro_desde > '2017-12-31')
#####
# Estatisticas descritivas
summary(group3)
str(group3)

# Identifiquei uma quantidade anormal de idades acima dos 90 anos
group3 %>% filter_all(any_vars(. %in% c(101))) # nenhum

group3 %>% filter(idade > 90) # 47

#### An�lise univariada
### Vari�veis categ�ricas
# Genero
# bar plot das idades
a = table(group3$genero)
barplot(a, main="Generos dos clientes",
        ylab="Contagem",
        xlab="Genero",
        col = c('blue', 'red', 'yellow'))

# pie chart do genero
group3 %>% 
  pull(genero) %>%
  table() %>%
  pie(col = hcl.colors(length(table(group3$genero)),
                       "BluYl"))

## Vari�veis Num�ricas 
# Idade
# boxplot da Idade dos clientes
boxplot(group3$idade, horizontal = TRUE,
        col = 'purple', main = 'Idade dos clientes')
# a maioria dos clientes est� entre 40 e poucos e 60 e poucos anos

# Histograma das idades dos clientes
hist(group3$idade,
     col= "blue",
     main="Histograma das idades dos clientes",
     xlab="Idade",
     ylab="Frequencia",
     labels=TRUE) 
# a mesma distribui��o do grupo completo e do grupo1

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
# Avaliando as idades de acordo com o genero
ggplot(group3) + 
  aes(idade, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Idade de acordo com o genero')
# h� uma quantidade maior de homens

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

# density plot
ggplot(group3)+
  aes(renda_anual, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Renda anual de acordo com o genero')
# mesma tendencia

## genero x tempo como membro
# Bar plot
ggplot(group3) + 
  aes(membro_desde, fill = genero) + 
  geom_bar() + 
  theme_light() +
  labs(title = 'Tempo de fidelidade de acordo com o genero')

# density plot
ggplot(group3)+
  aes(membro_desde, fill = genero)+ 
  geom_density(position = 'identity', alpha = 0.5)+
  theme_light()+
  labs(title = 'Tempo de fidelidade de acordo com o genero')


## Idade x Renda
ggplot(group3, 
       aes(x = idade, 
           y = renda_anual)) +
  geom_point() +
  geom_point(color= "indianred3") +
  geom_smooth(method = "lm")
# Repete o padr�o dos outros anos


# analisando a correla��o entre renda e idade
df_num <- group3 %>%
  select_if(is.numeric)

mcor <- cor(df_num)

# Corr plot
corrplot(mcor, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7)
# correla��o positiva, como esperado
# Covariancia
print(cov(group3$idade,
          group3$renda_anual, method = "spearman")) 

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
# uma correla��o negativa entre a renda e o empo de fidelidade
#####

# Essa an�lise refor�a a tese que a segementa��o esta relacionada a idade e renda dos clientes