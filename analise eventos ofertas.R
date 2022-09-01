### Analisando as ofertas ###
# Pacotes e importando os dados 
######
# Pacotes
library(readr)
library(tidyverse)
library(lubridate)
library(corrplot)
library(psych)
library(cowplot)

# Importando os dados
eventos_ofertas <- read_csv("C:/Users/Lucas/Desktop/eventos_ofertas.csv")
portfolio_ofertas <- read_csv("C:/Users/Lucas/Desktop/portfolio_ofertas - portfolio_ofertas.csv")

#######

# Eventos de ofertas - dados gerais
#####
summary(eventos_ofertas)

# Checando os NA's
lapply(eventos_ofertas, 
       function(x) { length(which(is.na(x)))})
# muitos valores faltando com valor, id_oferta e recompensa

# Fatorando as variáveis da oferta
df_ofertas <- eventos_ofertas

df_ofertas$id_oferta <- factor(df_ofertas$id_oferta,
                               levels = c('ae264e3637204a6fb9bb56bc8210ddfd',
                                          '4d5c57ea9a6940dd891ad53e9dbe8da0',
                                          '3f207df678b143eea3cee63160fa8bed',
                                          '9b98b8c7a33c4b65b9aebfe6a799e6d9',
                                          '0b1e1539f2cc45b7b9fa7c272da2e1d7',
                                          '2298d6c36e964ae4a3e7e9706d1fb8c2',
                                          'fafdcd668e3743c1bb461111dcafc2a4',
                                          '5a8bc65990b245e5a138643cd4eb9837',
                                          'f19421c1d4aa40978ebb69ca19b0e20d',
                                          '2906b810c7d4411798c6938adc9daaa5'),
                               labels = c('compre 1, leve 2',
                                          'compre 1, leve 2',
                                          'informativo',
                                          'compre 1, leve 2',
                                          'desconto',
                                          'desconto',
                                          'desconto',
                                          'informativo',
                                          'compre 1, leve 2',
                                          'desconto'))

df_ofertas$tipo_evento <- factor(df_ofertas$tipo_evento,
                                 levels = c("oferta recebida",
                                            "oferta visualizada",
                                            "transacao",
                                            "oferta conclu\xedda"),
                                 labels = c('oferta recebida',
                                            'oferta visualizada',
                                            'transação',
                                            'oferta concluída'))

# Tirando a coluna index
df_ofertas <- df_ofertas %>%
  select(-1)

summary(df_ofertas)

#### Análise univariada
### Variáveis categóricas
# Tipo de oferta
# bar plot das ofertas
a = table(df_ofertas$id_oferta)
barplot(a, main="Tipos de oferta utilizados",
        ylab="Contagem",
        xlab="Tipo de oferta",
        col = c('blue', 'red', 'yellow'))

# A maior parte foi promoções de desconto e compre1, leve 2

# tipo de evento
# pie chart
df_ofertas %>% 
  pull(tipo_evento) %>%
  table() %>%
  pie(col = hcl.colors(length(table(df_ofertas$tipo_evento)),
                       "BluYl"))
# A maior parte dos registros estpa relacionada a transações
# Quase não hpa registro de ofertas concluídas

## Variáveis Numéricas 
# Valor pago
# Histogramas
hist(df_ofertas$valor,
     col= "blue",
     main="Valores pagos nas transações",
     xlab="Valor pago (em R$)",
     ylab="Frequencia",
     labels=TRUE) 
# evidentemente a maior parte dos registros foi de transações baixas. 
# entretanto alguns valores muito altos chamam atenção

# Recompensa
# boxplot 
boxplot(df_ofertas$recompensa, horizontal = TRUE,
        col = 'purple', main = 'Recompensas obtidas por promoção')

# Histogramas
hist(df_ofertas$recompensa,
     col= "blue",
     main="Recompensas utilizadas nas transações",
     xlab="Tipo de recompensa",
     ylab="Frequencia",
     labels=TRUE) 
# Recompensa mais registrada: 5, 2, 3 e 10

# Tempo decorrido 
boxplot(df_ofertas$tempo_decorrido, horizontal = TRUE,
        col = 'purple', main = 'Tempo para as transações')

#####

# Feature engineering - Avaliando os canais de oferta e sua duração
#####
ofertas <- eventos_ofertas

ofertas$canal <- factor(ofertas$id_oferta,
                        levels = c('ae264e3637204a6fb9bb56bc8210ddfd',
                                   '4d5c57ea9a6940dd891ad53e9dbe8da0',
                                   '3f207df678b143eea3cee63160fa8bed',
                                   '9b98b8c7a33c4b65b9aebfe6a799e6d9',
                                   '0b1e1539f2cc45b7b9fa7c272da2e1d7',
                                   '2298d6c36e964ae4a3e7e9706d1fb8c2',
                                   'fafdcd668e3743c1bb461111dcafc2a4',
                                   '5a8bc65990b245e5a138643cd4eb9837',
                                   'f19421c1d4aa40978ebb69ca19b0e20d',
                                   '2906b810c7d4411798c6938adc9daaa5'),
                        labels = c('email, mobile, social',
                                   'web, email, mobile, social',
                                   'web, email, mobile',
                                   'web, email, mobile',
                                   'web, email',
                                   'web, email, mobile, social',
                                   'web, email, mobile, social',
                                   'email, mobile, social',
                                   'web, email, mobile, social',
                                   'web, email, mobile'))

# Canais mais utilizados pras ofertas
ofertas %>% 
  pull(canal) %>%
  table() %>%
  pie(col = hcl.colors(length(table(ofertas$canal)),
                       "BluYl"))

# A maior parte das ofertas foi aproveitada quando disparada por web, mobile, social e email

# Duração das promoções utilizadas
# Histogramas
hist(as.numeric(ofertas$duracao),
     col= "blue",
     main="Duração das promoções mais utilizadas",
     xlab="Dias de promoção",
     ylab="Frequencia",
     labels=TRUE) 
# A maior parte das promoções utilizadas durava apenas um dia

# Histograma das valores minimos pagos pelos clientes
hist(as.numeric(ofertas$val_minimo),
     col= "blue",
     main="Valor minimo pago nas promoções pelos clientes",
     xlab="Valor (em R$)",
     ylab="Frequencia",
     labels=TRUE) 
# A maior parte das promoções usadas não exigia valor minimo
#####
