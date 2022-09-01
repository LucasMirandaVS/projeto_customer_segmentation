# Pacotes e importando os dados 
#####
# Pacotes
library(readr)
library(tidyverse)
library(lubridate)

# Importando os dados
dados_clientes <- read_csv("C:/Users/Lucas/Desktop/dados_clientes.csv")
eventos_ofertas <- read_csv("C:/Users/Lucas/Desktop/eventos_ofertas.csv")
#####

## Data Preparation
## Clientes
#####
#definindo um df para os clientes a partir do original
clientes <- dados_clientes %>%
  select(c(genero, idade, id, membro_desde, renda_anual))

# conferindo e excluindo os dados faltantes (NA's)
colSums(is.na(clientes))
clientes <-na.omit(clientes)

# Definindo a variavel de data como date
clientes$membro_desde <- ymd(clientes$membro_desde)

#####

## Ofertas
# Preparando o df ofertas
#####
ofertas <- eventos_ofertas %>%
  select(-1, -7)

ofertas$tipo_oferta <- factor(ofertas$id_oferta,
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


ofertas$duracao <- factor(ofertas$id_oferta,
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
                          labels = c(7, 5, 4, 7, 10, 7, 10, 3, 5, 7))


ofertas$val_minimo <- factor(ofertas$id_oferta,
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
                             labels = c(10, 10, 0, 5, 20, 7, 10, 0, 5, 10))
#####

## Aplicando o RFM
#####
# Retirando os na's e as colunas valor e recompensa
Supply <- ofertas %>%
  filter(is.na(valor)) %>% 
  arrange(cliente) %>%
  select(-3, -5)

# Somente o valor
Supply2 <- ofertas %>%
  filter(!is.na(valor)) %>% 
  arrange(cliente)


# Creation of Recency, Frequency y Monetary 
# Frequencia das compras
frequencia <- aggregate(Supply[,1], list(Supply$cliente), length)
names(frequencia) <- c('idCliente', 'Compras')

freq2 <- aggregate(Supply2[,1], list(Supply2$cliente), length)
names(freq2) <- c('idCliente', 'Compras')

# Valor pago nas compras
monetary <- aggregate(Supply2[,3],list(Supply2$cliente), sum)
names(monetary) <- c('idCliente', 'Valor')

# Recencia
analysis_date <- lubridate::as_date("2022-08-19")

recencia <- clientes %>%
  group_by(id) %>%
  summarise(Dias_Membro = as.numeric(analysis_date - membro_desde))
names(recencia) <- c('idCliente', 'Dias_Membro')

# Unindo
rfm_df <- merge(frequencia, monetary, by = "idCliente")
rfm_df <- merge(rfm_df, recencia, by = 'idCliente')
summary(rfm_df)

#  Criando o rank RFM
rfm_df$Rank_Recencia <- cut(rfm_df$Dias_Membro, 5, labels = F)
rfm_df$Rank_Frequencia <- cut(rfm_df$Compras, 5, labels = F)
rfm_df$Rank_Monetario <- cut(rfm_df$Valor, 5, labels = F)

# Analise
groupRFM <- count(rfm_df, Rank_Recencia, Rank_Frequencia, Rank_Monetario)
groupRFM <- rfm_df$Rank_Recencia*100 + rfm_df$Rank_Recencia*10 +
  rfm_df$Rank_Monetario
rfm_df <- cbind(rfm_df, groupRFM)

# Plot
ggplot(rfm_df, aes(factor(groupRFM))) +
  geom_bar() +
  ggtitle('Segmentos de consumidores por RFM') +
  labs(x="rank RFM",y="Clientes") + 
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold"))
# Apontou 5 segmentos distintos

# Classificando os segmentos
rfm_df$Segment <- '0'

rfm_df$Segment[which(rfm_df$groupRFM %in% c(554,544, 543, 445, 552, 551, 544, 542, 541, 535, 534, 533, 532, 531, 525, 524, 523, 522, 521,444,434,443, 344, 442, 244, 424, 441  ))] <- "Clientes Leais"
rfm_df$Segment[which(rfm_df$groupRFM %in% c(332,333,342, 343, 334, 412,413,414,431,432,441,421,422,423, 424, 433 ,233,234, 241,311, 312, 313,314,321,322,323,324, 331,  341))] <- "Promissores"
rfm_df$Segment[which(rfm_df$groupRFM %in% c(124, 133, 134, 142, 143, 144, 214,224,234, 242, 243, 232 ))] <- "Hesitantes"
rfm_df$Segment[which(rfm_df$groupRFM %in% c(122, 123,131 ,132, 141, 212, 213, 221, 222, 223, 231 ))] <- "Necessita atenção"
rfm_df$Segment[which(rfm_df$groupRFM %in% c(111, 112, 113, 114, 121, 131, 211, 311, 411, 511 ))] <-"Oferecer mais promoções"

# Visualizando os segmentos
table(rfm_df$Segment)

rfm_df <- rfm_df %>%
  filter(Segment != '0') 

ggplot(rfm_df) + geom_bar(aes(x = Segment, fill = Segment))+theme(axis.text.x=element_text(angle=90,hjust=1)) +
labs(title = "Segmentos de consumidores")


#####
