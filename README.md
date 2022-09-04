# Identificando Segmentos de Clientes
Neste projeto foi realizada a identificação de diferentes segmentos de clientes de um restaurante fictício. O objetivo do projeto é realizar a recomendação de segmentação da base de clientes, e com base nos resultados sugerir estratégias de venda e promoção adequados para cada segmento identificado. O projeto foi realizado utilizando a liguagem R.

## Sobre os dados

A Tadayomi é uma famosa temakeria lacalizada em Laranjeiras, e é conhecida por seus temakis de alta qualidade com foco no público de classe B +. Quando um cliente vai à Tadayomi, ele é incentivado a preencher seu cpf, email e telefone para receber promoções e novidades. Com esses dados, a temakeria conseguiu montar uma base de clientes robusta. Essa base é formada por três tabelas:

#### Portfólio Ofertas
Tabela informativa. Contém as descrições das ofertas que são enviadas aos clientes. Possui as seguintes colunas:
* ID: código de identificação da oferta
* Oferta: Tipo de oferta enviada, podendo ser informativo, desconto e compre 1 e leve 2
* Canal: canal de envio da comunicação podendo ser email, web, celular e redes sociais

#### Dados_clientes
Dados demográficos dos clientes. Possui as seguintes colunas:
* Gênero: gênero do cliente
* Idade: idade dos clientes
* Id: código único de cada cliente
* Membro Desde: data em que o cliente criou uma conta
* Renda Anual: renda anual fornecida pelo cliente.

A tabela dos clientes têm o seguinte formato:

![image](https://user-images.githubusercontent.com/77032413/188335162-b35978b0-81a0-4b09-aa46-c94588d3144c.png)

#### Eventos_ofertas
Base com registros de todos os eventos. Suas colunas são:
* Cliente: código do cliente que executou aquele evento
* Valor: contém informação do valor gasto na compra
* id_oferta: código da oferta

A tabela de ofertas tem o seguinte formato:

![image](https://user-images.githubusercontent.com/77032413/188335332-c3189643-9ccf-4cc1-9caa-333d0db03e83.png)

## Análise Exploratória dos dados
#### Analisando os Dados dos Clientes 
Primeiro, foquei em analisar os dados preenchidos pelos clientes ao realizarem os cadastros. Os resultados das estatísticas descritivas são os que seguem:

![image](https://user-images.githubusercontent.com/77032413/188335138-b2a15d9e-f76f-4d0c-afbf-cce2a0b8a9ad.png)

 
Através da análise exploratória foi possível identificar alguns dados interessantes sobre os clientes:

* A maior parte da amostra é constituída por homens, sendo 8484 registros para homens contra 6129 para mulheres. Além disso, haviam 212 registros como “outro”

![image](https://user-images.githubusercontent.com/77032413/187936195-4f196a06-002e-47e5-9d8a-3f7e6a9f5c57.png)

* A renda anual média dos clientes está entre R$ 40.000 e 80.000 ao ano

![image](https://user-images.githubusercontent.com/77032413/187936447-b6aaf2b6-8b00-40f0-a0f3-eed3b99269fd.png)

* Além dos fatos mencionados anteriormente, também foi possível observar que a maior parte dos clientes se registrou a partir de 2018. O gráfico abaixo demonstra esse fenômeno: Cada ponto representa um cliente, quanto mais pontos vermelhos, mais registros. Ao analisar o período de cadatro no eixo vertical é possível perceber que eles se concentram no ano de 2016 em diante. Já o eixo horizontal mostra que os registros de pessoas mais velhas foram feitos em sua maioria a partir de 2016.

![image](https://user-images.githubusercontent.com/77032413/187940548-77023c5c-0e82-4ead-b432-aa1a9d05d415.png)

Entretanto, este grafico informa outro fato importante. Ao analisar as idades registradas pelos clientes(no eixo horizontal do gráfico acima), pude observar um comportamento anormal nos registros: Havia uma quantidade muito alta de clientes que informaram idades muito avançadas. Além disso, a maior parte dos registros feitos por pessoas acima dos 100 anos foram feitos a partir de 2016. 

Resolvi então investigar esse fenômeno mais a fundo. Para isso, separei os dados em grupos de acordo com a faixa etária pra ver se encontrava resultados similares. Essa decisão foi tomara levando em consideração o padrão de ocorrencia de idade e renda dos clientes. O scatter plot abaixo mostra que há um "teto" para os clientes de até 35 anos de cerca de R$ 75.000,00 por ano, depois o mesmo padrão para R$ 100.000,00 e até 50 anos de idade. Além disso a linha azul demonstra uma correlação positiva entre as duas variáveis. 

![image](https://user-images.githubusercontent.com/77032413/187940101-44029c1b-0390-458d-8292-02779215d39b.png)

Outro fator que chamou atenção foi a presença de clientes que informavam idades superiores a 100 anos de idade no momento do cadastro. Decidi separar os clientes mais velhos da amostra (acima de 90 anos) em um grupo e analisar suas estatísticas. Além de perceber que para essa faixa etária haviam mais mulheres do que homens, pude observar uma quantidade anormal de clientes com idade muito avançada e rendimentos muito altos, conforme mostram os gráficos abaixo.

![image](https://user-images.githubusercontent.com/77032413/187941343-d7ccdb3e-078c-4435-bf9e-4a397d0234eb.png)![image](https://user-images.githubusercontent.com/77032413/187941738-77071289-5a38-4e65-8d6e-a3635d480268.png)

Apesar da temakeria ter um enfoque em um publico com um poder aquisitivo mais elevado, essa quantidade alta de pessoas com idade avançada e rendimentos elevados é inconsistente. Esses fatos corroboram a hipótese que os dados de clientes podem estar enviesados: alguns clientes podem não ter preenchido corretamente de forma acidental ou deliberada.

#### Registros de ofertas
Uma vez analisada a tabela de clientes, voltei a análise para os registros de ofertas realizados pela empresa. Aqui o foco foi averiguar quais eram as promoções mais utilizadas e quais os canais mais comuns em que os clientes receberam as ofertas. Após realizar uma etapa de limpeza dos dados, parti para analisar as estatísticas descritivas da tabela:

![image](https://user-images.githubusercontent.com/77032413/188335219-bc897d6c-4524-449a-a9b1-debf27062b21.png)

Os principais resultados encontrados nesta etapa foram os seguintes:

* A oferta mais utilizada era a de “compre 1, leve 2’, seguida por promoções de descontos.

![image](https://user-images.githubusercontent.com/77032413/187942718-3894a3db-70c3-4bf7-bd2b-63d9097195e1.png)

* Além disso, a maior parte das ofertas foi aproveitada quando disparada por todos os canais disponíveis: web, mobile, social e email. Conforme mostra o grafico de pizza abaixo, a quantidade de compras utilizando cupons disparados por todos os canais simultaneamente foi maior que as disparadas por menos canais.

![image](https://user-images.githubusercontent.com/77032413/187942954-f25789f7-e0ca-485e-ad5e-b6466dd1f5f8.png)

## Segmentação de Clientes

Através da análise dos registros foi possível identificar quais eram os tipos de oferta mais utlizados pelos clientes, além de averiguar por onde eles recebiam essas ofertas.	Também foi possível identificar a existência de um viés nos registros de novos clientes. Por isso, para identificar os diferentes segmentos de clientes, foram usados somente os dados cujo input não vinha diretamente dos cliente, e sim gerados automaticamente quando as compras eram feitas. São eles: Número de compras feito por cada cliente, valor total gasto por cada cliente e tempo registrado (em dias).

A partir dos dados escolhidos, foi realizada a segmentação dos clientes através do método RFM (Recency, Frequency, Monetary), que usa as variáveis mencionadas anteriormente para gerar novas variáveis (ou features) que representam os diferentes grupos de consumidores com padrões específicos de consumo. Com o RFM, é possível agrupar os segmentos de clientes de acordo com seus padrões de gasto e frequencia de gastos. Esses dados são convertidos em um score, que avalia os diferentes atributos considerados e dá uma nota para cada um dos clientes de 111 a 554.

Uma vez devidamente separarados, é hora de atribuir nomes aos grupos identificados. Os grupos foram rotulados com as seguintes categorias: 
* Clientes Leais: Aqueles clientes com pontuação mais alta no RFM, que compram varias vezes com as ofertas oferecidas e gastam muito e deveriam ser recompensados com ofertas 
* Promisores: Clientes que ainda não compraram o suficiente para serem considerados leais mas parecem estar a caminho disso
* Hesitantes: Clientes que já realizaram compras com as ofertas, mas em um volume menor e gastando menos
* Necessita Atenção: Clientes que compraram poucas vezes com as ofertas oferecidas e que potencialmente podem parar de comprar na temakeria
* Oferecer Mais Promoções: Clientes que compraram poucas vezes, gastaram pouco e tem poucas chances de utilizar das promoções de novo. Por isso precisam ser oferecidos mais ofertas

A distribuição dos segmentos ficou da seguinte: 

![image](https://user-images.githubusercontent.com/77032413/187944660-0e3824f2-1a6c-45f9-90c0-0aee4f63302c.png)

É possível observar o elevado número de clientes que necessitam de atenção e que precisam ser oferecidos mais promoções para não deixarem de comprar na Tadayomi. Entretanto, o alto número de clientes promisores é animador. Tendo em vista os números dos segmentos encontrados, e também os canais mais utilizados para acessar as ofertas, as recomendações são as seguintes:

* Clientes Leais: Promoções de desconto como bonificação pela fidelidade. Disparo por web, email e mobile.
* Promissores: Promoções de desconto e compre 1 leve 2 para incentivar o consumo. Por ser um grupo numeroso, recomenda-se o disparo por web, email, mobile e social para aumentar as chances de incrementar as vendas.
* Hesitantes: Promoção compre 1, leve 2. Disparo por email, mobile e social.
* Necessita Atenção: Promoções de desconto e compre 1, leve 2 para demonstrar atenciosidade por parte da Tadayomi. Por ser um grupo numeroso, recomenda-se o disparo por web, email, mobile e social para aumentar as chances de incrementar as vendas.
* Oferecer mais promoções: Promoções de desconto e compre 1, leve 2 para demonstrar atenciosidade por parte da Tadayomi. Por ser um grupo numeroso, recomenda-se o disparo por web, email, mobile e social para aumentar as chances de incrementar as vendas.
