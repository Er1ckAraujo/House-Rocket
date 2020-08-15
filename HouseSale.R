# Esse projeto foi proposto no artigo: https://sejaumdatascientist.com/os-5-projetos-de-data-science-que-fara-o-recrutador-olhar-para-voce/

install.packages(c("tidyverse", "magrittr", "esquisse", "GGally", "devtools",
                   "libridate", "patchwork", "DT"))

library(dplyr)
library(magrittr)
library(lubridate)

HouseData <- kc_house_data %>% # Mudando categoria das variáveis e salvando em um novo objeto
  mutate(id = as.factor(kc_house_data$id),
         date = ymd(HouseData$date),
         bedrooms = as.factor(kc_house_data$bedrooms),
         bathrooms = as.factor(kc_house_data$bathrooms),
         floors = as.factor(kc_house_data$floors),
         waterfront = as.factor(kc_house_data$waterfront),
         view = as.factor(kc_house_data$view),
         condition = as.factor(kc_house_data$condition),
         grade = as.factor(kc_house_data$grade),
         yr_renovated = as.factor((kc_house_data$yr_renovated)),
         zipcode = as.factor(kc_house_data$zipcode))

head(HouseData)
summary(HouseData)
#---------------------------------------------------------------------------------
# Observações: Data: 1 ano entre 02/05/2014 até 27/05/2015
# Preço: 75000<x<7700000; Média: $540088; 
# Quartos: Quantidades mais comuns de quartos em ordem decrescente: 3, 4, 2, 5, 6, 1.
# Banheiros: A nota .5 é Atribuida a banheiros que não possuem área para banho.
# é mais comum encontrar casa com 2 banheiros completos e um sem aérea de banho.
# é comum também encontrar casas com apenas um banheiro completo.
# sqft_living: Tamanho da propriedades em m² 290<x<13540. Média: 2080.
# sqft_lot: Tamanho do lote da propriedade em m² 520<x<1651359. Média:15107.
# floors: A maior parte das casas possuem 1 Andar, seguidas de 1.5, 2.
# Waterfront: 21450 casas não possuem vista pro mar, 163 sim.
# View: Em uma escala de  0 a 4, 19489 casas receberam nota 0 no quesito vista.
# 936 receberam nota 2, e 319 receberam nota 4.
# condition: Em um índice de 1 a 5, 30 apartamentos receberam nota 1, 14031 
# receberam nota 3, 1701 receberam nota 5.
# grade: Um índice de 1 a 13, onde 1-3 fica aquém da construção e design 
# dos edifícios, 7 tem um nível médio de construção e design e 11-13 têm
# um alto nível de qualidade de construção e design. 8981 imoveis receberam nota 7.
# 6068 nota 8, a maior parte dos apartamentos estão em um estado médio pra bom.
# sqfft_above: Metragem quadrada da habitação acima do nível do solo. 290<x<9410, média:1788.
# sqft_basement: Metragem quadrada do porão, 0<x<4820. Média: 291.
# yr_built: Ano de construção 1900<x<2015. Média: 1971.
# sqft_living15: A metragem quadrada do espaço habitacional para os 15 vizinhos mais próximos.
# 399<x<6210. média: 1987.
# sqft_lot15: A metragem quadrada do espaço habitacional para os 15 vizinhos mais próximos.
# 651<x<971200. média: 12768.
#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
# Observações: Testando correlações das variáveis numéricas
#---------------------------------------------------------------------------------

  library(GGally)
ggpairs(HouseData, columns = c(3,6, 7, 13, 14))
  
#---------------------------------------------------------------------------------
# Observações:
# Maiores correlações: Price vs Living(0.702); Living vs Above(0.877);
# Above vs Price(0.606)
# O Tamanho da casa influencia fortemente seu preço.
#---------------------------------------------------------------------------------

# PERGUNTA 1
#---------------------------------------------------------------------------------
# Testando correlações das variáveis categóricas
#---------------------------------------------------------------------------------
# Price vs bedrooms
library(ggplot2)

ggplot(HouseData) +
 aes(x = bedrooms, y = price) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

summary(HouseData$bedrooms)
library(dplyr)
library(magrittr)

# Filtrando os mais baratos

PriceVsBedrooms <- HouseData %>%
  select(price, bedrooms) %>% 
  filter(bedrooms %in% c("0", "1", "2", "3"))

mean(PriceVsBedrooms$price)
  

#---------------------------------------------------------------------------------
# Observações: 
# Hipoteses: imoveis com mais quartos são mais caros? A Variância é pequena?
# R: Não necessariamente. No bloxpot podemos ver que os maiores preços estão em casas
# com 5 e 6 quartos. 
# Imóveis com 8 quartos possuem uma dispersão maior.
#---------------------------------------------------------------------------------

library(ggplot2)
library(magrittr)

ggplot(HouseData) +
 aes(x = bathrooms, y = price) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

PriceVsBathrooms <- HouseData %>%
  select(price, bathrooms) %>%
  filter(bathrooms %in% c("0", "0.5", "0.75", "1", "1.25", "1.5", "1.75", "2",
         "2.25", "2.5"))

mean(PriceVsBathrooms$price)
#---------------------------------------------------------------------------------
# Observações: O preço médio das casas aumentam a medida que a quantidade 
# de banheiros aumenta.
#---------------------------------------------------------------------------------

library(ggplot2)
library(magrittr)

ggplot(HouseData) +
 aes(x = floors, y = price) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

PriceVsFloors <- HouseData %>%
  select(price, floors) %>%
  filter(floors %in% "1")

mean(PriceVsFloors$price)
#---------------------------------------------------------------------------------
# Observações: o preço medio dos imóveis possui um pequeno aumento nos imóveis
# com 2,5 andares. 
#---------------------------------------------------------------------------------

library(ggplot2)
library(magrittr)

ggplot(HouseData) +
 aes(x = waterfront, y = price) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

# Casas sem vista pro mar

PriceVsWaterfront1 <- HouseData %>%
  select(price, waterfront) %>%
  filter(waterfront %in% "0")

mean(PriceVsWaterfront1$price)

# Casas com vista pro mar

PriceVsWaterfront2 <- HouseData %>%
  select(price, waterfront) %>%
  filter(waterfront %in% "1")

mean(PriceVsWaterfront2$price)

# Aumento percentural entre casas sem vista vs com vista

((mean(PriceVsWaterfront2$price)/mean(PriceVsWaterfront1$price))-1)*100

#---------------------------------------------------------------------------------
# Observações: Casas com vista pro mar, em média são mais valiosas. Por outro lado, 
# casas que não possuem vista pro mar possuem outliers mais caros.
#---------------------------------------------------------------------------------

library(ggplot2)
library(magrittr)

ggplot(HouseData) +
 aes(x = view, y = price) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

PriceVsView <-  HouseData %>%
  select(price, view) %>%
  filter(view %in% c("2", "3"))

mean(PriceVsView$price)
#---------------------------------------------------------------------------------
# Observações: imoveis com maior nota em vista são em média mais caros
#---------------------------------------------------------------------------------

library(ggplot2)
library(magrittr)

ggplot(HouseData) +
 aes(x = condition, y = price) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

PriceVsCondition <- HouseData %>%
  select(price, condition) %>%
  filter(condition %in% c("3","4"))

mean(PriceVsCondition$price)
#---------------------------------------------------------------------------------
# Observações: A condição dos apartamentos não parece ser um fator muito definitivo
# na sua valorização. já que a média dos valores entre as notas não varia muito.
#---------------------------------------------------------------------------------

library(ggplot2)
library(magrittr)
ggplot(HouseData) +
 aes(x = grade, y = price) +
 geom_boxplot(fill = "#0c4c8a") +
 theme_minimal()

PriceVsGrade <- HouseData %>%
  select(price, grade) %>%
  filter(grade %in% c("1","2","3","4","5","6","7","8"))

mean(PriceVsGrade$price)

# Obtendo média de cada fator

PriceVsGrade2 <- HouseData %>%
  select(price, grade) %>%
  filter(grade %in% c("9", "10", "11", "12", "13"))

mean(PriceVsGrade2$price)


# Filtrando nosso perfil de casas para compra

library(magrittr)
library(dplyr)

PerfilCompra <- HouseData %>%
  filter(bedrooms %in% c("0", "1", "2", "3"),
         bathrooms %in% c("0", "0.5", "0.75", "1", "1.25", "1.5", "1.75", "2"),
         floors %in% "1",
         waterfront %in% "0",
         view %in% c("2", "3"),
         condition %in% c("3", "4"),
         grade %in% c("1","2","3","4","5","6","7","8"))

summary(PerfilCompra$price)

#---------------------------------------------------------------------------------
# Observações: Os imóveis com melhor nota possuem os preços mais altos.
#---------------------------------------------------------------------------------

# PERGUNTA 2

# Análise das datas

library(dplyr)
library(magrittr)
library(lubridate)

# Como todo conjunto de dados se passa no intervalo de um ano, vamos extrair os 
# meses e analisa-los como fatores.

HouseData$months <- month(HouseData$date)

HouseData <- HouseData %>%
  mutate(months = as.factor(HouseData$months))

# Analisando a variável months

library(ggplot2)
library(magrittr)

meses <- ggplot(HouseData) +
  aes(x = months) + 
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

meses
#------------------------------------------------------------------------------
# Observações: Um grande volume de casas foi vendido em maio. 
#------------------------------------------------------------------------------

library(ggplot2)

mesesPreco <- ggplot(HouseData) +
  aes(x = months, y = price) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#------------------------------------------------------------------------------
# Observações: As negociações mais valiosas ocorreram em abril, junho, agosto, 
# setembro e Outubro.
#------------------------------------------------------------------------------

library(patchwork)

meses + mesesPreco

# Observando as casas mais valiosas de casa um dos meses citados.

# Abril

library(dplyr)
library(magrittr)

HouseData %>% 
  filter(months == "4") %>%
  summary()

# Propriedade mais cara de Abril foi vendida a $5,350,000 
# Vamos ver que casa é essa

HouseData %>%
  filter(price == "5350000") %>%
  View()

#------------------------------------------------------------------------------
# Observações: Casa com 5 quartos, 5 banheiros, 8 mil m², 2 Andares, 
# Nota 3 em condição, 12 nota geral
#------------------------------------------------------------------------------

# junho

HouseData %>%
  filter(months == "6") %>%
  summary()

# Propriedade mais cara vendida a $ 7,062,500

HouseData %>%
  filter(price == "7062500") %>%
  View()
#------------------------------------------------------------------------------
# Observações: casa com 5 quartos, 4.5 banheiros, 10040 m², 2 andares, nota 3
# em condição, 11 nota geral.
#------------------------------------------------------------------------------

# Agosto

HouseData %>%
  filter(months == "8") %>%
  summary()

# imóvel vendido a 5,570,000

HouseData %>%
  filter(price == "5570000") %>%
  View()

#------------------------------------------------------------------------------
# Observações: 5 quartos, 5.75 banheiros, 9200 m², 2 andares, condição 3, nota 13
#------------------------------------------------------------------------------

# Setembro

HouseData %>%
  filter(months == "9") %>%
  summary()

# Propriedade vendida a 6,885,000

HouseData %>%
  filter(price == "6885000") %>%
  View()

#------------------------------------------------------------------------------
# Observações: 6 quartos, 7.45 banheiros, 9890 m², 2 andares, condição 3,
# nota 13.
#------------------------------------------------------------------------------

# Outubro

HouseData %>%
  filter(months == "10") %>%
  summary()

# Casa negociada a 7,700,000 
# A propriedade mais cara de todo o banco de dados.

HouseData %>%
  filter(price == "7700000") %>%
  View()

#-------------------------------------------------------------------------------
# Observações: 6 quartos, 8 banheiros, 12050 m², 2,5 andares, 4 condição, nota 13
#-------------------------------------------------------------------------------

# Traçar perfil das propriedades mais caras. A partir daí, um perfil de venda.

library(dplyr)
library(magrittr)

HouseData %>%
  filter(bedrooms %in% c("4", "5", "6"),
         bathrooms %in% c("4", "5", "6", "7", "8"),
         sqft_living >= 5000,
         floors %in% c("1.5", "2", "2.5")) %>%
  summary()


#-------------------------------------------------------------------------------
# Observações: Casas com esse perfil possuem preço de venda entre 784,500 e
# 7,700,000. Com média 2,346,534. 
# Comprando imóveis com a média de compra que estimamos, $502,808.0, e vendendo
# pela média de venda estimada, temos uma possibilidade de retorno maior que 300%
# parece um retorno assustador, mas não estamos levando em consideração os gastos
# gerados nessas reformas
#-------------------------------------------------------------------------------

# Retorno percentual
((2346534/502808)-1)*100

# Tabela casas analisadas

library(magrittr)
library(dplyr)
library(DT)

HouseData %>%
  filter(price %in% c("5350000","7062500", "5570000", "6885000", "7700000")) %>%
  select(bedrooms, bathrooms, sqft_living, floors, price, months) %>%
  View()

# PERGUNTA 3
library(dplyr)
library(magrittr)

# Valorização por reforma  

HouseData %>% 
  select(bedrooms, bathrooms, sqft_living, floors, price) %>%
  summary()

# Quantidade de quartos
# 1
HouseData %>%
  select(bedrooms, price) %>%
  filter(bedrooms %in% "1") %>%
  summary()

q1 <-  317643

# 2
HouseData %>%
  select(bedrooms, price) %>%
  filter(bedrooms %in% "2") %>%
  summary()

q2 <-  401373

# 3
HouseData %>%
  select(bedrooms, price) %>%
  filter(bedrooms %in% "3") %>%
  summary()

q3 <-  446232

# 4
HouseData %>%
  select(bedrooms, price) %>%
  filter(bedrooms %in% "4") %>%
  summary()

q4 <-  635420

# 5
HouseData %>%
  select(bedrooms, price) %>%
  filter(bedrooms %in% "5") %>%
  summary()

q5 <-  786600

# 6
HouseData %>%
  select(bedrooms, price) %>%
  filter(bedrooms %in% "6") %>%
  summary()

q6 <- 825521

((((media6/media5) + (media5/media4) + (media4/media3) + (media3/media2) + 
    media2/media1)/5)-1)*100
#  de 1 a 6  quartos cada qaurto aumenta em media 21% o valor do patrimônio total.

# Banheiros de 4 a 8

# 4 
HouseData %>%
  select(bathrooms, price) %>%
  filter(bathrooms %in% "4") %>%
  summary()

b4 <- 1267709 

# 5
HouseData %>%
  select(bathrooms, price) %>%
  filter(bathrooms %in% "5") %>%
  summary()

b5 <- 1673492

# 6
HouseData %>%
  select(bathrooms, price) %>%
  filter(bathrooms %in% "6") %>%
  summary()

b6 <- 2946833

# 8
HouseData %>%
  select(bathrooms, price) %>%
  filter(bathrooms %in% "8") %>%
  summary()

b8 <- 4990000

((((b8/b6)+(b6/b5)+(b5/b4))/3)-1)*100

# andares
# 1
HouseData %>%
  select(floors, price) %>%
  filter(floors %in% "1") %>%
  summary()

a1 <- 442181

# 2
HouseData %>%
  select(floors, price) %>%
  filter(floors %in% "2") %>%
  summary()

a2 <- 648891

(((a2/a1)-1)*100)

# Conclusão: É importante lembrar que as variáveis avaliadas não são independentes.
# portanto o tamanho da casa influencia na quantidade de banheiros, de quartos,
# andares e vice-versa. Contudo, a partir dessa análise é possivel ver
# como cada investimento impacta o valor final do patrimonio.#
