print("Hello World")

##### Instalando os pacotes necessários #####

  install.packages("dplyr")
library(dplyr)
  install.packages('tidyverse')
library(tidyverse)


##### Chamando Arquivo .csv #####

pelican <- read.csv("C:/Users/lucfe/Documents/01_ESTUDOS/Pos_Graduação/03_Modulo_3/BD_AE/Trilha01/Estudo_de_caso_1/PelicanStores.csv", encoding = "UTF-8")

##### Lendo o Arquivo chamado #####

View(pelican)

#### Criando tabela de dados ####
  
  dicionario <- data.frame(
    "NOME_VARIAVEL"=c("Cliente","Tipo de Cliente","Itens","Vendas líquidas","Método de Pagamento","Gênero","Estado Civil","Idade"),
    "DESCRICAO"=c("Identificação única do cliente","Tipo de compra realizada","Quantidade de itens","Valor da compra","Tipo de pagamento",
                  "Gênero do cliente","Estado civil do cliente","Idade do cliente"),
    "TIPO_VARIAVEL"=c("Qualitativa","Qualitativa","Quantitativa","Quantitativa","Qualitativa","Qualitativa","Qualitativa","Quantitativa"),
    "TIPO_MENSURACAO"=c("Nominal","Nominal","Discreta","Contínua","Nominal","Nominal","Nominal","Discreta"),
    "VALORES_POSSIVEIS"=c("Números inteiros positivos","Promocional ou Regular","Números inteiros positivos",
                          "Números reais positivos","American Express, Cartão Proprietário, Discover, MasterCard ou Visa",
                          "Feminino, Masculino e Não Informado","Casado ou Solteiro","Números inteiros positivos")
  )
  
  dicionario
view(dicionario)

###### Número total de itens comprados #####

itens_total = sum(pelican$Itens)
itens_total

###### Quantia total comprada pelo cartão de crédito #####

pelican_2 = pelican[, c("Vendas.líquidas","Método.de.Pagamento")]
head(pelican_2)

vendas_liquidas_num <- as.numeric(sub(",", ".", pelican_2$`Vendas.líquidas`))

pelican_2$`Vendas.líquidas` = vendas_liquidas_num

str(pelican_2)

sum(pelican_2$Vendas.líquidas)

pelican_3 <- pelican_2 %>% 
  filter(str_detect(pelican_2$Método.de.Pagamento, "Cartão Proprietário")
         )

sum(pelican_3$Vendas.líquidas)


##### Estatística ####

#### Variável "ITENS", determinando Quartis, percentis, mádias, medianas, desvio padrão, variancia e moda ####

var_itens = pelican$Itens
var_itens

print(summary(var_itens))#Quartis
print(quantile(var_itens))
print("Média:") 
mean (var_itens) #Média
print("Mediana:") 
median(var_itens) #Mediana
print("Desvio Padrão:") 
sd(var_itens) #Desvio Padrão
print("Variância") 
var(var_itens) #Variância

funcao <- function(v_itens) {
  uniqv <- unique(v_itens)
  uniqv[which.max(tabulate(match(v_itens, uniqv)))]
}

# Create the vector with numbers.
v_itens <- var_itens

# Calculate the mode using the user function.
moda <- funcao(v_itens)
print(moda)

#### Variável "Vendas.líquidas", determinando Quartis, percentis, mádias, medianas, desvio padrão, variancia e moda ####


#var_vendas_liquidas_str = pelican$Vendas.líquidas
var_vendas_liquidas <- as.numeric(sub(",", ".", pelican$`Vendas.líquidas`))


print(summary(var_vendas_liquidas))#Quartis
print(quantile(var_vendas_liquidas))
print("Média:") 
mean (var_vendas_liquidas) #Média
print("Mediana:") 
median(var_vendas_liquidas) #Mediana
print("Desvio Padrão:") 
sd(var_vendas_liquidas) #Desvio Padrão
print("Variância") 
var(var_vendas_liquidas) #Variância

funcao <- function(v_vendas_liquidas) {
  uniqv <- unique(v_vendas_liquidas)
  uniqv[which.max(tabulate(match(v_vendas_liquidas, uniqv)))]
}

# Create the vector with numbers.
v_vendas_liquidas <- var_vendas_liquidas

# Calculate the mode using the user function.
moda <- funcao(v_vendas_liquidas)
print(moda)

#### Variável "Idade", determinando Quartis, percentis, mádias, medianas, desvio padrão, variancia e moda ####


var_idade = pelican$Idade



print(summary(var_idade))#Quartis
print(quantile(var_idade))
print("Média:") 
mean (var_idade) #Média
print("Mediana:") 
median(var_idade) #Mediana
print("Desvio Padrão:") 
sd(var_idade) #Desvio Padrão
print("Variância") 
var(var_idade) #Variância

funcao <- function(v_idade) {
  uniqv <- unique(v_idade)
  uniqv[which.max(tabulate(match(v_idade, uniqv)))]
}

# Create the vector with numbers.
v_idade <- var_idade

# Calculate the mode using the user function.
moda <- funcao(v_idade)
print(moda)

# nolint
