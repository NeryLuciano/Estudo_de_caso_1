print("Hello World")

##### Instalando os pacotes necess�rios #####

  install.packages("dplyr")
library(dplyr)
  install.packages('tidyverse')
library(tidyverse)


##### Chamando Arquivo .csv #####

pelican <- read.csv("C:/Users/lucfe/Documents/01_ESTUDOS/Pos_Gradua��o/03_Modulo_3/BD_AE/Trilha01/Estudo_de_caso_1/PelicanStores.csv", encoding = "UTF-8")

##### Lendo o Arquivo chamado #####

View(pelican)

#### Criando tabela de dados ####
  
  dicionario <- data.frame(
    "NOME_VARIAVEL"=c("Cliente","Tipo de Cliente","Itens","Vendas l�quidas","M�todo de Pagamento","G�nero","Estado Civil","Idade"),
    "DESCRICAO"=c("Identifica��o �nica do cliente","Tipo de compra realizada","Quantidade de itens","Valor da compra","Tipo de pagamento",
                  "G�nero do cliente","Estado civil do cliente","Idade do cliente"),
    "TIPO_VARIAVEL"=c("Qualitativa","Qualitativa","Quantitativa","Quantitativa","Qualitativa","Qualitativa","Qualitativa","Quantitativa"),
    "TIPO_MENSURACAO"=c("Nominal","Nominal","Discreta","Cont�nua","Nominal","Nominal","Nominal","Discreta"),
    "VALORES_POSSIVEIS"=c("N�meros inteiros positivos","Promocional ou Regular","N�meros inteiros positivos",
                          "N�meros reais positivos","American Express, Cart�o Propriet�rio, Discover, MasterCard ou Visa",
                          "Feminino, Masculino e N�o Informado","Casado ou Solteiro","N�meros inteiros positivos")
  )
  
  dicionario
view(dicionario)

###### N�mero total de itens comprados #####

itens_total = sum(pelican$Itens)
itens_total

###### Quantia total comprada pelo cart�o de cr�dito #####

pelican_2 = pelican[, c("Vendas.l�quidas","M�todo.de.Pagamento")]
head(pelican_2)

vendas_liquidas_num <- as.numeric(sub(",", ".", pelican_2$`Vendas.l�quidas`))

pelican_2$`Vendas.l�quidas` = vendas_liquidas_num

str(pelican_2)

sum(pelican_2$Vendas.l�quidas)

pelican_3 <- pelican_2 %>% 
  filter(str_detect(pelican_2$M�todo.de.Pagamento, "Cart�o Propriet�rio")
         )

sum(pelican_3$Vendas.l�quidas)


##### Estat�stica ####

#### Vari�vel "ITENS", determinando Quartis, percentis, m�dias, medianas, desvio padr�o, variancia e moda ####

var_itens = pelican$Itens
var_itens

print(summary(var_itens))#Quartis
print(quantile(var_itens))
print("M�dia:") 
mean (var_itens) #M�dia
print("Mediana:") 
median(var_itens) #Mediana
print("Desvio Padr�o:") 
sd(var_itens) #Desvio Padr�o
print("Vari�ncia") 
var(var_itens) #Vari�ncia

funcao <- function(v_itens) {
  uniqv <- unique(v_itens)
  uniqv[which.max(tabulate(match(v_itens, uniqv)))]
}

# Create the vector with numbers.
v_itens <- var_itens

# Calculate the mode using the user function.
moda <- funcao(v_itens)
print(moda)

#### Vari�vel "Vendas.l�quidas", determinando Quartis, percentis, m�dias, medianas, desvio padr�o, variancia e moda ####


#var_vendas_liquidas_str = pelican$Vendas.l�quidas
var_vendas_liquidas <- as.numeric(sub(",", ".", pelican$`Vendas.l�quidas`))


print(summary(var_vendas_liquidas))#Quartis
print(quantile(var_vendas_liquidas))
print("M�dia:") 
mean (var_vendas_liquidas) #M�dia
print("Mediana:") 
median(var_vendas_liquidas) #Mediana
print("Desvio Padr�o:") 
sd(var_vendas_liquidas) #Desvio Padr�o
print("Vari�ncia") 
var(var_vendas_liquidas) #Vari�ncia

funcao <- function(v_vendas_liquidas) {
  uniqv <- unique(v_vendas_liquidas)
  uniqv[which.max(tabulate(match(v_vendas_liquidas, uniqv)))]
}

# Create the vector with numbers.
v_vendas_liquidas <- var_vendas_liquidas

# Calculate the mode using the user function.
moda <- funcao(v_vendas_liquidas)
print(moda)

#### Vari�vel "Idade", determinando Quartis, percentis, m�dias, medianas, desvio padr�o, variancia e moda ####


var_idade = pelican$Idade



print(summary(var_idade))#Quartis
print(quantile(var_idade))
print("M�dia:") 
mean (var_idade) #M�dia
print("Mediana:") 
median(var_idade) #Mediana
print("Desvio Padr�o:") 
sd(var_idade) #Desvio Padr�o
print("Vari�ncia") 
var(var_idade) #Vari�ncia

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
