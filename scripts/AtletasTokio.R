
# ANALISIS EXPLORATORIO BASICO TOKIO 2020 ---------------------------------


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(funModeling)
library(readxl)

# Datos -------------------------------------------------------------------

atletas <- read_xlsx("data/Athletes.xlsx")
dim(atletas)
str(atletas)
colnames(atletas) <- c("nombre","pais","disciplina")
df_status(atletas)
peru <- atletas %>% filter(pais == "Peru")
dim(peru)
df_status(peru)



# Limpieza de datos -------------------------------------------------------


# Se comprobara si hay datos duplicados en la columna 
# nombre
nrow(atletas)[1]          # hay 11085 registros
df_status(atletas$nombre) # hay 11062 valores unicos
length(unique(atletas$nombre))
# al parecer hay datos repetidos
dup <- atletas$nombre[duplicated(atletas$nombre)]
ind <- which(atletas$nombre %in% dup)
atletas.dup <- atletas[ind,]
sum(table(atletas.dup$nombre) == 2)
num_disciplina <- rep(c("disciplina_1", "disciplina_2"),23)
num_pais <- rep(c("pais_1", "pais_2"),23)
atletas.dup$num_disciplina <- num_disciplina
atletas.dup$num_pais <- num_pais
atletas.dup %>% 
  select(-pais, -num_pais) %>%
  pivot_wider(names_from = num_disciplina,
              values_from = disciplina) -> atletas.dup2
atletas.dup2

atletas.dup %>% 
  select(-disciplina,-num_disciplina) %>%
  pivot_wider(names_from = num_pais,
              values_from = pais) -> atletas.dup3
atletas.dup3

# atletas.dup2 : muestra los atletas que participaron 
# en 2 disciplinas.
# atletas.dup3 : muestra los atletas con sus respectvas
# nacionalidades

# Analizaremos si disciplina 1 y discplina 2 coinciden :
atletas.dup2[atletas.dup2$disciplina_1 == atletas.dup2$disciplina_2,]
# entonces, existen 2 registros duplicados, se encuentran 
# en las posiciones:
ind.dup2 <- which(atletas.dup2$disciplina_1 == atletas.dup2$disciplina_2)
nombre.dup2 <- atletas.dup2$nombre[ind.dup2]

# Se debe 1 solo regstro, la de ALI Mohammed. PORTELA Teresa
# es una deportista portuguesa. Existe otra PORTEL Teresa de
# nacionalidad espaniola
ind2 <- which(atletas$nombre %in% nombre.dup2)
atletas[ind2,]
atletas2 <- atletas[-c(255),]
dim(atletas)
dim(atletas2)

# Ahora, analizaremos si pais 1 y pais 2 coinciden
ind.dup3 <- which(atletas.dup3$pais_1 != atletas.dup3$pais_2)
nombre.dup3 <- atletas.dup3$nombre[ind.dup3]
# Vemos si los nombres eliminados anteriormente aparecen
# en este analisis
nombre.dup2 %in% nombre.dup3
nombre.dup2[2] # este nombre aparece, como ya se elimino
# no lo tomaremos en cuenta en este analisis
nombre.dup3 <- nombre.dup3[-8]
# Luego, se procedera a eliminar los datos incorrectos
ind3 <- which(atletas2$nombre %in% nombre.dup3)
atletas2[ind3,c(1,2)]
# Se averiguara las verdaderas nacionalidades en Google,
# se eliminara los datos incorrectos, mas no los duplicados,
# ya que podrian estar participando en mas de 1 disciplina.
# Despues de las averiguaciones, se han encontrado nombres
# duplicados que pertenecian a diferentes nacionalidades,
# en otras palabras, diferentes deportistas tienen el mismo
# nombre.

# Nuestra data final sera : atletas2

# Numero de atletas por pais ----------------------------------------------

atl.pais <- atletas2 %>% group_by(pais,nombre) %>% count()
atl.pais 
atl.pais2 <- atl.pais %>% group_by(pais) %>% count(sort = T)
atl.pais2

i <- which(duplicated(atletas2$nombre))
atletas2[c(1703,1704),]

# Atletas peruanos por disciplina -----------------------------------------

peru <- atletas2 %>% 
  filter(pais == "Peru") %>%
  group_by(disciplina) %>%
  count(sort = T)
peru
# La disciplina con más atletas peruanos es Atletismo.
# Completan el podio: Sailing con 5 atletas y Surf con
# 4 atletas.

# Script de ayuda --------------------------------------------------------

nom <- c("ana","ana","ana","pedro","pedro","luis","leandro","nadia","manuel")
pa <- c("peru","peru","peru","chile","chile","colombia","peru","colombia","peru")
dis <- c("d1","d2","d3","d2","d5","d3","d6","d9","d2")
df <- data.frame(nom,pa, dis)
colnames(df)
# numero de atletas por pais
df %>% group_by(pa,dis) %>% count
# 
df %>% group_by(pa, nom) %>% summarise(conteo = n()) %>% 
  group_by(pa) %>% count()
  
