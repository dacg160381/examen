##################################################
# Programa: FIJA                                 #
# Autor: Daniel Chavez (dacg160381@hotmail.com)  #
# Fecha Actualizacion: 21/07/2021                #
##################################################
#------------------------------------------------------------------------------------------------#

# Antes, limpiamos el workspace, por si hubiera algun dataset o informacion cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Vamos a cargar las librerias necesarias, de no ententerlas ir a: 
#   https://www.rdocumentation.org/
#   de no tenerlas instalarlas, hacerlo con las sentencia: 
#   install.chipages(c("tidyverse","readr","readxl","dummies","sqldf","fpc"), dependencies = T)
library(tidyverse)
library(readr)
library(readxl)
library(dummies)
library(sqldf)
library(data.table)
library(caret)


# Vamos a preparar los PRODUCTO_2
datos<- fread("MODELOS_SOHO/FIJA_NC_28122022.csv")

# as.character, variables indicadores (IDs)
datos$FLAG_DEAC    <- as.character(datos$FLAG_DEAC)
datos$PERIODO <- as.character(datos$PERIODO)
datos$RUCCOMPANIA <- as.character(datos$RUCCOMPANIA)

# Tabla de frecuencias por target, analisis del experto
prop.table(table(datos$FLAG_DEAC))

table(datos$PERIODO, datos$FLAG_DEAC)

datos = datos %>% filter(!PERIODO %in% c("202209","202210"))

set.seed(123)
# Se crean los ?ndices de las observaciones de entrenamiento
train <- createDataPartition(y = datos$FLAG_DEAC, p = 0.80, list = FALSE, times = 1)
datos_train <- datos[train, ]
datos_valid  <- datos[-train, ]

dim(datos_train)
dim(datos_valid)
dim(datos)

addmargins(table(datos_train$FLAG_DEAC))
addmargins(round(100*prop.table(table(datos_train$FLAG_DEAC)),6))

addmargins(table(datos_valid$FLAG_DEAC))
addmargins(round(100*prop.table(table(datos_valid$FLAG_DEAC)),6))

# datos_1 para preparar
datos_1 <- data.table(rbind(cbind(Grupo = "TRAIN", datos_train),
                            cbind(Grupo = "VALID", datos_valid)))
table(datos_1$Grupo)
str(datos_1[,1:10])

pos_num <- names(c(which(sapply(datos_1,class)=="character"),which(sapply(datos_1,class)=="factor")))

# reemplazmos por 0 a todos los NAs, por regla de negocio. Ademas, eliminamos variable con sd = 0
nas_data <- colSums(is.na(datos_1))[colSums(is.na(datos_1)) != 0] # todas la variables con NAs
length(nas_data)

datos_1[is.na(datos_1)] <- 0 # reemplazo por 0

Sds <- apply(datos_1 %>% select(-pos_num),2 ,sd , na.rm = TRUE) # sd = 0
# primero
datos_2 <- as.data.frame(datos_1 %>% select(-pos_num) %>% select(names(Sds[Sds != 0])))

## Funcion de normalizacion [0, 1] y trasformacion para todas las variables
#Nor01 <- function(data) (data - min(data,na.rm=T))/(max(data,na.rm=T) - min(data,na.rm=T))
datos_2 <- datos_2 %>% mutate_all(scale)
datos_3 <- datos_2 %>% mutate_all(function(X) log(X^2+1))

# union de transformaciones
datos_4 <- data.frame(datos_2, datos_3)
dim(datos_4)

# kurtorsis
library(moments)
Kur  <- datos_4 %>% map(kurtosis)
str(Kur)
hist(as.numeric(Kur)[as.numeric(Kur)<=4000])
length(Kur[Kur>5000])

library(rlist)
Kur2=list.subset(Kur, Kur<=1000)
datos_4 = datos_4 %>% select(list.names(Kur2))
dim(datos_4)

ske  <- datos_4 %>% map(skewness)
str(ske)
hist(as.numeric(ske))

ske2=list.subset(ske, ske <= 30)
datos_4 = datos_4 %>% select(na.omit(list.names(ske2)))
dim(datos_4)
str(datos_4[,1:10])

# resumen variables
skeee <- datos_4 %>% map(skewness)
kuree <- datos_4 %>% map(kurtosis)
asimetria <- data.frame(ske = as.numeric(skeee), kur = as.numeric(kuree))
rownames(asimetria) <- colnames(datos_4)
summary(asimetria)
dim(datos_4)
names(datos_4[2, 1:20])

#------------------------------------------------------------------------------------------------------------------------
# deteccion de valores extremos
# Funcion que detecta valores extremos y los reemplaza por NAs
NA_out <- function(x, na.rm = TRUE, ...) {
  # Quantiles del 25% y 75% de la variable
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm) 
  H_ext <- 1.5 * IQR(x, na.rm = na.rm) 
  # Se crea la variable igual a "x", donde se reemplazara
  #   los extremos hallados por NA
  y_ext <- x
  y_ext[x < (qnt[1] - H_ext)] <- NA
  y_ext[x > (qnt[2] + H_ext)] <- NA
  return(y_ext)
}

# deteccion de valores extremos
datos_4.1 <- datos_4 %>% mutate_all(NA_out)
datos_4.1[, 1:20]
nas <- which(colSums(is.na(datos_4.1))!=0)

# Imputacion, metodo 4
datos_na <- datos_4.1[ ,names(datos_4.1%>%select(nas))]
dim(datos_na)

nas_data <- colSums(is.na(datos_na))[colSums(is.na(datos_na)) != 0] # todas la variables con NAs
# % de variables con NAs
head(sort(nas_data, decreasing = TRUE)/nrow(datos_1),20)
library(agricolae)
h2<- graph.freq(nas_data/nrow(datos_1),plot=TRUE, col = 3)
print(table.freq(h2),row.names=FALSE)
# desicion: tomar variables por debajo del 44%
#freq <- nas_data/nrow(datos_1)
#sort(freq[freq > 0.4], decreasing = T)

ls()
rm(datos_2,datos_3, datos_4, datos_4.1)

for(j in 1:ncol(datos_na)){
  na.true = is.na(datos_na[,j])
  mini <- quantile(datos_na[, j], 0.25, na.rm=T)
  maxi <- quantile(datos_na[, j], 0.75, na.rm=T)
  na.azar = sample(runif(100000, mini, maxi), length(na.true[na.true == T]), replace = T)
  na.id = subset(as.numeric(rownames(datos_na)), is.na(datos_na[, j]))
  datos_na[na.id, j] = na.azar
}


# comprobacion de sd y sum = 0
cero_data <- colSums(datos_na)[colSums(datos_na) == 0]

length(cero_data)
sd_data <- apply(datos_na,2,sd)
length(sd_data[sd_data==0])
datos_1[,1:10]

dim(datos_1)

datos_num <- as.data.frame(cbind(datos_1[, c(1,2,3,39)], 
                                 select(datos_na, -c(names(cero_data)),-c(names(sd_data[sd_data==0])))))
dim(datos_num)
colnames(datos_num)
# Data consolidada para hipotesis
# nos quedamos con los menores a 0.05, que cumplen con las hipotesis:
#  Para Cuantitativas                    Para cualitativas
#  Ho: medias iguales (1 y 0)                   Ho: independientes (1 y 0)
#  Ha: medias diferentes (1 y 0)                Ho: dependientes (1 y 0)
rm(datos_na)
pvalues <- c(0)
for(i in 5:ncol(datos_num)) pvalues[i-4] <- t.test(datos_num[, i] ~ as.factor(datos_num$FLAG_DEAC))$p.value

datavar <- data.frame(variables = names(datos_num)[5:(length(pvalues)+4)], pvalues)
datavar <- subset(datavar, is.na(datavar[,2])!=TRUE)
datavar <- subset(datavar, datavar[,2] < 0.1)
dim(datavar)
write.csv(datavar, "Variables_pvalue_soho2.csv")

rm(datos, datos_train, datos_valid)

# Data final de modelamiento
datos_num2 <- data.frame(datos_1[, c(1,2,3,39)] ,
                         datos_num %>% select(as.vector(as.character(datavar[,1]))))
dim(datos_num2)
datos_num2[1:2, 1:10]
# Exportar base para modelar
colnames(datos_num2)
saveRDS(datos_num2, "MODELOS_SOHO/01_base_modelo_NC_FIJA.rds")


