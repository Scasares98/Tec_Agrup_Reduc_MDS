#librerias
library(readr)
library(factoextra)
library(ggplot2)
library(cluster)
library(gridExtra)
library(corrplot)
library(haven)
library(dplyr)
library(PerformanceAnalytics)
library(FactoMineR)
library(gplots)
library(graphics)
library(dplyr)


data <- read_sav("tterreno.sav")
data <- data.frame(data[,-1], row.names = make.names(data$modelo, unique = T))
data <- data[-1]
View(data)

names(data)
str(data)


dim(data)
nrow(data)
ncol(data)

sapply(data, function(x) sum(is.na(x)))

#marca   modelo      pvp cilindro       cc potencia      rpm     peso   plazas   cons90  cons120  consurb velocida acelerac    acel2 
#0        0        0        0        0        0        0        2        0       10       15        7        3       46        0 


#---------------------------------------------------------------
## Transformación de valores NAN ##

peso

mediana_peso <- median(data$peso, na.rm=TRUE)
data[is.na(data$peso), "peso"] <- mediana_peso

cons90

mediana_cons90 <- median(data$cons90, na.rm=TRUE)
data[is.na(data$cons90), "cons90"] <- mediana_cons90

cons120

mediana_cons120 <- median(data$cons120, na.rm=TRUE)
data[is.na(data$cons120), "cons120"] <- mediana_cons120

consurb

mediana_consurb <- median(data$consurb, na.rm=TRUE)
data[is.na(data$consurb), "consurb"] <- mediana_consurb

velocida

mediana_velocida <- median(data$velocida, na.rm=TRUE)
data[is.na(data$velocida), "velocida"] <- mediana_velocida

acelerac

mediana_acelerac <- median(data$acelerac, na.rm=TRUE)
data[is.na(data$acelerac), "acelerac"] <- mediana_acelerac

sapply(data, function(x) sum(is.na(x)))



#Dataframe Numéricas
lista_variables = c("pvp","cilindro","cc","potencia","rpm","peso","plazas",
                    "cons90","cons120","consurb","velocida","acelerac", "acel2")
data_numericas <- data[lista_variables]
data_numericas <- as.data.frame(data_numericas)
dim(data_numericas)



