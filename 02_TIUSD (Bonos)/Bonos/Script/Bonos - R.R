#Importamos las librerias
library(readr)
library(glmnet)
library(tidyverse)
library(fBasics)
library(car)
library(dplyr)
library(ggplot2)
library(knitr)
library(MASS)
library(corrplot)
library(PerformanceAnalytics)
library(gvlma)
library(tinytex)
library(devtools)
library(rsample)
library(tidyr)
library(broom)
library(flextable)
library(mgcv)
library(reshape2)
library(rmdformats)

library(factoextra)
library(FactoMineR)


#Importamos el dataset

bonos <- read_delim("ACPTIUSD.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE)
head(bonos)


#----------------------------
## ANÁLISIS EXPLORATORIO
#----------------------------

#Cambiamos el formato de las fechas del dataset

bonos = bonos[complete.cases(bonos), ]
bonos$Fechas = as.Date(bonos$X1, format = "%d/%m/%Y")
bonos=bonos[,2:12]

## Función melt de reshape2: "estira" el data frame

bonos_m <- melt( bonos )
View(bonos_m)

# Lo aplicamos al TIUSD2, empleando como indicador Fechas
data_long = melt(bonos, id="Fechas")
ggplot(data=data_long, aes(x= Fechas, y=value,  color=variable)) +
  #geom_line()
  geom_point(alpha = 0.3,  position = position_jitter()) +  #stat_smooth(method = "lm") +
  labs(y = "Tipo", colour="Bono")


#Estructura del Dataset

#Training - observaciones activas
bonos_act <- bonos[1:750,1:9]
fechas_act <- bonos[1:750, 11]
bonos_act <- cbind(bonos_act, fechas_act)
dim(bonos_act)
View(bonos_act)

#Testing - observaciones suplementarias
bonos_sup <- bonos[750:783, 10]
dim(bonos_sup)
View(bonos_sup)

#Summary
#1
summary(bonos_act)
#2
bonos_act_sum <- bonos_act[-10]

bonos_act_stats = data.frame(
  Min = apply(bonos_act_sum, 2, min, na.rm=TRUE), # mín
  Q1 = apply(bonos_act_sum, 2, quantile, 1/4, na.rm=TRUE), # 1er cuartil
  Med = apply(bonos_act_sum, 2, median, na.rm=TRUE), # mediana
  Mean = apply(bonos_act_sum, 2, mean, na.rm=TRUE), # media
  SD = apply(bonos_act_sum, 2, sd), # Desviación típica
  Q3 = apply(bonos_act_sum, 2, quantile, 3/4, na.rm =TRUE), # 3er cuartil
  Max = apply(bonos_act_sum, 2, max, na.rm=TRUE) # Máx
)
bonos_act_stats=as.data.frame(round(bonos_act_stats, 1))
bonos_act_stats

#----------------------------------------------------------------------------------------------
#¿Tiene sentido llevar a cabo, en este caso, un análisis de componentes principales? 
#Para justificarlo, deberá llevar a cabo las pruebas que estime oportunas, como, 
#por ejemplo el análisis de la matriz de correlaciones, el del determinante de dicha matriz, 
#la prueba de esfericidad de Bartlett, el KMO o el MSA.
#----------------------------------------------------------------------------------------------


##Análisis de la matriz de correlación
#--------------------------------------

cor.mat = round(cor(bonos_act_sum),2) #problemas con los NA; dos opciones: use="complete.obs" que elimina la fila completa allí donde
#existe un NA (opción radical pero recomendada) o bien use="pairwise.complete.obs", que los elimina los pares de datos afectados;
# en principio, parecería más adecuada pero puede dar lugar a problemas de matrices no definidas-positivas.
cor.mat #problema: los NAs


#si queremos conocer los nds, necesitamos cargar otro paquete, Hmisc

require(Hmisc)
cor.mat.nds= rcorr(as.matrix(bonos_act_sum))
cor.mat.nds #genera tres elementos en la salida: R, nº de observaciones, nds
# Podemos visualizarlo mediante un correlograma del paquete corrplot (que cargamos)
require(corrplot)
corrplot(cor.mat, type="lower", order="original", #type=lower hace ref a cómo queremos visualizar la matriz, si por debajo,
         #completa o por encima de la diagonal principal;
         # Method cambia la salida; probar "pie", "number" o "color"
         tl.col="black", tl.cex=0.7, tl.srt=45)  # las correlaciones positivas en azul, las negativas en rojo
# tl.col, color etiquetas; tl.srt, ángulo etiquetas (string rotation)
corrplot(cor.mat, type="full", order="hclust", addrect = 3,
         tl.col="black", tl.cex=0.7, tl.srt=45) #permite visualizar clusters
#...  y también podemos visualizar un chart de correlaciones con el paquete PerformanceAnalytics, que cargamos


#install.packages("PerformanceAnalytics")
require(PerformanceAnalytics)
chart.Correlation(bonos_act_sum, histogram=TRUE, pch=19)
# La distribución de cada variable en la diagonal;
# Por debajo: diagramas de dispersión por pares con línea de ajuste
# Por encima: el valor del coef de corr con el nds como estrellas:
# p-valores(0, 0.001, 0.01, 0.05, 0.1, 1) <=> símbolos("***", "**", "*", ".", " ")
# ... o a través de un mapa de calor
col = colorRampPalette(c("red", "white", "blue"))(20) #definimos la paleta de colores;
heatmap(x = cor.mat, col = col, symm = TRUE) # symm = T  si la matriz es simétrica
# Índice KMO y prueba de esfericidad de Bartlett para verificar la idoneidad del ACP - ANFAC


#Matriz de correlaciones parciales  (-1 * matriz anti-imagen de spss, sin la diagonal)
#install.packages("ppcor")
require(ppcor)

p.cor.mat=pcor(bonos_act_sum) #devuelve la matriz de correlaciones parciales (estimate), los p-valores, el valor del estadístico t
p.cor.mat# (t-statistic), el tamaño muestral (n), etc


#https://rpubs.com/marcelo-chavez/multivariado_1

##Determinante de la matriz de correlación
#-----------------------------------------

det(cor.mat)


##Prueba de esfericidad de Bartlett
#-----------------------------------------
bartlett.test(bonos_act_sum)


#KMO o el MSA.
#-----------------------------
library(psych)
KMO(bonos_act_sum)



#----------------------------------------------------------------------------------------------
#¿Cuántos componentes permitirían explicar, adecuadamente, 
#la estructura subycente de los tipos de interés aquí analizados? Justifique su respuesta empleando, 
#por ejemplo, las pruebas de la varianza explicada o del gráfico de sedimentación;
#----------------------------------------------------------------------------------------------


#PRUEBA DE LA VARIANZA EXPLICADA
#-------------------------------

#REALIZAMOS EL ACP CON EL APARTADO DE TRAINING
acp = PCA(bonos_act_sum, graph=T) 
acp
# extraemos los autovalores y observamos la varianza explicada

acp$eig # con FacotMineR
get_eig(acp) #con factoextra


# hacemos el screeplot

fviz_eig(acp, addlabels=TRUE, hjust = -0.3)+
  labs(title="Scree plot / Gr?fico de sedimentaci?n", x="Dimensiones", y="% Varianza explicada")
theme_minimal()


# Relaci?n de las variables con los CCPP

var=get_pca_var(acp) #factoextra
var


# coordenadas y contribuciones de las variables
var$coord #coordenadas de las observaciones (ind) o variables (var)
var$contrib  # contribuciones (en %) de las variables a los CCPP. La contribuci?n de una variable (var) 
# a un CP dado es (en %): (var.cos2 * 100) / (total cos2 del componente).
var$cor # correlaciones de las observaciones (ind) o variables (var)
var$cos2 # representa la calidad de la representaci?n para las variables sobre el mapa factorial. 
# Se calcula como el cuadrado de las coordenadas: var.cos2 = var.coord * var.coord

var$cos2
var$coord^2

x=var$coord[,1]
y=var$coord[1,]

sum(x^2)
sum(y^2)

z=var$cos2[,1] # cos2 del CP1
sum(z) # cos2 total del CP1 (Autovalor del CP1)
z[1]/sum(z)*100 # contribuci?n de la primera variable a la explicaci?n del CP1

#Con factominer
acp$var$coord
acp$var$cor
acp$var$cos2
acp$var$contrib


# Gr?fico por defecto de las variables en el espacio de dos CCPP ...
fviz_pca_var(acp, col.var = "steelblue")

## ... del que modificamos los colores para visualizar mejor la contribuci?n de la variable en el eje principal

fviz_pca_var(acp, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE) +
  labs(title="Mapa de ejes principales")+
  theme_minimal()

fviz_pca_var(acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribuci?n de las variables a cada uno de los ejes principales
## eje 1
fviz_contrib(acp, choice="var", axes = 1 )+
  labs(title = "Contribuciones a la Dim 1")
## eje 2
fviz_contrib(acp, choice="var", axes = 2 )+
  labs(title = "Contribuciones a la Dim 2")

## ambos ejes
fviz_contrib(acp, choice="var", axes = 1:2)+
  labs(title = "Contribuciones a las dos dimensiones")


# Extraccci?n de resultados para individuos
#-----------------------------------------
ind = get_pca_ind(acp)
ind

#Coordenadas de los individuos / observaciones en el plano de los ejes principales
head(ind$coord) 
## Dice bastante poco, as? que ser? mejor representarlos en un mapa 2D

# 1.  repel = TRUE para evitar solapamientos
# 2. Control automatico del color de los indiv mediante la opci?n cos2, calidad de los individuos sobre el mapa de ejes;
#       emplear s?lo puntos, mejor;
# 3. Emplear el gradiente de color

#cos2 representa la comunalidad de la variable en el factor, el cuadrado de la carga factorial; su suma por factores es uno 

fviz_pca_ind(acp, repel = T, col.ind = "cos2")+
  scale_color_gradient2(low="blue", mid="white",
                        high="red", midpoint=0.6)+
  theme_minimal()



#GRAFICO DE SEDIMENTACION
#-------------------------------
scree(cor.mat,main ="Grafico_de_Sedimentacion")




#--------------------------------------------------------------------------------------
#Finalmente, ¿tiene sentido llevar a cabo una rotación de las variables subyacentes? 
#Para responder, lleva a cabo una rotación Varimax, por ejemplo.
#--------------------------------------------------------------------------------------

#Rotacion VARIMAX
#-------------------------------

factanal(bonos_act_sum, factors = 3, rotation = 'varimax')




#--------------------------------------------------------------------------------------
#Por último, deberá elaborar las oportunas conclusiones.
#--------------------------------------------------------------------------------------

#Prediccion
#-------------------------------
library(pls)
library(imputeTS)

rm(list=ls())

bonos <- read_delim("ACPTIUSD.csv", ";", 
                    escape_double = FALSE, trim_ws = TRUE)

bonos = bonos[complete.cases(bonos), ]
bonos
bonos$Fechas = as.Date(bonos$X1, format = "%d/%m/%Y")

TIUSD <- bonos

training <- TIUSD[1:750, 2:11]
test <- TIUSD[750:783, 2:11]

colnames(training)[10] <- 'IRS_10Y'
colnames(test)[10] <- 'IRS_10Y'


set.seed(123)
modelo_pcr <- pcr(formula = IRS_10Y ~ ., data = training, scale. = TRUE, validation = "CV")
modelo_pcr_CV <- MSEP(modelo_pcr, estimate = "CV")
min(modelo_pcr_CV$val)

# Test-MSE
predicciones <- predict(modelo_pcr, newdata = test, ncomp = 2)
test_mse <- mean((predicciones - test$IRS_10Y)^2)
test_mse

















