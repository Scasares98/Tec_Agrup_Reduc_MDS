#AFC Votos

#Importamos librerias
library(readr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(corrplot)
library(gplots)
library(graphics)

#-------------------------------

#El rmarkdown está más completo

#-------------------------------



#Importamos los datos


votos<-matrix(c(462,502,383,316,846,
                441,857,544,376,639,
                471,83,551,388,172,
                576,606,616,478,499,
                369,274,230,762,169), nrow = 5, ncol = 5)
rownames(votos)<-c("Trabaja", "Domestico", "Parado", "Estudiante", "Jubilado")
colnames(votos)<-c("PP", "PSOE", "UP", "Cs", "Otros")


votos_tab <- as.table(as.matrix(votos))

votos_scol <- read_delim("AFC-votos.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

votos_scol <- votos_scol[,2:6]
votos_scol <- as.table(as.matrix(votos_scol))
votos_scol


------
votos
votos_tab
votos_scol
------


#EDA
summary(votos)

balloonplot(t(votos_tab), main ="Votos", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

mosaicplot(votos_tab, shade = TRUE, las=2,
           main = "Votos")


#Contraste de independencia chi cuadrado

chisq <- chisq.test(votos_tab)
chisq


#Analisis de correspondencias

votos_afc = CA(votos_tab, graph=FALSE)
print(votos_tab)

summary(votos_tab, nb.dec = 2, ncp = 2)





#Interpretación analisis Correspondencias

#Nivel asociacion entre filas y columnas

autov = get_eigenvalue(votos_afc)
traza = sum(autov[,1]) 
cor.coef = sqrt(traza)
cor.coef
#----------------------------------------------------
#Esto es lo mismo que el chisq

# Estadístico chi cuadrado
chi2 = traza*sum(as.matrix(votos_scol))
chi2

# Grados de libertad del estadístico
gdl = (nrow(votos_scol) - 1) * (ncol(votos_scol) - 1)
gdl

# Nivel de significación del estadístico de contraste, o _p-valor_
pval = pchisq(chi2, df = gdl, lower.tail = FALSE)
round(pval, 2)
#----------------------------------------------------


#Autovalores y gráfico de sedimentación

autoval = get_eigenvalue(votos_afc)
head(round(autoval, 2))

#screeplot:
fviz_screeplot(votos_afc)+
  ggtitle("Gráfico de sedimentación") +
  labs(x="Dimensiones",y="Porcentaje de varianza explicada")

# Gráfico de dispersión del análisis de correspondencias entre filas y columnas.


plot.CA(votos_afc, axes = c(1,2), col.row = "blue", col.col = "red")



#Mapa 2D
fviz_ca_biplot(votos_afc)+
  theme_minimal()+
  ggtitle("Mapa 2D. Análisis de correspondencias simples")



#Contribución de filas y columnas

filas=get_ca_row(votos_afc)
filas

head(filas$coord)


fviz_ca_row(votos_afc)


fviz_ca_row(votos_afc, col.row="steelblue", shape.row = 15)+
  ggtitle("Análisis de correspondecias simples. Mapa de puntuación de filas")



head(filas$contrib)


corrplot(filas$contrib, is.corr=FALSE)

#CONTRIBUCION DE LAS FILAS A LAS DIMENSIONES
fviz_contrib(votos_afc, choice = "row", axes = 1)+
  ggtitle("Contribución de las filas a la 1ª dimensión")+
  labs(x="Filas",y="Porcentaje de contribución")  


fviz_contrib(votos_afc, choice = "row", axes = 2) +
  ggtitle("Contribución de las filas a la 2ª dimensión")+
  labs(x="Filas",y="Porcentaje de contribución")



# Contribución total de las filas a las dos dimensiones
fviz_contrib(votos_afc, choice = "row", axes = 1:2) +
  ggtitle("Contribución de las filas a las dos dimensiones")+
  labs(x="Filas",y="Porcentaje de contribución")

#CONTRIBUCION DE LAS columnas A LAS DIMENSIONES

fviz_contrib(votos_afc, choice = "col", axes = 1)+
  ggtitle("Contribución de las filas a la 1ª dimensión")+
  labs(x="Filas",y="Porcentaje de contribución")  


fviz_contrib(votos_afc, choice = "col", axes = 2) +
  ggtitle("Contribución de las filas a la 2ª dimensión")+
  labs(x="Filas",y="Porcentaje de contribución")

fviz_contrib(votos_afc, choice = "col", axes = 1:2) +
  ggtitle("Contribución de las filas a las dos dimensiones")+
  labs(x="Filas",y="Porcentaje de contribución")


# Color de los puntos de fila en virtud de su poder explicativo
# Valores posibles del argumento col.row:
# "cos2", "contrib", "coord", "x", "y"
fviz_ca_row(votos_afc, col.row = "contrib")
# Podemos cambiar el color del gradiente
fviz_ca_row(votos_afc, col.row="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=10)+
  theme_minimal()
# Control de la transparencia de los puntos de fila de acuerdo a su contribución con alpha.var
# Valores posibles del argumento alpha.var:
# "cos2", "contrib", "coord", "x", "y"
fviz_ca_row(votos_afc, alpha.row="contrib")+
  ggtitle("Mapa factorial de ANACOR. Contribución de las filas.")+
  theme_minimal()



# Calidad de la representación de las filas: el Cos2
head(filas$cos2)
corrplot(filas$cos2, is.corr=FALSE)


fviz_cos2(votos_afc, choice = "row", axes = 1:2)+
  ggtitle("Cos2 de las dos dimensiones")+
  labs(y="Cos2 - calidad de la representación")


# Representación conjunta de filas y columnas


fviz_ca_biplot(votos_afc)+
  theme_minimal()




fviz_ca_biplot(votos_afc, map ="rowprincipal", arrow = c(TRUE, TRUE))+ 
  ggtitle("Análisis de correspondencias simples. Gráfico asimétrico.")



fviz_ca_biplot(votos_afc, map ="colgreen",
               arrow = c(TRUE, FALSE))
