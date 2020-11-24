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
#View(data)

names(data)
str(data)


dim(data)
nrow(data)
ncol(data)

sapply(data, function(x) sum(is.na(x)))

#marca   modelo      pvp cilindro       cc potencia      rpm     peso   plazas   cons90  cons120  consurb velocida acelerac    acel2 
#0        0        0        0        0        0        0        2        0       10       15        7        3       46        0 


#---------------------------------------------------------------



#quitamos otras variables

lista_variables_1 = c("pvp","cc","velocida",'peso','cilindro')

data_num <- data[lista_variables_1]
data_num <- as.data.frame(data_num)

data_num <- na.omit(data_num)
dim(data_num)

#Dataframe Numéricas
lista_variables = c("pvp","cilindro","cc","rpm","peso","plazas","cons90","cons120","consurb","velocida","acelerac")
data_numericas <- data[lista_variables]
data_numericas <- as.data.frame(data_num)
dim(data_numericas)


#formato final
#--------------------------
#summary
summary(data_num)
cor(data_num)
#---------------------------------------------------------------

##### EDA VARIABLES SELECCIONADAS #####

corrplot(cor(data_num), type = 'lower', method = 'shade' )

c("pvp","cc","velocida",'peso','cilindro')

#PVP

ggplot(data_num, aes(x=data_num$pvp)) + geom_histogram(bins = 120) +
  ggtitle('Distribución de los precios de los coches') 
  
  
hist(data_num$pvp, 
     col = 'royalblue2',
     border = 'black',
     prob = TRUE,
     main = 'Precio de los coches',
     breaks = 20)
lines(density(data_num$pvp),
      lwd = 3,
      col = 'red')



precio_dist <- sort.int(data_num$pvp, decreasing = TRUE)
#View(precio_dist)

precio_dist_0 <- as.data.frame(precio_dist[0:20])
colnames(precio_dist_0)[1] <- 'valores'

precio_dist_1 <- as.data.frame(precio_dist[21:40])
precio_dist_2 <- as.data.frame(precio_dist[41:60])
precio_dist_3 <- as.data.frame(precio_dist[61:80])
precio_dist_4 <- as.data.frame(precio_dist[81:100])
precio_dist_5 <- as.data.frame(precio_dist[101:120])



ggplot() + geom_histogram(precio_dist_0, aes(x = valores))

  geom_histogram(data = precio_dist, binwidth = 0.1, fill = "yellow") +
  theme_classic()

  
  
  ggplot() +
    geom_histogram(data = diamonds[diamonds$cut !='Premium',],  mapping = aes(carat), binwidth = 0.1, fill = "blue") +
    geom_histogram(data = diamonds[diamonds$cut =='Premium',],  mapping = aes(carat), binwidth = 0.1, fill = "yellow") +
    theme_classic()


#cc

ggplot(data_num, aes(x=data_num$cc)) + geom_histogram() +
  ggtitle('Distribución de los cc de los coches') 


hist(data_num$cc, 
     col = 'royalblue2',
     border = 'black',
     prob = TRUE,
     main = 'Precio de los coches',
     breaks = 50)
lines(density(data_num$cc),
      lwd = 3,
      col = 'red')

#velocida

ggplot(data_num, aes(x=data_num$velocida)) + geom_histogram(bins = 120) +
  ggtitle('Distribución de la velocidad de los coches') 

hist(data_num$velocida, 
     col = 'royalblue2',
     border = 'black',
     prob = TRUE,
     main = 'Precio de los coches',
     breaks = 120)
lines(density(data_num$velocida),
      lwd = 3,
      col = 'red')


#peso

ggplot(data_num, aes(x=data_num$peso)) + geom_histogram(bins = 120) +
  ggtitle('Distribución del peso de los coches')

hist(data_num$peso, 
     col = 'royalblue2',
     border = 'black',
     prob = TRUE,
     main = 'Precio de los coches',
     breaks = 120)
lines(density(data_num$peso),
      lwd = 3,
      col = 'red')

#cilindro

ggplot(data_num, aes(x=data_num$cilindro)) + geom_histogram() +
  ggtitle('Distribución del nº de cilindros de los coches') +
  abline(v = 5, lwd = 10)

hist(data_num$cilindro, 
     col = 'royalblue2',
     border = 'black',
     prob = TRUE,
     main = 'Precio de los coches')
lines(density(data_num$cilindro),
      lwd = 3,
      col = 'red')










## ACP ##


data_num_table <- as.table(as.matrix(data_num))

chisq <- chisq.test(data_num_table)
chisq

data_num_table_afc = CA(data_num_table, graph=FALSE)
print(data_num_table)

summary(data_num_table, nb.dec = 2, ncp = 2)


autov = get_eigenvalue(data_num_table_afc)
traza = sum(autov[,1]) 
cor.coef = sqrt(traza)
cor.coef

autoval = get_eigenvalue(data_num_table_afc)
head(round(autoval, 2))

#screeplot:
fviz_screeplot(data_num_table_afc)+
  ggtitle("Gráfico de sedimentación") +
  labs(x="Dimensiones",y="Porcentaje de varianza explicada")

plot.CA(data_num_table_afc, axes = c(1,2), col.row = "blue", col.col = "red")


#---
fviz_contrib(data_num_table_afc, choice = "col", axes = 1)+
  ggtitle("Contribución de las columnas a la 1ª dimensión")+
  labs(x="Filas",y="Porcentaje de contribución")  


fviz_contrib(data_num_table_afc, choice = "col", axes = 2) +
  ggtitle("Contribución de las columnas a la 2ª dimensión")+
  labs(x="Filas",y="Porcentaje de contribución")

fviz_contrib(data_num_table_afc, choice = "col", axes = 1:2) +
  ggtitle("Contribución de las columnas a las dos dimensiones")+
  labs(x="Filas",y="Porcentaje de contribución")










#---------------------------------------------------------------
## Transformación de valores NAN ##

#peso
#mediana_peso <- median(data$peso, na.rm=TRUE)
#data[is.na(data$peso), "peso"] <- mediana_peso

#cons90
#mediana_cons90 <- median(data$cons90, na.rm=TRUE)
#data[is.na(data$cons90), "cons90"] <- mediana_cons90

#cons120

#mediana_cons120 <- median(data$cons120, na.rm=TRUE)
#data[is.na(data$cons120), "cons120"] <- mediana_cons120

#consurb

#mediana_consurb <- median(data$consurb, na.rm=TRUE)
#data[is.na(data$consurb), "consurb"] <- mediana_consurb

#velocida

#mediana_velocida <- median(data$velocida, na.rm=TRUE)
#data[is.na(data$velocida), "velocida"] <- mediana_velocida

#acelerac

#mediana_acelerac <- median(data$acelerac, na.rm=TRUE)
#data[is.na(data$acelerac), "acelerac"] <- mediana_acelerac

#sapply(data, function(x) sum(is.na(x)))


#---------------------------------------------------------------


#escalamos las variables

performScaling= T # on/off para experimentar
if (performScaling) {
  # Loop sobre cada columna
  for (colName in names(data_num)) {
    # Comprueba si la columna es de datos numéricos.
    if(class(data_num[,colName]) == 'integer' | class(data_num[,colName]) == 'numeric') {
      # escala la columna.
      data_num[,colName] = scale(data_num[,colName])
    }
  }
}

head(data_num, n = 3)

longitug_data_num <- length(data_num)

#---------------------------------------------------------------

#---------------------------------------------------------------

q.dist = get_dist(data_num[,1:longitug_data_num], stand = TRUE, method = "pearson") #sólo admite valores numéricos
str(q.dist)

fviz_dist(q.dist, lab_size = 5)


fviz_dist(q.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size = 5)

dist.eucl = dist(data_num, method = 'euclidean', upper = F)

#---------------------------------------------------------------

#Distancia euclídea

corrplot(as.matrix(dist.eucl), is.corr = FALSE, method = "color", tl.cex=0.6, tl.col="blue")



#... que podemos cambiar a
#corrplot(as.matrix(dist.eucl), is.corr = FALSE, method = "color", type="lower",
         #diag=F, order="hclust", tl.cex=0.6, tl.col="blue")
#---------------------------------------------------------------

#dendograma
plot(hclust(dist.eucl, method = "ward.D2"), cex=0.7, main="Dendrograma", ylab="Anchura",
     xlab="Análisis cluster aplicando Ward sobre matriz de distancias euclídeas")

#---------------------------------------------------------------

#data_num.eclust = eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
 #                        hc_metric="euclidean", nstart=25) #NS START, configuración de semillas (utilizas 25 sembrado sdistintos y los optimiza)


# podemos fijar el número de clusters añadiendo k=número de clusters, por ejemplo k=4
data_num.eclust = eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
                         hc_metric="euclidean", nstart=25, k=6)

#el triangulo o circulo gordo indica el centroide del grupo



# Gráfico de silueta (silhouette plot) - Lo hacemos sobre la opción con k=4 clusters
fviz_silhouette(data_num.eclust)


#numero óptimo
data_num.eclust$nbclust


#--------------------------
#Y pasamos a un jerárquico
#--------------------------
# calcula hclust, forzando a 4 grupos
data_num.eclust.j = eclust(data_num[,-1], "hclust", k=4)



# dendrograma con 4 grupos
fviz_dend(data_num.eclust.j, rect = TRUE)



# silueta
fviz_silhouette(data_num.eclust.j)


# scatter plot
fviz_cluster(data_num.eclust.j)



#-----pruebas
par(mfrow=c(2,3))

g2 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=2)
g3 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=3)
g4 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=4)
g5 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=5)
g6 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=6)
g7 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=7)

```{r}
set.seed(154)
p1 <- fviz_cluster(kmeans(data_num, centers = 2, nstart = 25), geom = 'point',
                   data = data_num, stat.ellipse = 'euclid') + ggtitle('K = 2')
p2 <- fviz_cluster(kmeans(data_num, centers = 3, nstart = 25), geom = 'point',
                   data = data_num, stat.ellipse = 'euclid') + ggtitle('K = 3')
p3 <- fviz_cluster(kmeans(data_num, centers = 4, nstart = 25), geom = 'point',
                   data = data_num, stat.ellipse = 'euclid') + ggtitle('K = 4')
p4 <- fviz_cluster(kmeans(data_num, centers = 5, nstart = 25), geom = 'point',
                   data = data_num, stat.ellipse = 'euclid') + ggtitle('K = 5')
p5 <- fviz_cluster(kmeans(data_num, centers = 6, nstart = 25), geom = 'point',
                   data = data_num, stat.ellipse = 'euclid') + ggtitle('K = 6')
p6 <- fviz_cluster(kmeans(data_num, centers = 7, nstart = 25), geom = 'point',
                   data = data_num, stat.ellipse = 'euclid') + ggtitle('K = 7')

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
```


```{r}


g2 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=2)
g3 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=3)
g4 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=4)
g5 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=5)
g6 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=6)
g7 <- eclust(data_num[,-1], FUNcluster = "kmeans", stand=TRUE,
             hc_metric="euclidean", nstart=25, k=7)

View(g6)
```

```{r}
#--------------------------
#Y pasamos a un jerárquico
#--------------------------
# calcula hclust, forzando a 4 grupos
data_num.eclust.j = eclust(data_num[,-1], "hclust", k=6)



# dendrograma con 4 grupos
fviz_dend(data_num.eclust.j, rect = TRUE)



# silueta
fviz_silhouette(data_num.eclust.j)


# scatter plot
fviz_cluster(data_num.eclust.j)

```

