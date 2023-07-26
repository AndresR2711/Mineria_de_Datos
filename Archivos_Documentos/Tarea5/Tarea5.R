# Tarea5
library(cluster)
library(factoextra)
library(fmsb)
# ========================
#         Ejercicio 1
# ========================
setwd("/Users/AndresR/Documents/Tarea5")
datos <- read.csv("EjemploAlgoritmosRecomendacion.csv", header=TRUE, sep=";" , dec = ",",row.names = 1)
datos
set.seed(123)
km <- kmeans(datos, centers = 4, iter.max = 200, nstart = 100)
km

km$centers
# Inercia Total
km$totss  

# Inercia Intra-clases por grupo (una para cada grupo)
km$withinss 
# Inercia Intra-clases
km$tot.withinss
# inercia Inter-clases
km$betweenss
# Verificación del Teorema de Fisher
km$totss==km$tot.withinss+km$betweenss    



set.seed(123)
gap_stat <- clusGap(datos, FUN=kmeans, nstart=100, K.max=10, B=50)
fviz_gap_stat(gap_stat)


wss <- rep(NA, 10)
for (i in 1:10) wss[i] <- kmeans(datos, centers=i, nstart=100, iter.max=200)$tot.withinss
plot(1:10, wss, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")


avg_sil <- function(k) {
  km.res <- kmeans(datos, centers=k, nstart=100, iter.max=200)
  ss <- silhouette(km.res$cluster, dist(datos))
  mean(ss[, 3])
}
sil <- sapply(2:10, avg_sil)
plot(2:10, sil, type="b", xlab="Number of clusters", ylab="Average silhouette width")

fviz_nbclust(datos, FUNcluster = kmeans, method = "wss")



distancias <- dist(datos, method = "euclidean")
distancias

cluster_ward <- hclust(distancias, method = "ward.D2")
#cluster_ward


# Asignación de clústeres para la agregación de Ward con 2 clústeres
grupo <- cutree(cluster_ward, k = 2)
grupo

# Gráfico de barras horizontal
graficoH <- ggplot(data = datos, aes(x = datos$Precio, y = datos$Calidad.Producto, fill = grupo)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  labs(x = "Precio", y = "Calidad",
       title = "Relacion Calidad con Respeto al Precio")

# Mostramos el gráfico
graficoH

graficoV <- ggplot(data = datos, aes(x = datos$ImagenProducto, y = datos$TamanoPaquete, fill = grupo)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  coord_flip() +
  labs(x = "ImagenProducto  ", y = "Tamano Paquete",
       title = "Imagen del producto en relacion al tamaño del paquete")

# Mostramos el gráfico
graficoV


centroide <- function(num.cluster, datos, clusters) {
  ind <- (clusters == num.cluster)
  return(colMeans(datos[ind,]))
}

modelo <- hclust(dist(datos),method= "ward.D")
grupos <- cutree(modelo, k=2)
NDatos<-cbind(datos,grupos)

centro.cluster1<-centroide(1,(datos),(grupos))
centro.cluster2<-centroide(2,(datos),(grupos))
centros<-rbind(centro.cluster1,centro.cluster2)

maximos<-apply(centros,2,max)
minimos<-apply(centros,2,min)
centros<-rbind(minimos,centros)
centros<-rbind(maximos,centros)



color <- c("#0000FF","#D95B43","#C02942","#542437","#53777A")


radarchart(as.data.frame(centros),maxmin=TRUE,axistype=4,axislabcol="slategray4",
           centerzero=FALSE,seg=8, cglcol="gray67",
           pcol=color,plty=1,plwd=5,title="Radar Chart")

legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2"),
                 seg.len=-1.4,title="Clústeres",pch=21,bty="n" ,lwd=3, y.intersp=1, 
                 horiz=FALSE,col=color)


print("Se puede observar que los usuarios que le dan prioridad al tamaño del paquete tienden a tener esta como uno de sus principales intereses,dejando de lado las demas variables. Mientras que los del primer cluster toman en consideración los demás valores a excepción de este")
lectura2 <- read.csv("EjemploAlgoritmosRecomendacion.csv", header=TRUE, sep=";" , dec = ",")
lectura2



clust_ward4 <- hclust(dist(datos[,2:6]), method = "ward.D2")

clusters_ward4 <- cutree(clust_ward4, k = 7)


datos_clusters <- cbind(datos, clusters_ward4)


colnames(lectura2)[1] <- "Nombre"

datos_clusters


Leo <- subset(datos_clusters, lectura2$Nombre == "Justin")
Leo






datos_clusters

pruebas <- subset(datos_clusters, datos_clusters$clusters_ward4 == 6)
pruebas


distanciasprueba <- dist(pruebas, method = "euclidean")
distanciasprueba


print("Se puede observar que teresa tiene cierta preferencia por los artículos durables,y al ver que puntuó de mal manera el precio probablemente prefiera productos con alta durabilidad pero a un precio bajo. Se le podrian recomendar los productos comprados por Marisol ,con quien posee una distancia de 0.3674235 ")

print("leo por otro lado se ve que le da gran importancia al valor educativo,así como la durabilidad que posea el producto,se le podrían recomendar articulos que entren en este rango de caracteristicas. Podria recibir recomendaciones similares a las de Santiago,quien posee una distancia de 2.1017850 de Leo")

print("Con respecto a justin se puede observar que tiene cierta preferencia por los articulos que posean valor educativo,de rapida entrega y sobre todo gran durabilidad. Se le podrian recomendar los articulos de Flavia,con quien posee 0.2738613 de distancia ")



# ========================
#         Ejercicio 2
# ========================

setwd("/Users/AndresR/Documents/Tarea5")
datos <- read.csv("DatosBeijing.csv", header=TRUE, sep="," )
datos

str(datos)
summary(datos)
dim(datos)


datos <- na.omit(datos)

# > dim(datos)
# [1] 43824    13
# 

# 41757    
print("verifcando los dartos del dim antres y despues del omit se puede concluir que se eliminaron un total de 2 067 filas")
# datos

# c)
datos <- subset(datos, select = -DireccionViento)
datos



print("la variable DireccionViento debería de eliminarse porque es una variable categórica con una varias categorías(direcciones del viento), lo que podría hacer que el análisis de clustering sea más difícil de interpretar.")
print("Como alternativa se podria utilizar el ACP para reducir la cantidad de variables")

# d)
print(" Si se ejecuta un hclust() dará error a razon de que la variable DireccionViento es una variable no numerica")

# e)
km <- kmeans(datos[,-5], centers = 3, iter.max = 1000, nstart = 50)
km


# f)
modelo <- hclust(dist(datos),method= "ward.D")
grupos <- cutree(modelo, k=3)
NDatos<-cbind(datos,grupos)

centro.cluster1<-centroide(1,(datos),(grupos))
centro.cluster2<-centroide(2,(datos),(grupos))
centro.cluster3<-centroide(3,(datos),(grupos))
centros<-rbind(centro.cluster1,centro.cluster2,centro.cluster3)

maximos<-apply(centros,2,max)
minimos<-apply(centros,2,min)
centros<-rbind(minimos,centros)
centros<-rbind(maximos,centros)



color <- c("#0000FF","#D95B43","#979123","#542437","#53777A")


radarchart(as.data.frame(centros),maxmin=TRUE,axistype=4,axislabcol="slategray4",
           centerzero=FALSE,seg=8, cglcol="gray67",
           pcol=color,plty=1,plwd=5,title="Radar Chart")

legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2","Cluster 3"),
                 seg.len=-1.4,title="Clústeres",pch=21,bty="n" ,lwd=3, y.intersp=1, 
                 horiz=FALSE,col=color)

library(knitr)
img <- include_graphics("Rplot.png")
img

print("Es posible observar que segun el cluster de interes los valores e  interpretacion varian de gran manera,por ejemplo,el primer cluste toma en consideracion valores como el dia.horas de nieved y la velocidad del viento y estas son relevantes,sin embargo clusters como el tercero poseen valores mayores en las temperaturas,dia y año,ademas,que el valor de la concentracion de la particula es un poco menor en comparacion e incluso sumamente baja si se toma en cuenta el segundo cluster")

library(factoextra)

datos_cluster <- datos[,-5]
datos <- na.omit(datos[, -5])

#jambu
jambu <- c()
for(k in 1:10){
  kmeans_model <- kmeans(datos, centers=k, iter.max=100, nstart=5)
  jambu[k] <- kmeans_model$tot.withinss
}

plot(1:10, jambu, type="b", pch=20, main="Codo de Jambu")


#silhouette

silhouette <- fviz_nbclust(datos, kmeans, method="silhouette", k.max=10, nstart=5, iter.max=100)
print(silhouette)

print("Se puede observar que tanto el codo de janbu,como el silhouette sugieren la utilización de dos clusters")
