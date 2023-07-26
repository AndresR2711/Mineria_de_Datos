library("FactoMineR")
library("factoextra")
library("factoextra")
library("corrplot")
library("car")
library("dplyr")


# ========================
#         Ejercicio 1
# ========================
#a)

setwd("/Users/Andres Rojas/Documents/Tarea3")
datos <- read.csv("SpotifyTop2018_40_V2.csv")

#Calcular el resumen numérico
summary(datos)
#Con loudness
print(
  "Tal comno es posible observar en los datos de la variable loudness las canciones poseen un rango de entre -9.211 y -3.093 db ,con una mediana de -5.930 y una media de -5.846,lo que indica que el sonido de las canciones es moderado,con algunas variaciones dependiendo del artista o genero"
)
#Con tempo
print(
  "Tempo posee un rango de entre 77.17 y 191.70 bpm con una mediana de 122.53 y una media de 122.11,esto indica que usualmente las cancions reproducidas por los usuarios tienen un tempo alto,que es caracteristico de canciones energeticas"
)
#b)
plot(datos$danceability, datos$energy)
print(
  "Es posible interpretar que las canciones con gran bailabilidad poseen tambien una intensidad alta"
)
#c)
boxplot(datos$danceability, datos$speechiness)
print(
  "tal como es posible observar tanto la variable danceability como speechiness poseen datos atipicos,en el caso de danceability son 0.258 y 0.284,mientras que para speechiness hay unicamente uno que es 0.516"
)
#for (i in 1:ncol(datos)) {
# box <- boxplot(datos[,i], plot = FALSE)$out
#if (length(box) > 0) {
# cat("Valores atípicos en la variable '", colnames(datos)[i], "' :", box, "\n")
#}
#}
#d)
matriz <- cor(datos)

corrplot(
  matriz,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.srt = 45
)

print(
  "La correlacion entre energy y loudness es bastante alta a como se puede observar con el ejemplo anterior,lo que sugiere que las canciones con alta energia tambien tienden a tener una sonoridad alta"
)
print(
  "La correlacion entre valence y danceability es fuerte,lo que indica que las canciones positivas tambien tienden a ser bailables"
)


#e)

pca <- prcomp(datos, center = TRUE, scale. = TRUE)
pca

res <- PCA(datos[, 1:10], scale.unit = TRUE, graph = FALSE)
res
plot(
  res,
  axes = c(1, 2),
  choix = "var",
  col.var = "blue",
  new.plot = TRUE,
  select = "cos2 0.05"
)


fviz_pca_var(
  pca,
  col.var = "blue",
  col.ind = "black",
  repel = TRUE,
  axes = c(1, 2),
  # seleccionamos los componentes a graficar
  geom = c("point", "text"),
  title = "Círculo de correlaciones"
)
print(
  "Es posible observar que las variables con una correlacion alta se encuentran cercanas al circulo,mientras que las bajas,cerca al centro del circulo"
)


fviz_pca_biplot(
  pca,
  col.ind = "cos2",
  repel = TRUE,
  axes = c(1, 3),
  title = "Plano de los componentes 1 y 3"
)
print(
  "Tal como podemos observar,humble y in my feelings estan en dos diferentes cluster,lo que indica que tienen caracteristicas diferentes"
)
print(
  "Por el otro lado,las demas canciones mencionadas estan mal representadas en los componentes 1 y 2 a razon de que la variabilidad no es la misma que en las que estas canciones destacan "
)
# ========================
#         Ejercicio 2
# ========================

setwd("/Users/Andres Rojas/Documents/Tarea3")
datos <- read.csv("TablaAffairs.csv", header = TRUE, sep = ";")
summary(datos)
print(
  "Si analizamos la variable edad podemos llegar a la conclusion de que la gran parte de las personas que se casaron lo hicieron alrededor de los 32 años,al encontrarse con una mediana de 32.00   y una media de 32.49   "
)

str(datos)

data <- subset(datos, select = -c(1, 3, 6, 10))


data
matriz <- cor(data)
matriz
corrplot(
  matriz,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.srt = 45
)

print(
  "Con las variables de edad y años de casado se puede intuir que a mayor edad,mayor años de casado,esto a razon de que poseen una correlacion postiva fuerte"
)
print(
  "Por otro lado,con las variables educacion y ocupacion se puede observar existe una correlacion positiva moderada,lo que indica que a mayor grado educativo mayor será la ocupacion que este posea"
)


pca <- prcomp(data, center = TRUE, scale. = TRUE)
pca

res <- PCA(data[, 1:6], scale.unit = TRUE, graph = FALSE)
res
plot(
  res,
  axes = c(1, 2),
  choix = "var",
  col.var = "blue",
  new.plot = TRUE,
  select = "cos2 0.05"
)
#3)
print(
  "Tanto las correlaciones de educacion como de ocupacion son fuertes,mientras que algunas otras como tiempo fiel y religiosa al estar mas cerca del centro del circulo poseenuna correlacion mas debil"
)


#d)

datos
datos$Genero <- as.numeric(datos$Genero)
datos$Hijos <- as.numeric(datos$Hijos)


#datos$Genero <- as.factor(datos$Genero)


# ========================
#         Ejercicio 3
# ========================

datos <- read.csv("SAheart.csv", header = TRUE, sep = ";")
datos
numericos <- datos[, sapply(datos, is.numeric)]
numericos


res <- PCA(numericos,
           scale.unit = TRUE,
           ncp = 5,
           graph = FALSE)
res
# 1
#Individuos
fviz_pca_ind(
  res,
  pointsize = 1,
  pointshape = 1,
  fill = "#E7B800",
  label = "none",
  repel = TRUE,
  select.ind = list(cos2 = 0.05),
  axes = c(1, 2)
)

# imagen
knitr::include_graphics("Cluster.png")

#2
fviz_pca_var(
  res,
  col.var = "steelblue",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  select.ind = list(cos2 = 0.05)
)
print(
  "valores como la obecidad y adiposity se encuentran altamente corelacionados,al igual que otras variables que tienen aspectos en comun como lo puede ser el alcohol y el tobacco"
)
#3
fviz_pca_biplot(
  res,
  col.var = "#2E9FDF",
  col.ind = "#696969",
  select.ind = list(cos2 = 0.05),
  axes = c(1, 2)
)

print(
  "La formacion de los cluster se puede relacionar a los grupos ubicados en la parte superior derecha,quietenes tienenaltos valores de sbd,tobacco,alcohol y age,es decir,todos manejan algun tipo de dato en comun,lo que genera que se realicen agrupacione que son mas evidentes con la superposicion"
)

#b)
datos <- datos %>%
  dplyr::mutate(famhist = dplyr::recode(famhist,
                                        "Present" = 1,
                                        "Absent" = 0))
datos <- datos %>%
  dplyr::mutate(chd = dplyr::recode(chd,
                                    "Si" = 1,
                                    "No" = 0))


res <- PCA(datos,
           scale.unit = TRUE,
           ncp = 5,
           graph = FALSE)
res



fviz_pca_ind(
  res,
  pointsize = 1,
  pointshape = 1,
  fill = "#E7B800",
  label = "none",
  repel = TRUE,
  select.ind = list(cos2 = 0.05),
  axes = c(1, 2)
)
# imagen
knitr::include_graphics("Cluster2.png")

#2
fviz_pca_var(
  res,
  col.var = "steelblue",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  select.ind = list(cos2 = 0.05)
)
print(
  "Es posible observar una reduccion en la correlacion de los variables como tobacco y alcohol,por la presencia de dos variables mas en comparacion al intento anterior,aunque de igualmente la correlacion es fuerte ya no es tan evidente"
)

#3
fviz_pca_biplot(
  res,
  col.var = "#2E9FDF",
  col.ind = "#696969",
  select.ind = list(cos2 = 0.05),
  axes = c(1, 2)
)
knitr::include_graphics("pic.png")
print("A pesar de que los valores continuan agrupandose en el mismo cluster ahora se encuentran de alguna manera mejor distribuidos y con una correlacion mas debil en comparacion al primer intento. A pesar de esto se denota que tobacco y alcohol siguen siendo positivos y de los principales")
#4
print("En lo personal me parece mas interesante el segundo acp,a razon de que esperaba cambios menores en la sobre posicion del circulo y mas notable en los clusters,y termino siendo de manera contraria de cierta manera,a razon de que el cambio es mas notorio a mi parece en el circulo")