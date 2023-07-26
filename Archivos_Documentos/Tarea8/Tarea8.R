#Para  formatear el doc es Ctrl + Shift + A
setwd("/Users/AndresR/Documents/Tarea8")
library(traineR)
library(caret)
library(knitr)
library(rpart.plot)
library(dplyr)

indices.general <- function(MC) {
  precision.global <- sum(diag(MC)) / sum(MC)
  error.global <- 1 - precision.global
  precision.categoria <- diag(MC) / rowSums(MC)
  res <-
    list(
      matriz.confusion = MC,
      precision.global = precision.global,
      error.global = error.global,
      precision.categoria = precision.categoria
    )
  names(res) <-
    c("Matriz de Confusión",
      "Precisión Global",
      "Error Global",
      "Precisión por categoría")
  return(res)
}

# ========================
#         Ejercicio 1
# ========================

datos <-
  read.csv(
    'Tumores.csv',
    header = TRUE,
    sep = ',',
    dec = '.',
    stringsAsFactors = T
  )
datos$tipo <- factor(datos$tipo)
datos
muestra <-
  createDataPartition(datos$tipo,
                      times = 1,
                      p = 0.25,
                      list = FALSE)
ttesting <- datos[muestra, ]
# ttesting
taprendizaje <- datos[-muestra, ]
# taprendizaje

modelo       <-
  train.rpart(tipo ~ ., data = taprendizaje, minsplit = 2)
prediccion   <- predict(modelo, ttesting, type = 'class')
mc           <- confusion.matrix(newdata = ttesting, prediccion)

general.indexes(mc = mc)


modelo <- train.rpart(
  formula = tipo ~ varianza + asm + entropia ,
  data = taprendizaje,
  minsplit = 2
)
modelo

prp(
  modelo,
  extra = 104,
  branch.type = 2,
  box.col = c("pink",
              "palegreen3",
              "cyan")[modelo$frame$yval]
)

# ========================
#         Ejercicio 2
# ========================

# ========================
print("Ejercicio 2.1")
# ========================

datos <-
  read.table(
    "titanicV2020.csv",
    header = TRUE,
    sep = ',',
    dec = '.',
    stringsAsFactors = T
  )
datos <- subset(datos, select = -imagen)
datos
# Se oimiten valores innecesarios para el analsis
datos <-
  datos[, c("Survived",
            "Pclass",
            "Sex",
            "Age",
            "Fare")]

datos <- na.omit(datos)
datos

datos$Survived <- factor(datos$Survived, levels = c(1, 0))
datos$Pclass <- factor(datos$Pclass, levels = c(1, 2, 3))
datos$Sex <- factor(datos$Sex, levels = c("female", "male"))


pasarEnteros <- sapply(datos, is.integer)
datos[pasarEnteros] <- lapply(datos[pasarEnteros], as.factor)
# summary(datos)
# datos
# ========================
print("Ejercicio 2.2")
# ========================
tam <- dim(datos)
n   <- tam[1]

muestra      <- sample(1:n, floor(n * 0.8))
ttesting     <- datos[-muestra, ]
taprendizaje <- datos[muestra, ]

# ========================
print("Ejercicio 2.3")
# ========================

tabla.comparacion <- data.frame()

modelo       <-
  train.rpart(Survived ~ ., data = taprendizaje, minsplit = 2)
prediccion   <- predict(modelo, ttesting, type = 'class')
mc           <- confusion.matrix(newdata = ttesting, prediccion)

indices.general(mc)

#Grafico del arbol
prp(
  modelo,
  extra = 104,
  branch.type = 2,
  box.col = c("pink",
              "palegreen3",
              "cyan")[modelo$frame$yval]
)


# datos

# Con selección de variables
modelo <-
  train.rpart(formula = Survived ~ Sex +   Pclass  ,
              taprendizaje,
              minsplit = 2)
prp(
  modelo,
  extra = 104,
  branch.type = 2,
  box.col = c("pink",
              "palegreen3",
              "cyan")[modelo$frame$yval]
)


valores <-
  c((general.indexes(mc = mc)[c(-1, -4)]), (unlist(general.indexes(mc = mc)[4])))
valores$modelo <- "rpart"
tabla.comparacion <- rbind(tabla.comparacion, valores)
tabla.comparacion

# ========================
print("Ejercicio 2.4")
# ========================


indices.comparacion <- function(kernels) {
  for (k in kernels) {
    if (k == "linear" ||
        k == "radial" || k == "polynomial" || k == "sigmoid") {
      modelo <- train.svm(Survived ~ ., taprendizaje, kernel = k)
      prediccion <- predict(modelo, ttesting, type = "class")
      MC <- confusion.matrix(ttesting, prediccion)
      modelo <-
        c((general.indexes(mc = MC)[c(-1, -4)]), (unlist(general.indexes(mc = MC)[4])))
      modelo$modelo <- k
      tabla.comparacion <- rbind(tabla.comparacion, modelo)
    } else{
      modelo <- train.knn(Survived ~ ., taprendizaje, kernel = k)
      prediccion <- predict(modelo, ttesting, type = "class")
      MC <- confusion.matrix(ttesting, prediccion)
      modelo <-
        c((general.indexes(mc = MC)[c(-1, -4)]), (unlist(general.indexes(mc = MC)[4])))
      modelo$modelo <- k
      tabla.comparacion <- rbind(tabla.comparacion, modelo)
    }
    
  }
  
  return(tabla.comparacion)
}



listaModelos <- c(
  "linear",
  "radial",
  "polynomial",
  "sigmoid",
  "epanechnikov",
  "triangular",
  "biweight",
  "triweight",
  "cos",
  "inv",
  "gaussian",
  "optimal",
  "rectangular"
)


modelos_a_compararar <- indices.comparacion(listaModelos)

modelos_a_compararar <-
  select(modelos_a_compararar, modelo, everything())
modelos_a_compararar


round(0.8181818, 2)
round(0.8133971, 2)

print(
  "Se puede observar que el modelo que se estaba utilizando en este caso(rpart) posee un mayor valor de precision general,al este poseer un 0.82 en este,mientras que en comparacion a los demas modelos el que mas se acerca son el inv  y el epanechnikov,donde ambos empatan en 0.81"
)
# ========================
#         Ejercicio 3
# ========================

datos <-
  read.csv(
    'ZipData_2020.csv',
    header = TRUE,
    sep = ';',
    dec = '.',
    stringsAsFactors = TRUE
  )

datos

tam <- dim(datos)
n   <- tam[1]

indice <- createDataPartition(datos$Numero, p = 0.8, list = FALSE)
# indice
ttesting     <- datos[-indice, ]
# ttesting
taprendizaje <- datos[indice, ]
# taprendizaje

# Genera el modelo
modelo       <-
  train.rpart(Numero ~ ., data = taprendizaje, minsplit = 2)
prediccion   <- predict(modelo, ttesting, type = 'class')
mc           <- confusion.matrix(newdata = ttesting, prediccion)
# Índices de Calidad de la predicción
indices.general(mc)


prp(
  modelo,
  extra = 104,
  branch.type = 2,
  box.col = c("pink",
              "palegreen3",
              "cyan")[modelo$frame$yval]
)



print(
  "En este caso se puede observar que los resultados son buenos,pero podrian ser mejores.En la gran mayoria de casos,no obstante se puede obeservar que en algunos casos se denota que las ramas al ser mas delgadas poseen una menor cantidad de datos,lo que denota la diferencia entre unos datos y otros,asi como lo es entre las variables 134 y 199 con el si y no de cada uno respectivamente. A su vez,se observa que las prediccion tienden a poseer valores algo bajos,al estar todos por debajo del 20 por ciento"
)
# Con selección de variables
modelo <- train.rpart(formula = Numero ~ V122 + V73,
                      data = taprendizaje,
                      minsplit = 2)
prp(
  modelo,
  extra = 104,
  branch.type = 2,
  box.col = c("pink",
              "palegreen3",
              "cyan")[modelo$frame$yval]
)
print(
  "Por otro lado,en este otro arbol se observa que los valores predictivos mejoran en cierto punto,con el 0 teniendo un 39 por ciento y teniendo datos y ramas mas similares en cuando a ancho"
)

mc <-  confusion.matrix(ttesting, prediccion)




indices.general(mc)

imagen1 <- include_graphics("Anterior.jpg")
imagen1
imagen2 <- include_graphics("Actual.jpg")
imagen2

print(
  "
En comparacion a tareas anteriores los datos y formas de emplearlos son basicamente las mismas,aunque en comparacion a los calculos anteriores se dieron algunos cambios,por ejemplos los valores predictivos en general se vieron dismuinuidos,esto afecta de gran manera la precision global del modelo utilizado,por ejemplo,podemos observar que actualmente este posee 0.7292341 de precision global,mientras que en ejercicios anteriores esta precision era de 0.9677419,que como podemos observar es considerablemente mas alta
"
)


# ========================
#         Ejercicio 4
# ========================


print("Variable Color")



# Variable inicial Color
#
print("Color Amarrillo")
print("Amarillo: 3 de tipo 1- 2G,1P (3/4)")
print("Amarillo: 1 de tipo 0- 1P (1/4)")


Amarillo <- 1 - (3 / 4) ^ 2 - (1 / 4) ^ 2
Amarillo
# 0.375
print("Color Azul")

print("Azul:2 de tipo 1- 1P,1G (2/7)")
print("Azul:5 de tipo 0-3G,2P (5/7)")
Azul = 1 - (2 / 7) ^ 2 - (5 / 7) ^ 2
Azul
# 0.396

Split <- (4 / 11) * 0.375 + (7 / 11) * 0.396
Split
# 0.3883636
# round(0.3883636,2)
# 0.39

# print("-----------------------")
print("Variable Tamano")
# print("-----------------------")

print("Grande")
print("3:1 y 3:0")


Grande <- 1 - (3 / 6) ^ 2 - (3 / 6) ^ 2
Grande
# 0.5


print("Pequeño")
print("2:1 y 3:0")

Pequeno <-  1 - (2 / 5) ^ 2 - (3 / 5) ^ 2
Pequeno
# 0.48

Split <- (6 / 11) * 0.5 + (5 / 11) * 0.48
Split
# 0.4909091
# round(0.4909091,3)
# 0.491
print(
  "Como podemos observar la impureza es mayor para tamaño,al esta poseer un valor de 0.491,mientras que la clase color es de 0.39. Es por esto por lo que la division por color será mejor"
)

# EJERCICIO 4.2

# datos <- data.frame(
#   Tipo = c(1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0),
#   Color = c("Amarillo", "Amarillo", "Amarillo", "Azul", "Azul", "Azul", "Azul", "Amarillo", "Azul", "Azul", "Azul"),
#   Tamaño = c("Grande", "Grande", "Pequeño", "Pequeño", "Grande", "Grande", "Pequeño", "Pequeño", "Pequeño", "Grande", "Grande")
# )
# datos

# filtrado<- subset(datos, Tamaño == "Pequeño"&Color == "Azul")
# filtrado
arbol <- include_graphics("Arbol.PNG")
arbol
