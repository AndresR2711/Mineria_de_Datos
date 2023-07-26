install.packages("traineR")
install.packages("ggplot")
library(traineR)
library(ggplot2)
library(knitr)
setwd("/Users/AndresR/Documents/Tarea6")
#Para  formatear el doc es Ctrl + Shift + A

# ========================
#         Ejercicio 1
# ========================


parte1 <- include_graphics("Matriz.png")
parte1

parte2 <- include_graphics("Parte2.png")
parte2
# ========================
#         Ejercicio 2
# ========================

ejercicio1 <- function(matrizConfu) {
  VN <- matrizConfu[1, 1]
  FN <- matrizConfu[2, 1]
  FP <- matrizConfu[1, 2]
  VP <- matrizConfu[2, 2]
  precision_global <- (VN + VP) / (VN + FP + FN + VP)
  error_global <- 1 - precision_global
  precision_positiva <- VP / (FN + VP)
  precision_negativa <- VN / (VN + FP)
  falsos_positivos <- FP / (VN + FP)
  falsos_negativos <- FN / (FN + VP)
  asertividad_positiva <- VP / (FP + VP)
  asertividad_negativa <- VN / (VN + FN)
  metricas <- list(
    precision_global = precision_global,
    error_global = error_global,
    precision_positiva = precision_positiva,
    precision_negativa = precision_negativa,
    falsos_positivos = falsos_positivos,
    falsos_negativos = falsos_negativos,
    asertividad_positiva = asertividad_positiva,
    asertividad_negativa = asertividad_negativa
  )
  
  
  return(metricas)
}

matrizConfu <-
  matrix(c(892254, 212, 8993, 300), nrow = 2, byrow = TRUE)

matrizConfu

prueba <- ejercicio1(matrizConfu)

prueba

print(
  "En el modelo se puede observar una gran cantidad de posibles falsos positivos,lo que podria traer problemas al trabajar con el mismo. Por lo tanto,el modelo debería de ser probado y según los resultados que arroje decidir si es pertinente para el caso en especifico"
)


# ========================
#         Ejercicio 3
# ========================
#profe
equilibrio.variable.predecir <-
  function(datos,
           variable.predecir,
           ylab = "Cantidad de individuos",
           xlab = "",
           main = paste("Distribución de la variable", variable.predecir),
           col = NA) {
    gg_color <- function (n) {
      hues <- seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    if (missing(variable.predecir) |
        !(variable.predecir %in% colnames(datos))) {
      stop("variable.predecir tiene que ser ingresada y ser un nombre de columna",
           call. = FALSE)
    }
    if (is.character(datos[, variable.predecir]) |
        is.factor(datos[, variable.predecir])) {
      if (length(col) == 0 || is.na(col)) {
        col <- gg_color(length(unique(datos[, variable.predecir])))
      } else{
        col <- rep(col, length(unique(datos[, variable.predecir])))
      }
      ggplot(
        data = datos,
        mapping = aes_string(x = variable.predecir, fill = variable.predecir)
      ) +
        geom_bar() +
        scale_fill_manual(values = col, name = variable.predecir) +
        labs(x = xlab, y = ylab, title = main) +
        theme_minimal() +
        theme(legend.position = "bottom")
    } else{
      stop("La variable a predecir tienen que ser de tipo factor o character",
           call. = FALSE)
    }
  }


# Ejercicio 3.1

Datos <-
  read.csv(
    'Tumores.csv',
    header = TRUE,
    sep = ',',
    dec = '.',
    stringsAsFactors = T
  )

Datos$tipo <- factor(Datos$tipo)
str(Datos)
equilibrio.variable.predecir(Datos, "tipo")
numero.filas <- dim(Datos)[1]
muestra <- sample(1:numero.filas, numero.filas * 0.25)
ttesting <- Datos[muestra,]
taprendizaje <- Datos[-muestra,]

modelo <-
  train.knn(tipo ~ ., data = taprendizaje, kmax = floor(sqrt(numero.filas)))
modelo

prediccion <- predict(modelo, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)

# 3.2

# ========================
#       Rectangular
# ========================

modelo_rectangular <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "rectangular")
modelo_rectangular

prediccion <- predict(modelo_rectangular, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)

# ========================
#     Triangular
# ========================

modelo_triangular <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "triangular")
modelo_triangular

prediccion <- predict(modelo_triangular, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)

# ========================
#       Epanechnikov
# ========================

modelo_epanechnikov <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "epanechnikov")
modelo_epanechnikov

prediccion <- predict(modelo_epanechnikov, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)

# ========================
#       Biweight
# ========================

modelo_biweight <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "biweight")
modelo_biweight

prediccion <- predict(modelo_biweight, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)

# ========================
#       Triweight
# ========================
modelo_triweight <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "triweight")
modelo_triweight


prediccion <- predict(modelo_triweight, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)


# ========================
#           Cos
# ========================

modelo_cos <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "cos")
modelo_cos


prediccion <- predict(modelo_cos, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)

# ========================
#           Inv
# ========================

modelo_inv <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "inv")
modelo_inv


prediccion <- predict(modelo_inv, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)

# ========================
#       Gaussian
# ========================
modelo_gaussian <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "gaussian")
modelo_gaussian


prediccion <- predict(modelo_gaussian, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)


# ========================
#       Optimal
# ========================
modelo_optimal <-
  train.knn(tipo ~ .,
            data = taprendizaje,
            kmax = floor(sqrt(numero.filas)),
            kernel = "optimal")
modelo_optimal


prediccion <- predict(modelo_optimal, ttesting, type = "class")
head(prediccion$prediction)
MC <- confusion.matrix(ttesting, prediccion)
general.indexes(mc = MC)


print(
  " Los nucleos en que se obtiene un valor mas cercano a 1 son los del optimal y triangular. A razon de que ambos poseen valores del 0.98,e incluso redondeables a 0.99"
)

# ========================
#         Ejercicio 4
# ========================

datos <- read.csv("titanicV2020.csv", header = TRUE, sep = ",")
datos


str(datos)


datos$Survived <- factor(datos$Survived, levels = c(1, 0))
datos$Pclass <- factor(datos$Pclass, levels = c(1, 2, 3))
datos$Sex <- factor(datos$Sex, levels = c("female", "male"))
datos$Embarked <- factor(datos$Embarked, levels = c("C", "Q", "S"))


# datos
#"PassengerId", "Ticket" y "Cabin" se pueden ignorar tambien
#se ignora la variable "Name"


datos <-
  datos[, c("Survived",
            "Pclass",
            "Sex",
            "Age",
            "SibSp",
            "Parch",
            "Fare",
            "Embarked")]

datos
# Resumen numérico
summary(datos)
# str(datos)

hist(datos$Age, main = "Histograma de la  Edad", xlab = "Age")
print(
  "Con el uso de un histograma se puede observar de manera mas sencilla el rango de las edades,donde tal como se puede observar había gran cantidad de personas jovenes,de entre 17 y 30 años,donde se encuentra una gran agrupacion de los datos"
)

# Boxplot de las variables numéricas
boxplot(datos[, c("Age", "SibSp", "Parch", "Fare")], main = "Boxplot de Variables Numéricas")
print(
  "Mediante el uso del bloxplot se es capaz de observar los datos atípicos,por ejemplo,en el caso de la variable Fare se encuentra un valor en 500,el cual probablemente sea un error de digitacion al observar que en comparacion a los demas del mismo sector se ubican en maximo de alrededor de 300 "
)

# Correlación entre variables
cor(datos[, c("Age", "SibSp", "Parch", "Fare")])

print(
  " A su vez,con el calculo de correlaciones se es capaz de observar que la cantidad Cantidad de hermanos o conyuges a bordo del Titanic(SibSp) está relacionada con la Cantidad de padres o hijos a bordo del Titanic(Parch) al estas tener una correlacion de 0.3735872"
)
# Gráfico Pclass
library(ggplot2)


ggplot(datos, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge")

print(
  "Es posible observar en el grafico que gran cantidad de personas en la tercer clase no fueron capaces de sobrevevir,mientras que en las clases 1 y 2 se mantienen unos valores mucho mas equilibrados entre los que lo lograron y los que no"
)

# Gráfico Sex
ggplot(datos, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge")

print(
  "En este gráfico es posible observar que entre los hombres predominan los que no lograron sobrevivir,mientras que por el lado de las mujeres este valor es contrario,donde predominan las que lograron sobrevivir,sin embargo,es importante tomar en cuenta que en el barco habían muchos más hombres que mujeres."
)

# Gráfico Embarked
ggplot(datos, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge")

print(
  "Es pósible observar que entre los pasajeros que embarcaron en Southampton predominan los que no sobrevivieron,mientras que los otros dos puertos mantienen valores equilibrados. "
)

# Ejercicio 4.3


table(datos$Survived)

print(
  "La salida  muestra que hay 815  pasajeros que no sobrevivieron y 494 pasajeros que sobrevivieron."
)


print(
  "Se puede concluir que este problema de predicción está desequilibrado, ya que la proporción de pasajeros que sobrevivieron es significativamente menor que la proporción de pasajeros que no sobrevivieron. Esto Puede afectar a la prediccion de la parte minoritaria."
)


# Ejercicio 4.4

muestra80 <- sample(nrow(datos), 0.8 * nrow(datos))

taprendizaje <- datos[muestra80, ]
taprendizaje
ttesting <- datos[-muestra80, ]
ttesting


taprendizaje <- na.omit(taprendizaje)
ttesting <- na.omit(ttesting)


modelo <- train.knn(Survived ~ ., data = taprendizaje)
modelo

prediccion <- predict(modelo, ttesting, type = "class")
head(prediccion$prediction)
length(prediccion)

MC <- confusion.matrix(ttesting, prediccion)
MC

general.indexes(mc = MC)

print("Global")
Global <- sum(diag(MC)) / sum(MC)
Global
print("categoría")
individual <- diag(MC) / rowSums(MC)



print(MC)
print(paste("Precisión global:", round(Global, 3)))
print(paste("Precisión categoría 0 (No sobrevivio):", round(individual[1], 3)))
print(paste("Precisión categoría 1 (Sobrevivio):", round(individual[2], 3)))


print(
  " La precisión global del modelo es de 0.837, esto indica que el modelo clasifica de manera corecta al 83 porciento de los pasajeros aproximadamente. A su vez es de 0.779 para los pasajeros que nos sobrevivieron,mientras que para los que sí es de 0.88. Lo que indica que el modelo predice correctamente al 90 porciento de los que sí sobrevivieron"
)
print(
  "Tal como podemos observar el modelo es bastante aceptable,aunque se podrían probar algunos otros valores con el fin de intentar mejorarlo"
)


# Ejercicio 4.5

datos2 <- datos[, c("Survived", "Pclass", "Sex", "Age", "Embarked")]



taprendizaje <- datos2[muestra80, ]
taprendizaje
ttesting <- datos2[-muestra80, ]
ttesting


taprendizaje <- na.omit(taprendizaje)
ttesting <- na.omit(ttesting)


modelo2 <- train.knn(Survived ~ ., data = taprendizaje)
modelo2

prediccion <- predict(modelo2, ttesting, type = "class")
head(prediccion$prediction)
length(prediccion)

MC2 <- confusion.matrix(ttesting, prediccion)
MC2

general.indexes(mc = MC2)

print("Global")
Global <- sum(diag(MC2)) / sum(MC2)
Global
print("categoría")
individual <- diag(MC2) / rowSums(MC2)



print(MC2)
print(paste("Precisión global:", round(Global, 3)))
print(paste("Precisión categoría 0 (No sobrevivio):", round(individual[1], 3)))
print(paste("Precisión categoría 1 (Sobrevivio):", round(individual[2], 3)))



print(
  "Se puede observar que los valores de pasajeros que sobrevivieron aumentaron,mientras que los de los que no sobrevivieron decayeron en cierta medida"
)

# Ejercicio 4.6

Punto4 <- ejercicio1(MC)
Punto5 <- ejercicio1(MC2)

df <- data.frame(t(unlist(Punto4)))
df
df2 <- data.frame(t(unlist(Punto5)))
df2

names(df) <-
  c(
    "precision_global",
    "error_global",
    "precision_positiva",
    "precision_negativa",
    "falsos_positivos",
    "falsos_negativos",
    "asertividad_positiva",
    "asertividad_negativa"
  )

names(df2) <-
  c(
    "precision_global",
    "error_global",
    "precision_positiva",
    "precision_negativa",
    "falsos_positivos",
    "falsos_negativos",
    "asertividad_positiva",
    "asertividad_negativa"
  )

Salida <- rbind(df, df2)
Salida

modelos <- include_graphics("modelos.png")
modelos
print(
  "Tal como es posible observar realmente las diferencias entre un modelo y otro son poco notorías,pero aún así varían,por ejemplo, el valor de precision positiva vario un 2 porciento en relación a los datos del modelo 1"
)



# ========================
#         Ejercicio 5
# ========================
datos <-
  read.table(
    "ZipData_2020.csv",
    header = TRUE,
    sep = ';',
    dec = '.',
    stringsAsFactors = T
  )

#5.2
table(datos$Numero)

print(
  "Se puede observar que los valores son equilibrados, ya que tenemos cantidades parecidas de cada categoria"
)

#5.3

dimension <- dim(datos)

n <- dimension[1]

muestra <- sample(1:n, floor(n * 0.20))

ttesting <- datos[muestra, ]

taprendizaje <- datos[-muestra, ]


modelo <- train.knn(Numero ~ .,
                    data = (taprendizaje),
                    kmax = floor(sqrt(n)))
modelo


prediccion <- predict(modelo, ttesting, type = "class")

prediccion

MC <- confusion.matrix(ttesting, prediccion)
MC
general.indexes(mc = MC)

MC

print(
  "Es posible observar que los valores por categoria en su mayoria se mantienen cerca o incluso superior al 90 porciento,algunos llegando al 98 por ciento de precisión. Estos valores al ser altos significa que son son buenos predictores"
)
