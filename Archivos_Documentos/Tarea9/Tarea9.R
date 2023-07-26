#Para  formatear el doc es Ctrl + Shift + A
setwd("/Users/AndresR/Documents/Tarea9")


library(caret)
library(traineR)
library(ggplot2)
library(randomForest)
library(ada)
library(xgboost)

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

# Índices para matrices NxN
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
# Bosques aleatorios

# Cargar la tabla de datos
datos <- read.csv("tumores_V2.csv", header = T)
datos$tipo <- as.factor(datos$tipo)
datos <- subset(datos, select = -imagen)
datos
set.seed(123)

na.omit(datos)

muestra <- createDataPartition(datos$tipo, p = 0.75, list = FALSE)
ttesting <- datos[-muestra, ]
taprendizaje <- datos[muestra, ]


modeloRandomF <-
  train.randomForest(tipo ~ .,
                     data = taprendizaje,
                     importance = T,
                     ntree = 500)
modeloRandomF
# varImp(modelo)

varImpPlot(modeloRandomF)

prediccion <- predict(modeloRandomF, ttesting, type = "class")
MCRandomF <- confusion.matrix(ttesting, prediccion)
indices_bosques <- indices.general(MCRandomF)
indices_bosques
general.indexes(mc = MCRandomF)




# Potenciacion


modeloPotencia <-
  train.ada(tipo ~ ., data = taprendizaje, iter = 500)
modeloPotencia
prediccion <- predict(modeloPotencia, ttesting)
plot(modeloPotencia, TRUE, TRUE)
MCPotencia <- confusion.matrix(ttesting, prediccion)
indices_potenciacion  <- indices.general(MCPotencia)
indices_potenciacion

# eXtreme Gradient Boosting
str(datos)

modeloXgb <-
  train.xgboost(
    formula = tipo ~ media + varianza + desviacion.estandar + entropia + asimetria + kurtosis + contraste + energia + asm + homogeneidad + disiminitud + correlacion + psnr + ssim + mse + dc ,
    data = taprendizaje,
    nrounds = 500,
    verbose = F
  )

prediccion <- predict(modeloXgb, ttesting , type = "class")
MCXgb <- confusion.matrix(ttesting, prediccion)
# MCXgb


indices_xgboost <- indices.general(MCXgb)
indices_xgboost

#Importancia de las Variables
#Importancia de las Variables
variables.importantes <-
  xgb.importance(feature_names = colnames(
    c(
      "media",
      "varianza",
      "desviacion.estandar",
      "entropia",
      "asimetria",
      "kurtosis",
      "contraste",
      "energia",
      "asm",
      "homogeneidad",
      "disiminitud",
      "correlacion",
      "psnr",
      "ssim",
      "mse",
      "dc"
    )
  ), model = modeloXgb)
xgb.plot.importance(importance_matrix = variables.importantes)

resultados_bosques <- indices.general(MCRandomF)
resultados_potenciacion <- indices.general(MCPotencia)
resultados_xgboost <- indices.general(MCXgb)

tabla.comparacion <- data.frame()
# datos <- subset(datos, select = -imagen)
# datos


bosques <-
  c((resultados_bosques[c(-1,-4)]), (unlist(resultados_bosques[4])))
potenciacion <-
  c((resultados_potenciacion[c(-1,-4)]), (unlist(resultados_potenciacion[4])))
xgboost <-
  c((resultados_xgboost[c(-1,-4)]), (unlist(resultados_xgboost[4])))

tabla.comparacion <- rbind(bosques, potenciacion, xgboost)
tabla.comparacion

print(
  "Tal como se puede observar, el valor que posee una mayor precision es el de bosques,al este tener una precision de 1,mientras que el peor es el xgboost. En comparacion a otras tareas se puede observar que los valores son un poco mas precisos,por ejemplo en la tarea 8,el valor fue de 0.9656 con el uso de rpart "
)

# ========================
#         Ejercicio 2
# ========================

# Cargar el archivo CSV
datos <-
  read.table(
    "titanicV2020.csv",
    header = TRUE,
    sep = ',',
    dec = '.',
    stringsAsFactors = T
  )
# datos

# Se oimiten valores innecesarios para el analsis
datos <-
  datos[, c("Survived",
            "Pclass",
            "Sex",
            "Age",
            "Fare")]

datos <- na.omit(datos)


datos$Survived <- factor(datos$Survived, levels = c(1, 0))
datos$Pclass <- factor(datos$Pclass, levels = c(1, 2, 3))
datos$Sex <- factor(datos$Sex, levels = c("female", "male"))
datos

tam <- dim(datos)
n   <- tam[1]

muestra      <- sample(1:n, floor(n * 0.8))
ttesting     <- datos[-muestra,]
taprendizaje <- datos[muestra,]


modeloRandomF <-
  train.randomForest(Survived ~ .,
                     data = taprendizaje,
                     importance = T,
                     ntree = 600)
# modeloRandomF
# varImp(modelo)

varImpPlot(modeloRandomF)

prediccion <- predict(modeloRandomF, ttesting, type = "class")
MCRandomF <- confusion.matrix(ttesting, prediccion)
indices_bosques <- indices.general(MCRandomF)
indices_bosques
# general.indexes(mc = MCRandomF)




# Potenciacion


modeloPotencia <-
  train.ada(Survived ~ ., data = taprendizaje, iter = 600)
modeloPotencia
prediccion <- predict(modeloPotencia, ttesting)
plot(modeloPotencia, TRUE, TRUE)
MCPotencia <- confusion.matrix(ttesting, prediccion)
indices_potenciacion  <- indices.general(MCPotencia)
indices_potenciacion

# eXtreme Gradient Boosting
str(datos)

modeloXgb <-
  train.xgboost(
    formula =  Survived ~ Pclass   + Sex      + Age      + Fare     ,
    data = taprendizaje,
    nrounds = 600,
    verbose = F
  )

prediccion <- predict(modeloXgb, ttesting , type = "class")
MCXgb <- confusion.matrix(ttesting, prediccion)
# MCXgb


indices_xgboost <- indices.general(MCXgb)
indices_xgboost

#Importancia de las Variables
#Importancia de las Variables
variables.importantes <-
  xgb.importance(feature_names = colnames(c("Pclass", "Sex", "Age", "Fare")), model = modeloXgb)
xgb.plot.importance(importance_matrix = variables.importantes)

resultados_bosques <- indices.general(MCRandomF)
resultados_potenciacion <- indices.general(MCPotencia)
resultados_xgboost <- indices.general(MCXgb)

tabla.comparacion <- data.frame()
# datos <- subset(datos, select = -imagen)
# datos


bosquesTita <-
  c((resultados_bosques[c(-1,-4)]), (unlist(resultados_bosques[4])))
potenciacionTita <-
  c((resultados_potenciacion[c(-1,-4)]), (unlist(resultados_potenciacion[4])))
xgboostTita <-
  c((resultados_xgboost[c(-1,-4)]), (unlist(resultados_xgboost[4])))

tabla.comparacion <-
  rbind(bosquesTita, potenciacionTita, xgboostTita)
tabla.comparacion

# round(0.8708134        ,2)
print(
  "Tal como se puede observar, el valor que posee una mayor precision es el de bosques,mientras que el peor es el xgboost, igual que el caso anterior con los datos de tumor. Por otro lado,en relacion  a las demas tareas se podia observar que los valores mas cercanos eran de 0.82 en el caso de r part,que en relacion a los comparados anteriormente a excepcion del xgboost es menos preciso "
)

print(
  " No obstante,la eleccion del modelo puede variar segun los datos que se busquen evaluar,ya que,si bien el modelo bosques tiene una mayor precision global en la precision de sobrevivientes(categoria 1) es mayor la presicion del modelo de potenciacion,razon que podria marcar la eleccion entre un modelo y otro"
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

# datos

tam <- dim(datos)
n   <- tam[1]
# 3.2
muestra <- createDataPartition(datos$Numero, p = 0.8, list = FALSE)
# indice
ttesting     <- datos[-muestra,]
# ttesting
taprendizaje <- datos[muestra,]


modeloRF <-
  train.randomForest(formula = Numero ~ .,
                     data = taprendizaje,
                     importance = T)
varImpPlot(modeloRF)
prediccion <- predict(modeloRF, ttesting, type = "class")
mcRF <- confusion.matrix(ttesting, prediccion)
indices.general(mcRF)


modelo <- train.xgboost(
  formula = Numero ~ .,
  data = taprendizaje,
  nrounds = 79,
  verbose = F
)
prediccion <- predict(modelo, ttesting , type = "class")
mcxgboost <- confusion.matrix(ttesting, prediccion)
indices.general(mcxgboost)

round(0.02696872*100,2)
print(
  "
Los resultados obtenidos para ambos modelos, son muy buenos en general,a razon de que ambos modelos logran una alta precisión global, con valores superiores al 96%, lo que indica que son capaces de predecir de manera correcta el 96 porciento de los datos aproximadamente.Aunque de igual manera es importante destacar qe poseen un error global de mas del 3%,2.7% para el caso de RF y 3.34% para  XGBoost respectivamente. Estos porcentajes de error segun lo que se busque predecir pueden no ser tan buenos."
)

print(
  "A su vez, la precisión por categoría también es alta en la mayoría de los casos, con valores de entre el 91% y el 99%.Esto nos indica que ambos modelos son capaces de predecir de manera correcta las distintas categorias que se estan trabajando"
)

# 3.3
# Resultados pasados
# $`Precisión Global`
# [1] 0.7394822
#
# $`Error Global`
# [1] 0.2605178
#
# $`Precisión por categoría`
# cero     cinco    cuatro       dos     nueve      ocho      seis     siete      tres       uno
# 0.8677419 0.5314685 0.7000000 0.5459459 0.7926829 0.7659574 0.6265060 0.8291139 0.5914634 0.9328063

print(
  "Los resultados en relacion a tareas pasadas se pueden considerar como unos mucho mas precisos,al menos en comparacion a la tarea8,donde se trabajo con el modelo r part,logrando una recisión global del 73.95%,mientras que este ahora es superior al 96% con ambos modelos. Esto indica que los modelos Random Forest y XGBoost son mucho mas precisos y sobre todo,efectivos al compararlos con r part"
)
