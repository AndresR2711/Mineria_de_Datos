install.packages("ggplot")
library(e1071)
library(ggplot2)
# ========================
#         Ejercicio 3
# ========================

# Ejercicio 3.1
getwd()
setwd("/Users/AndresR/Documents/Tarea7")

data <-
  read.csv(
    'ZipData_2020.csv',
    header = TRUE,
    sep = ';',
    dec = '.',
  )

data

# Ejercicio 3.2


equilibrio.variable.predecir <- function(datos, variable.predecir, ylab = "Cantidad de individuos", 
                                         xlab = "", main = paste("Distribución de la variable",variable.predecir), col = NA) {
  gg_color <- function (n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  if(missing(variable.predecir) | !(variable.predecir %in% colnames(datos))){
    stop("variable.predecir tiene que ser ingresada y ser un nombre de columna", call. = FALSE )
  }
  if(is.character(datos[,variable.predecir]) | is.factor(datos[,variable.predecir])){
    if(length(col) == 0 || is.na(col)){
      col <- gg_color(length(unique(datos[,variable.predecir])))
    }else{
      col <- rep(col,length(unique(datos[,variable.predecir])))
    }
    ggplot(data = datos, mapping = aes_string(x = variable.predecir, fill = variable.predecir)) +
      geom_bar() +
      scale_fill_manual(values = col, name = variable.predecir) +
      labs(x = xlab, y = ylab, title = main) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }else{
    stop("La variable a predecir tienen que ser de V2 factor o character", call. = FALSE )
  }
}

# Índices para matrices NxN
indices.general <- function(MC) {
  
  precision.global <- sum(diag(MC))/sum(MC)
  error.global <- 1 - precision.global
  precision.categoria <- diag(MC)/rowSums(MC)
  res <- list(matriz.confusion = MC, precision.global = precision.global, error.global = error.global, 
              precision.categoria = precision.categoria)
  names(res) <- c("Matriz de Confusión", "Precisión Global", "Error Global", 
                  "Precisión por categoría")
  return(res)
}



equilibrio.variable.predecir(data,"Numero")

# Fijar semilla aleatoria para reproducibilidad
set.seed(123)

# Dividir en conjuntos de entrenamiento y validación
data$Numero<-as.factor(data$Numero)
muestra <-  sample(nrow(data), 0.8 * nrow(data))
taprendizaje   <- data[muestra, ]
ttesting  <- data[-muestra, ]

# Genera el modelo kernel = "linear"
modelo <- svm(Numero~., data = taprendizaje, kernel = "radial")
# modelo
# Prediccion y Matriz de Confusion
prediccion <- predict(modelo,ttesting)

MC<-table(ttesting$Numero,prediccion)

# Índices de Calidad de la predicción
indices.general(MC)

# 0.9317204*100
# round(93.17204,2)
# 0.9920319*100
# round(99.20319,2)

print("Los resultados del modelo pueden ser observados en la salida de índices.general, donde se encuentran valores como el de la precisión global y la matriz de confusión. La precisión del modelo es de 93.17%, la cual es relativamente alta, lo que deriva en menos errores o confusiones en las predicciones de los datos. De igual manera que puede observar que en variables como la del 1 esta es del 99.2%, lo que demuestra un alto valor de predictibilidad. La matriz de confusión muestra el número de predicciones correctas e incorrectas para cada categoría.
")

print("Se puede llegar a la conclusión de que en general los datos obtenidos son buenos, a razón de que la precisión general del modelo es alta y la gran parte de las precisiones de cada categoría son cercanas o superiores al 90 %, algunas incluso, siendo cercanas a 100, como lo es el caso del 1,2 y 0, con valores de 0.9920319, 0.9791667 y 0.9726962 respectivamente. Quizá se posible el realizar mas pruebas con diferentes valores y evaluar diferentes modelos para observar de manera más completa los resultados del modelo.")
# Ejercicio 3.2

print("
En comparación a tareas anteriores el modelo y forma de emplearlo es básicamente la misma, lo que si cambio en gran medida fueron los valores de predicción, ya que, al observar por ejemplo los datos del titanic la precisión del modelo era de 0.837,mientras que en este caso esta es mayor, teniendo un 93.17 de precisión, lo que genera que los datos sean aún más precisos en comparación a los de la tarea anterior
")


