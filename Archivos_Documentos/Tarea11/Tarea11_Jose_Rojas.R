library(doSNOW)
library(parallel)
library(MASS)
numCores <- detectCores()
numCores
cl <- makeCluster(24, type="SOCK") # number of cores
registerDoSNOW(cl) #Parallel Computing

library(traineR)
setwd("E:/Users/AndresR/Documents/Tarea11")
library(knitr)
library(ggplot2)
library(kknn)
library(rmarkdown)
library(caret)
library(rpart.plot)
library(dplyr)
library(tidyverse)
library(scales)
library(class)
library(e1071)
library(randomForest)


# install.packages("caret",dependencies=TRUE)

# getwd()
#Para  formatear el doc es Ctrl + Shift + A
# ========================
#         Ejercicio 1
# ========================
# 1.1
# Cargar la tabla de datos


datos <-
  read.csv(
    'Tumores.csv',
    header = TRUE,
    sep = ',',
    dec = '.',
  )

datos <- subset(datos, select = -imagen)
na.omit(datos)

str(datos)
summary(datos)
dim(datos)

enteros <- sapply(datos, is.integer)
datos[enteros] <- lapply(datos[enteros], as.factor)

datos$tipo <- factor(datos$tipo)

muestra <- sample(1:nrow(datos), 0.75 * nrow(datos))
ttesting <- datos[-muestra, ]
taprendizaje <- datos[muestra, ]

# 1.2

v.error.tt <- rep(0, 5)

for (i in 1:5) {
  muestra <- sample(1:nrow(datos), 0.75 * nrow(datos))
  ttesting <- datos[-muestra, ]
  taprendizaje <- datos[muestra, ]
  
  modelo <- train.knn(tipo ~ ., data = taprendizaje, kmax = 50)
  
  prediccion <- predict(modelo, ttesting, type = "class")
  
  MC <- confusion.matrix(ttesting, prediccion)
  
  acierto <- sum(diag(MC)) / sum(MC)
  error <- 1 - acierto
  v.error.tt[i] <- error
}

v.error.tt
plot(v.error.tt, col = "red", type = "b", main = "Variación del Error", xlab = "Número de iteración", ylab = "Error de predicción")



v.error.tc<-rep(0, 5)

for(i in 1:5) {
  modelo <- train.knn(tipo~.,data=datos,kmax=50)
  prediccion <- predict(modelo,datos,type = "class")
  MC <- confusion.matrix(datos, prediccion)  
  # Porcentaje de buena clasificacion y de error
  acierto<-sum(diag(MC))/sum(MC)
  error <- 1- acierto
  v.error.tc[i] <- error
}  

plot(v.error.tt,col="red",type="b",ylim=c(min(v.error.tt,v.error.tc),max(v.error.tt,v.error.tc)+0.05),main="Variación del Error",xlab="Número de iteración",ylab="Estimación del Error")
points(v.error.tc,col="blue",type="b")
legend("topright",legend=c("Tabla de Testing","Tabla completa"),col=c("red","blue"),lty=1,lwd=1)



# 1.3


n <- dim(datos)[1] # Aquí n=150
n
v.error.kg<-rep(0,5)

for(i in 1:5) {
  errori <- 0

  grupos <- createFolds(1:n,10) # grupos$Fold0i es el i-ésimo grupo  
  # Este ciclo es el que hace "cross-validation" (validación cruzada) con 5 grupos (Folds)
  for(k in 1:5) {    
    muestra <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
    
    ttesting <- datos[-muestra, ]
    taprendizaje <- datos[muestra, ]
    modelo <- train.knn(tipo~.,data=taprendizaje,kmax=50)
    prediccion <- predict(modelo,ttesting,type = "class")
    MC <- confusion.matrix(ttesting, prediccion)  
    # Porcentaje de buena clasificación y de error
    acierto<-sum(diag(MC))/sum(MC)
    error <- 1 - acierto
    errori <- errori + error
  } 
  v.error.kg[i] <- errori/5
}
plot(v.error.kg, col = "red", type = "b", ylim = c(min(v.error.kg,v.error.tt), max(v.error.kg, 
                                                                                                               v.error.tt) + 0.05), main = "Variación del Error", xlab = "Número de iteración", 
     ylab = "Estimación del Error")
points(v.error.tt, col = "blue", type = "b")
legend("topright", legend = c("K-ésimo grupo","Tabla Testing(Error Anterior)"), col = c("red",  "blue"), lty = 1, lwd = 1)

plot <- include_graphics("Rplot.PNG")
plot
# 1.4
print("Tal como se puede observar en la figura anterior el metodo de K-ésimo grupo se mantiene similar al anterior,sin embargo, en varias de las iteraciones se puede observar que este posee un valor de estimacion del error menor. Esto nos indica que el modelo utilizado tiene un mejor rendimiento en la tarea de predicción en comparacion al anterior")

# ========================
#         Ejercicio 2
# ========================
# 2.1
n <- dim(datos)[1]
# n
deteccion.no.discrete <- c()
deteccion.no.real <- c()
deteccion.no.gentle <- c()

# datos$tipo<- as.factor(datos$tipo)
for(i in 1:5){
  grupos <- createFolds(1:n,10) 
  no.discrete <- 0
  no.real <- 0
  no.gentle <- 0
  # Este ciclo es el que hace 'cross-validation' (validación cruzada) con 10
  # grupos (Folds)
  for(k in 1:10) {

    muestra <- grupos[[k]]  # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[-muestra, ]
    taprendizaje <- datos[muestra, ]
    
    modelo<-train.ada(tipo~.,data=taprendizaje,iter=80,nu=1,type="discrete")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.discrete <- no.discrete + MC[1,1]
    
    modelo<-train.ada(tipo~.,data=taprendizaje,iter=80,nu=1,type="real")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.real <- no.real + MC[1,1]
   
    na.omit(no.gentle)
    modelo<-train.ada(tipo~.,data=taprendizaje,iter=80,nu=1,type="gentle")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.gentle <- no.gentle +MC[1,1]
  


  }
  
  deteccion.no.discrete[i] <- no.discrete
  deteccion.no.real[i] <- no.real
  deteccion.no.gentle[i] <- no.gentle
  
}

resultados <- data.frame("discrete"     = deteccion.no.discrete,
                         "real"     = deteccion.no.real,
                         "gentle" = deteccion.no.gentle) # Preparamos los datos

par(oma=c(0, 0, 0, 5)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Detección de 1´s con ADA", 
        xlab = "Número de iteración",
        ylab = "Cantidad de 1´s  detectados",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda

plot <- include_graphics("1sADA.PNG")
plot
print("Sí se puede determinar cual algoritmo es mejor,por ejemplo,en este caso se pueded observar que discrete en la mayoría de las iteraciones posee una mayor cantidad de 1´s detectados,incluso sin variar tanto entre una ejecucion y otra como sucede con los otros dos")

# # 2.2


deteccion.error.discrete <- c()
deteccion.error.real <- c()
deteccion.error.gentle <- c()

# Validación cruzada 5 veces
for (i in 1:5) {
  grupos <- createFolds(1:n, 10) # Crea los 10 grupos
  error.discrete <- 0
  error.real <- 0
  error.gentle <- 0
  
  
  # Este ciclo es el que hace validación cruzada con 10 grupos
  for(k in 1:10) {
    muestra <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
    ttesting <- datos[-muestra, ]
    taprendizaje <- datos[muestra, ]
    
    modelo<-train.ada(tipo~.,data=taprendizaje,iter=80,nu=1,type="discrete")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.discrete<-error.discrete+(1-(sum(diag(MC)))/sum(MC))*100
    
    na.omit(error.real)
    modelo<-train.ada(tipo~.,data=taprendizaje,iter=80,nu=1,type="real")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.real<- error.real+(1-(sum(diag(MC)))/sum(MC))*100
    
    na.omit(error.gentle)
    modelo<-train.ada(tipo~.,data=taprendizaje,iter=80,nu=1,type="gentle")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.gentle <- error.gentle+(1-(sum(diag(MC)))/sum(MC))*100
    

  }

  deteccion.error.discrete[i] <- error.discrete/10
  deteccion.error.real[i] <- error.real/10
  deteccion.error.gentle[i] <- error.gentle/10
}

resultados <- data.frame("discrete" = deteccion.error.discrete,
                         "real" = deteccion.error.real,
                         "gentle" = deteccion.error.gentle
               )

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda
matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Comparación del Error Global", 
        xlab = "Número de iteración",
        ylab = "Porcentaje de Error Global",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA, cex = 0.8,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda

plot <- include_graphics("errorglobal.PNG")
plot
print("tal como se puede oservar el metodo real es el que mantiene un error globar mas bajo,por tal,dependiendo que datos se desean estudiar y en caso de que la precision sea prioridad este metodo podria ser el recomendado,no obstante,como se puede ver la diferencia no es tan grande,por tal se deberá analizar otros factores que puedan influir en la decision")
# 2.3


n <- dim(datos)[1]

matrices_discrete <- list()
matrices_real <- list()
matrices_gentle <- list()

for (i in 1:5) {
  grupos <- createFolds(1:n, 10) 
  
  matrices_discrete_iter <- list()
  matrices_real_iter <- list()
  matrices_gentle_iter <- list()
  
  for (k in 1:10) {
    muestra <- grupos[[k]]
    ttesting <- datos[-muestra, ]
    taprendizaje <- datos[muestra, ]
    
    modelo <- train.ada(tipo ~ ., data = taprendizaje, iter = 80, nu = 1, type = "discrete")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    matrices_discrete_iter[[k]] <- MC
    
    modelo <- train.ada(tipo ~ ., data = taprendizaje, iter = 80, nu = 1, type = "real")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    matrices_real_iter[[k]] <- MC
    
    modelo <- train.ada(tipo ~ ., data = taprendizaje, iter = 80, nu = 1, type = "gentle")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    matrices_gentle_iter[[k]] <- MC
  }
  
  matrices_discrete[[i]] <- matrices_discrete_iter
  matrices_real[[i]] <- matrices_real_iter
  matrices_gentle[[i]] <- matrices_gentle_iter
}



promedio_discrete <- Reduce("+", matrices_discrete_iter) / length(matrices_discrete_iter)
promedio_real <- Reduce("+", matrices_discrete_iter) / length(matrices_discrete_iter)
promedio_gentle <- Reduce("+", matrices_gentle_iter) / length(matrices_gentle_iter)



porcentaje1sdiscrete <- promedio_discrete["1", "1"] / sum(promedio_discrete[, "1"]) * 100
porcentaje1sreal <- promedio_real["1", "1"] / sum(promedio_real[, "1"]) * 100
porcentaje1sgentle <- promedio_gentle["1", "1"] / sum(promedio_gentle[, "1"]) * 100

porcentaje0_discrete <- promedio_discrete["0", "0"] / sum(promedio_discrete[, "0"]) * 100
porcentaje0_real <- promedio_real["0", "0"] / sum(promedio_real[, "0"]) * 100
porcentaje0_gentle <- promedio_gentle["0", "0"] / sum(promedio_gentle[, "0"]) * 100

errordiscrete <- (sum(diag(promedio_discrete)) / sum(promedio_discrete)) * 100
errorreal <- (sum(diag(promedio_real)) / sum(promedio_real)) * 100
errorgentle <- (sum(diag(promedio_gentle)) / sum(promedio_gentle)) * 100


resultados_promedio <- data.frame(
  Metodo = c("discrete", "real", "gentle"),
  Porcentaje_1 = c(porcentaje1sdiscrete, porcentaje1sreal, porcentaje1sgentle),
  Porcentaje_0 = c(porcentaje0_discrete, porcentaje0_real, porcentaje0_gentle),
  Error_Global = c(errordiscrete, errorreal, errorgentle)
)


resultados_promedio_long <- reshape2::melt(resultados_promedio, id.vars = "Metodo")

ggplot(resultados_promedio_long, aes(x = Metodo, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Método", y = "Porcentaje/Error Global") +
  ggtitle("Comparación de los resultados") +
  scale_fill_manual(values = c("#FF9999", "#66CCFF", "#99FF99"))  # Customize fill colors if needed

plot <- include_graphics("resultados.PNG")
plot
print("Tal como se puede observar en este caso en especifico la diferencia entre un metodo y otro es minima,por esta razon no se puede recomendar un metodo en especifico")
# ========================
#         Ejercicio 3
# ========================

grupos <- createFolds(1:n, 10) 
algoritmos <- c("rectangular", "triangular", "epanechnikov", "biweight", "triweight",
                "cos", "inv", "gaussian", "optimal")

calculo1_s <- function(predicciones, observaciones) {
  sum(predicciones == 1 & observaciones == 1)
}
resultados <- matrix(0, nrow = 10, ncol = length(algoritmos))
colnames(resultados) <- algoritmos

for (i in 1:5) {
  muestra  <- grupos[[i]]
  
  ttesting <- datos[-muestra, ]
  taprendizaje <- datos[muestra, ]
  
  
  for (j in 1:length(algoritmos)) {
    modelo <- train.kknn(tipo ~ ., taprendizaje, kmax = 5, kernel = algoritmos[j])
    prediccion  <- predict(modelo, ttesting)
    resultados[i, j] <- calculo1_s(prediccion , ttesting$tipo)
  }
}



plot(1:length(algoritmos), resultados[1, ], col = "magenta", type = "b", ylim = c(min(resultados), max(resultados) + 0.05),
     main = "Variación del Error", xlab = "Algoritmo", ylab = "Estimación del Error", xaxt = "n")
points(1:length(algoritmos), resultados[2, ], col = "blue", type = "b")
points(1:length(algoritmos), resultados[3, ], col = "red", type = "b")
points(1:length(algoritmos), resultados[4, ], col = "green", type = "b")
points(1:length(algoritmos), resultados[5, ], col = "purple", type = "b")
legend("topright", legend = c("Iteración 1", "Iteración 2", "Iteración 3", "Iteración 4", "Iteración 5"), 
       col = c("magenta", "blue", "red", "green", "purple"), lty = 1, lwd = 1)


axis(1, at = 1:length(algoritmos), labels = algoritmos, las = 2, cex.axis = 0.8)

plot <- include_graphics("variaciones.PNG")
plot
print("En este caso en particular no es posible determinar con claridad cual es el mejor algoritmo,a razon de que estos varian segun la iteracion que se observe. Por ejemplo,el rectangular en la primer iteracion es de los que tienen una menor estimacion,mientras que en la segunda este es de los mas altos")

# 3.2

errorGlobal <- function(predicciones, observaciones) {
  sum(predicciones != observaciones) / length(observaciones)
}
resultados <- matrix(0, nrow = 10, ncol = length(algoritmos))
colnames(resultados) <- algoritmos

for (i in 1:5) {
  muestra  <- grupos[[i]]
  
  ttesting <- datos[-muestra, ]
  taprendizaje <- datos[muestra, ]
  
  
  for (j in 1:length(algoritmos)) {
    modelo <- train.kknn(tipo ~ ., taprendizaje, kmax = 5, kernel = algoritmos[j])
    prediccion  <- predict(modelo, ttesting)
    resultados[i, j] <- errorGlobal(prediccion , ttesting$tipo)
  }
}

plot(1:length(algoritmos), resultados[1, ], col = "magenta", type = "b", ylim = c(min(resultados), max(resultados) + 0.05),
     main = "Variación del Error", xlab = "Algoritmo", ylab = "Error Global",xaxt = "n")
points(1:length(algoritmos), resultados[2, ], col = "blue", type = "b")
points(1:length(algoritmos), resultados[3, ], col = "red", type = "b")
points(1:length(algoritmos), resultados[4, ], col = "green", type = "b")
points(1:length(algoritmos), resultados[5, ], col = "purple", type = "b")
legend("topright", legend = c("Iteración 1", "Iteración 2", "Iteración 3", "Iteración 4", "Iteración 5"), 
       col = c("magenta", "blue", "red", "green", "purple"), lty = 1, lwd = 1)

axis(1, at = 1:length(algoritmos), labels = algoritmos, las = 2, cex.axis = 0.8)

plot <- include_graphics("rectangular.PNG")
plot
print("En este caso se puede observar que en la gran parte de iteraciones el modelo algoritmo rectangular posee valores de error global menores en comparacion a los demás,estando en la mayoría como uno de los de menor error. Claramente no se podría afirmar que este sea el mejor pero ciertamente es de los mejores al menos para este caso en concreto")

# 3.3

datos$tipo<- as.factor(datos$tipo)


grupos <- createFolds(1:n, 10) 
algoritmos <- c("rectangular", "triangular", "epanechnikov", "biweight", "triweight",
                "cos", "inv", "gaussian", "optimal")

resultados <- list()
for (i in 1:length(algoritmos)) {
  resultados[[i]] <- matrix(0, nrow = 2, ncol = 2)
}
#Sino lo tiro asi falla
calcular_matriz_confusion <- function(predicciones, observaciones) {
  MC <- confusionMatrix(predicciones, observaciones)
  return(MC$table)
}

for (i in 1:5) {
  muestra  <- grupos[[i]]
  
  ttesting <- datos[-muestra, ]
  taprendizaje <- datos[muestra, ]
  
  for (j in 1:length(algoritmos)) {
    modelo <- train.kknn(tipo ~ ., taprendizaje, kmax = 5, kernel = algoritmos[j])
    prediccion  <- predict(modelo, ttesting)
    MC <- calcular_matriz_confusion(prediccion , ttesting$tipo)
    resultados[[j]] <- resultados[[j]] + MC
  }
}

porcentaje_1s <- rep(0, length(algoritmos))
porcentaje_0s <- rep(0, length(algoritmos))
error_global <- rep(0, length(algoritmos))

for (i in 1:length(algoritmos)) {
  matriz <- resultados[[i]] / 10
  TP <- matriz[1, 1]
  FN <- matriz[2, 1]
  TN <- matriz[2, 2]
  FP <- matriz[1, 2]
  
  porcentaje_1s[i] <- TP / (TP + FN)
  porcentaje_0s[i] <- TN / (TN + FP)
  error_global[i] <- (FP + FN) / (TP + FN + TN + FP)
}

barplot(porcentaje_1s, names.arg = algoritmos, col = "blue", main = "Porcentaje de 1's ",
        xlab = "Algoritmo", ylab = "Porcentaje de 1's")

barplot(porcentaje_0s, names.arg = algoritmos, col = "black", main = "Porcentaje de 0's",
        xlab = "Algoritmo", ylab = "Porcentaje de 0's", ylim = c(0, max(porcentaje_0s) + 0.1))


barplot(error_global, names.arg = algoritmos, col = "#FF9999", main = "Error global",
        xlab = "Algoritmo", ylab = "Error global")
# grafico1
plot <- include_graphics("grafico1.PNG")
plot

print("Tal como se puede observar en el grafico anterior el algoritmo con mayor porcentaje de 1´s promedio es el triweight")
plot <- include_graphics("grafico2.PNG")
plot
print("Con respecto a la medicion de mayor porcentaje de 0´s promedio no se puede llegar a un modelo en especifico a razon de que todos se encuentran al mismo nivel,por lo que no hay variacion entre 1 y otro")
plot <- include_graphics("errorglobal.PNG")
plot

print("A diferencia del resultado de 0´s promedio en este caso podemos observar que el algoritmo epanechnikov es relativamente menor a los demas. Por lo que en este caso en especifico es el algoritmo recomendado")

# 3.4
print("En caso de que el porcentaje de 1´s sera relevante eligiría el metodo triweight,a razón de que en ese caso especifico fue el algoritmo con mayor cantidad de 1´s promedio. No obstante,si los 3 valores calculados son relevantes eligiría el metodo epanechnikov,a razón de que a pesar de que el valor promedio de 1´s no es tan alto como el triweight,se asemeja bastante y posee un menor error global")
# ========================
#         Ejercicio 4
# ========================
# 



dim(datos)
summary(datos)

numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- 5
cantidad.grupos <- 10

deteccion.no.svm <- c()
deteccion.no.knn <- c()
deteccion.no.arbol <- c()
deteccion.no.bosque <- c()
deteccion.no.potenciacion <- c()
deteccion.no.xgboost <- c()
deteccion.no.red.neu <- c()


# Validación cruzada 5 veces
for (i in 1:cantidad.validacion.cruzada) {
  grupos <- createFolds(1:numero.filas, cantidad.grupos) # Crea los 10 grupos
  no.svm <- 0
  no.knn <- 0
  no.arbol <- 0
  no.bosque <- 0
  no.potenciacion <- 0
  no.xg  <- 0
  no.red.neu <- 0

  
  # Este ciclo es el que hace validación cruzada con 10 grupos
  for (k in 1:cantidad.grupos) {
    
    muestra <- grupos[[k]]
    ttesting <- datos[-muestra, ]
    ttraining <- datos[muestra, ]
    
    modelo <- train.svm(tipo ~ ., data = ttraining, kernel = "linear", probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.svm <- no.svm + MC["1", "1"] # Detección de los No Pagadores
    
    modelo <- train.knn(tipo ~ ., data = ttraining, kmax = 37)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.knn <- no.knn + MC["1", "1"] # Detección de los No Pagadores
    
    modelo = train.rpart(tipo ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.arbol <- no.arbol + MC["1", "1"] # Detección de los No Pagadores
    
    modelo <- train.randomForest(tipo ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.bosque <- no.bosque + MC["1", "1"] # Detección de los No Pagadores
    
    modelo <- train.ada(tipo ~ ., data = ttraining, iter = 20, nu = 1, type = "discrete")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.potenciacion <- no.potenciacion + MC["1", "1"] # Detección de los No Pagadores
    
    modelo <- train.xgboost(tipo ~ ., data = ttraining, nrounds = 79,
                            print_every_n = 10, maximize = F , eval_metric = "error")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.xg <- no.xg + MC["1", "1"] # Detección de los No Pagadores
    
    modelo <- train.neuralnet(tipo ~., data = ttraining, hidden = c(8,6,4), 
                              linear.output = FALSE, threshold = 0.5, stepmax = 1e+06)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    no.red.neu <- no.red.neu + MC["1", "1"] # Detección de los No Pagadores
  }

  deteccion.no.svm[i] <- no.svm
  deteccion.no.knn[i] <- no.knn
  deteccion.no.arbol[i] <- no.arbol
  deteccion.no.bosque[i] <- no.bosque
  deteccion.no.potenciacion[i] <- no.potenciacion
  deteccion.no.xgboost[i] <- no.xg
  deteccion.no.red.neu[i] <- no.red.neu


}

resultados <- data.frame("svm" = deteccion.no.svm,
                         "k_vecinos" = deteccion.no.knn,
                         "arboles" = deteccion.no.arbol,
                         "bosques" = deteccion.no.bosque,
                         "potenciacion" = deteccion.no.potenciacion,
                         "xgboost" = deteccion.no.xgboost,
                         "redes_neuralnet" = no.red.neu)

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda

matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Detección de tumor", 
        xlab = "Número de iteración",
        ylab = "Cantidad de personas con tumor",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda



# ejercicio 4.2

numero.filas <- nrow(datos)
cantidad.validacion.cruzada <- 5
cantidad.grupos <- 10

deteccion.error.svm <- c()
deteccion.error.knn <- c()
deteccion.error.arbol <- c()
deteccion.error.bosque <- c()
deteccion.error.potenciacion <- c()
deteccion.error.xgboost <- c()
deteccion.error.red.neu <- c()


# Validación cruzada 5 veces
for (i in 1:cantidad.validacion.cruzada) {
  grupos <- createFolds(1:numero.filas, cantidad.grupos) # Crea los 10 grupos
  error.svm <- 0
  error.knn <- 0
  error.arbol <- 0
  error.bosque <- 0
  error.potenciacion <- 0
  error.xg  <- 0
  error.red.neu <- 0

  
  # Este ciclo es el que hace validación cruzada con 10 grupos
  for (k in 1:cantidad.grupos) {
    
    muestra <- grupos[[k]]
    ttesting <- datos[-muestra, ]
    ttraining <- datos[muestra, ]
    
    modelo <- train.svm(tipo ~ ., data = ttraining, kernel = "linear", probability = FALSE)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.svm <- error.svm+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.knn(tipo ~ ., data = ttraining, kmax = 37)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.knn <- error.knn+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo = train.rpart(tipo ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.arbol <- error.arbol+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.randomForest(tipo ~ ., data = ttraining)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.bosque <- error.bosque+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.ada(tipo ~ ., data = ttraining, iter = 20, nu = 1, type = "discrete")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.potenciacion <- error.potenciacion+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.xgboost(tipo ~ ., data = ttraining, nrounds = 79,
                            print_every_n = 10, maximize = F , eval_metric = "error")
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.xg <- error.xg+(1-(sum(diag(MC)))/sum(MC))*100
    
    modelo <- train.neuralnet(tipo ~., data = ttraining, hidden = c(8,6,4), 
                              linear.output = FALSE, threshold = 0.5, stepmax = 1e+06)
    prediccion <- predict(modelo, ttesting)
    MC <- confusion.matrix(ttesting, prediccion)
    # Cálculo del ERROR
    error.red.neu <- error.red.neu+(1-(sum(diag(MC)))/sum(MC))*100
  }
  deteccion.error.svm[i] <- error.svm/cantidad.grupos
  deteccion.error.knn[i] <- error.knn/cantidad.grupos
  deteccion.error.arbol[i] <- error.arbol/cantidad.grupos
  deteccion.error.bosque[i] <- error.bosque/cantidad.grupos
  deteccion.error.potenciacion[i] <- error.potenciacion/cantidad.grupos
  deteccion.error.xgboost[i] <- error.xg/cantidad.grupos
  deteccion.error.red.neu[i] <- error.red.neu/cantidad.grupos
}

resultados <- data.frame("svm" = deteccion.error.svm,
                         "k_vecinos" = deteccion.error.knn,
                         "arboles" = deteccion.error.arbol,
                         "bosques" = deteccion.error.bosque,
                         "potenciacion" = deteccion.error.potenciacion,
                         "xgboost" = deteccion.error.xgboost,
                         "redes_neuralnet" = deteccion.error.red.neu)

par(oma=c(0, 0, 0, 8)) # Hace espacio para la leyenda
matplot(resultados, type="b", lty = 1, lwd = 1, pch = 1:ncol(resultados),
        main = "Comparación del Error Global", 
        xlab = "Número de iteración",
        ylab = "Porcentaje de Error Global",
        col = rainbow(ncol(resultados)))
legend(par('usr')[2], par('usr')[4], legend = colnames(resultados),bty='n', xpd=NA, cex = 0.8,
       pch=1:ncol(resultados), col = rainbow(ncol(resultados))) # La leyenda




