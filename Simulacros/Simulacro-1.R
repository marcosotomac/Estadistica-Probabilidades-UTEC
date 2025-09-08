DF <- read.csv("Desktop/Estadistica y Probabilidades/Simulacros/L1C Data.csv")

#libreria que calcula la moda
library(modeest)

#cantidad de observaciones del DF
nrow(DF)

#cantidad de variables del DF
ncol(DF)

#cantidad de datos faltantes
sum(is.na(DF))

#cantidad de filas incompletas
sum(!complete.cases(DF))

#si eliminamos todas las observaciones incompletas
DF2 <- DF[complete.cases(DF), ]
#nuestro tamaño de muestra seria
nrow(DF2)

#perderiamos cierto % de las observaciones completas que tenemos
(nrow(DF) - nrow(DF2)) / nrow(DF)

#el menor tamaño efectivo de la muestra es y corresponde a la variable
min(colSums(!is.na(DF)))
which.min(colSums(!is.na(DF)))

#Considere la variable L, los dos errores mas frecuentes son la lectura incorrecta de un cero como la letra "O" mayuscula y la lectura incorrecta de 1 como la letra "L" minuscula. Utiliza chartr para arreglar los errores mencionados. Cualquier error debe resultar en un dato faltante (NA)

DF$L <- as.numeric(chartr("O", "0", chartr("l", "1", DF$L)))
#Verifique que no hayan quedado errores
sum(is.na(DF$L))

#tamaño efectivo de la muestra
sum(!is.na(DF$L))

#mediana de la variable L
median(DF$L, na.rm= TRUE)


#promedio de la variable L
promedio <- mean(DF$L, na.rm= TRUE)

#desviacion estandar de la variable L
desviacion_estandar <- sd(DF$L, na.rm = TRUE)

#coeficiente de variacion
coeficiente_variacion <- desviacion_estandar / promedio * 100
coeficiente_variacion

#moda de la variable L
moda <- mlv(DF$L, method = "lientz", bw = 1, na.rm =FALSE)
moda


#ordenar las categorias de la variable Calidad (Basica, estandar, buena, excelente, premium)
DF$Calidad <- factor(DF$Calidad, levels = c("Básica", "Estándar", "Buena", "Excelente", "Premium"), ordered = TRUE)

calidad_num <- as.numeric(DF$Calidad)
#el primer cuartil de la variable Calidad es
quantile(calidad_num, 0.25, na.rm = TRUE)

#mediana
median(calidad_num)

#tercer cuartil
quantile(calidad_num, 0.75, na.rm= TRUE)


#rango de la variable
rango <- range(calidad_num, na.rm = TRUE)
rango



nf <- layout(mat = matrix(c(1,2) , 2 , 1, byrow = TRUE),  height = c(2,1))
par(mar=c(0.0, 4.1, 2.1, 1.1))
hist(DF$Peso
     , xlim = c(0, max(DF$Peso))
     , ylab = "densidad"
     , main = "Histograma de peso"
     , prob = TRUE
     , axes = FALSE
     , breaks = 20
     , col = "lightblue"
)
axis(2)
legend("topright", legend = "Promedio", pch = 4, bty = "n", pt.cex = 1.5)
par(mar=c(4.1, 4.1, 0.6, 1.1))
boxplot(DF$Peso
        , horizontal = TRUE
        , bty = "n"
        , ylim = c(0, max(DF$Peso))
        , frame = FALSE
        , axes = TRUE
        , col = "lightblue"
)
points(mean(DF$Peso, na.rm = TRUE), 1, pch = 4, cex = 1.5)
title(xlab = "peso (kg)", line = 2)













boxplot(DF$Precio ~ DF$Calidad
        , varwidth = TRUE
        , main = "Precio en términos de calidad"
        , ylab = "precio (S/.)"
        , xlab = "calidad"
        , col = "lightgreen"
)
abline(h = mean(DF$Precio), lty = 2, col = "red")
points(1:5
       , c(mean(DF$Precio[DF$Calidad == "Básica"])
           , mean(DF$Precio[DF$Calidad == "Estándar"])
           , mean(DF$Precio[DF$Calidad == "Buena"])
           , mean(DF$Precio[DF$Calidad == "Excelente"])
           , mean(DF$Precio[DF$Calidad == "Premium"])
       )
       , pch = 4
)
legend("topright"
       , legend = c("Promedio por calidad", "Promedio global")
       , pch = c(4, NA)
       , lty = c(NA, 2)
       , col = c("black", "red")
       , bty = "n"
)
