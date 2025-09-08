library(readr)

DF <- read_csv("~/Library/Containers/net.whatsapp.WhatsApp/Data/Desktop/Estadistica y Probabilidades/Simulacros/L1C Data.csv")
nrow(DF)

#columnas
ncol(DF)

#datos faltantes
sum(is.na(DF))

#observaciones incompletas por datos faltantes
sum(!complete.cases(DF))

### Parte 2

#si eliminamos todas las observaciones incompletas
nrow(DF) - (sum(!complete.cases(DF)))

#perderiamos
(sum(!complete.cases(DF)))/nrow(DF)

#menor tamaño efectivo de la muestra
min(colSums(!is.na(DF)))
which.min(colSums(!is.na(DF)))






sum(is.na(DF$L))

DF$L <- as.numeric(chartr("Ol", "01", DF$L))

summary(DF)
sum(is.na(DF$L))

390-64

median(DF$L, na.rm= TRUE)

media <- mean(DF$L, na.rm=TRUE)
round(media, 2)
estandar <- round(sd(DF$L, na.rm=TRUE), 2)
estandar

#coeficiente de variacion
cv <- round((estandar / media), 2)
cv

moda <- function(x) {
  uniqx <- unique(x)              # valores únicos
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


#moda de la variable DF$L
mlv(DF$L, method = "mfv", na.rm=TRUE)

#la variable DF$L esta sesgada hacia
if (media > median(DF$L, na.rm=TRUE)) {
  "la derecha"
} else if (media < median(DF$L, na.rm=TRUE)) {
  "la izquierda"
} else {
  "simétrica"
}


#Cambia de categorica ordinal a entera de la variable DF$Calidad
DF$Calidad <- as.integer(factor(DF$Calidad, levels = c("Básica", "Estándar", "Buena", "Excelente", "Premium"), ordered = TRUE))



#primer cuartil de la variable DF$Calidad
quantile(DF$Calidad, probs = 0.25, na.rm=TRUE)

median(DF$Calidad)

#tercer cuartil 
quantile(DF$Calidad, probs = 0.75, na.rm=TRUE)

#---
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

#rango intercuartil
IQR <- IQR(DF$Peso, na.rm=TRUE)
IQR
#promedio de Peso
mean(DF$Peso, na.rm=TRUE)
#moda del peso
mlv(DF$Peso, method = "mfv", na.rm=TRUE)
#esta sesgada hacia donde
max(DF$Peso)




















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

#madera mas barata
which.min(tapply(DF$Precio, DF$Calidad, mean))

#hay una dependencia entre la calidad y el precio
cor(DF$Calidad, DF$Precio, use = "complete.obs")

#el precio, para cada nivel de calidad, esta sesgado a la derecha


#la cuarta parte de los precios de las maderas basicas es mayor que el precio de las maderas de calidad excelente
quantile(DF$Precio[DF$Calidad == 1], probs = 0.75, na.rm=TRUE) > quantile(DF$Precio[DF$Calidad == 4], probs = 0.25, na.rm=TRUE)

#la madera mas cara es de calidad Buena
which.max(DF$Precio[DF$Calidad == 3])

#mientras mejor es la calidad, mayor es el precio en general?
tapply(DF$Precio, DF$Calidad, mean)

#promedio global
mean(DF$Precio, na.rm=TRUE)

#hay mas maderas de calidad Estándar que de cualquiera de otros tipos?
table(DF$Calidad)
