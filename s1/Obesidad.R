setwd("~/Desktop/Estadistica y Probabilidades")

df <- read.csv("Obesidad.csv")
#visualizar el dataframe en otra ventana
View(df)

#identificando valores faltantes de manera ordernada:
#el valor de 2 hace referencia a la columna
apply(is.na(df), 2, sum)

#nombre de las columnas
colnames(df)

#renombrar columna
colnames(df)[12] <- "history"

#renombrar columna por su nombre
#colnames(df)[colnames(df) == "history"] <- "historia"

