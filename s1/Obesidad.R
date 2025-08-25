setwd("~/Desktop/Estadistica y Probabilidades/s1")
install.packages("DescTools")
library(DescTools)

df <- read.csv("Obesidad.csv")
#visualizar el data frame en otra ventana
View(df)

# media, mediana y desviacion estandar de la variable Age
media <- mean(df$Age)
mediana <- median(df$Age)
desviacion_estandar <- sd(df$Age)
# mostrar los resultados
media
mediana
desviacion_estandar

#Varianza de la variable Weight
varianza <- var(df$Weight)
rango_weight <- range(df$Weight)
rango_weight

#Distribucion de frecuencias del nivel de obesidad (NObeyesdad)
tabla_frecuencias <- table(df$NObeyesdad)
tabla_frecuencias

#Porcenajes de personas que son mujer y hombre Gender
tabla_genero <- table(df$Gender)
tabla_genero
porcentaje_genero <- prop.table(tabla_genero) * 100 # calcula proporciones (decimal), por eso se multiplica por 100.
porcentaje_genero

#Relacion entre SMOKE y NObeyesdad, fumar no afecta la obesidad
tabla_smoke_obesidad <- table(df$SMOKE, df$NObeyesdad)
tabla_smoke_obesidad
View


#transporte mas usado es el Public_transportation
transporte_mas_usado <- table(df$Gender, df$MTRANS)

View(transporte_mas_usado)

